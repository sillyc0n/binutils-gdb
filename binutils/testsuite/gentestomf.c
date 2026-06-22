/* Copyright (C) 2024-2026 Free Software Foundation, Inc.

   This file is part of GNU Binutils.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA
   02110-1301, USA.  */

/* This file generates OMF (Object Module Format) test objects for the
   ix86-omf BFD backend.  The generated .o files are read by objdump(1)
   in the binutils test suite to verify that the OMF parser correctly
   interprets sections, symbols, and relocations.

   OMF record format (all records):
     [rectype:1][reclen:2 LE][payload:N-1][checksum:1]
   where reclen = N (total record bytes after the 3-byte header),
   and N = len(payload) + 1 (the last byte of the "payload" is the
   checksum byte, which is stripped before dispatch to the handler).

   For the handler, the "reclen" parameter (after -1 for checksum)
   is the number of payload bytes available.  */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define INCORRECT_USAGE 2
#define IO_ERROR 3

/* Write a 16-bit LE value. */
static void
put16le (uint8_t *buf, uint16_t v)
{
  buf[0] = v & 0xff;
  buf[1] = (v >> 8) & 0xff;
}

/* Write a 32-bit LE value. */
static void
put32le (uint8_t *buf, uint32_t v)
{
  buf[0] = v & 0xff;
  buf[1] = (v >> 8) & 0xff;
  buf[2] = (v >> 16) & 0xff;
  buf[3] = (v >> 24) & 0xff;
}

/* ------------------------------------------------------------------ */
/*  OMF record construction helpers                                    */
/* ------------------------------------------------------------------ */

/* Write a complete OMF record into *buf and return the total size.
   rectype : 1-byte record type
   payload : data bytes (checksum is appended automatically)
   plen    : length of payload
   Returns total bytes written (= plen + 4).  */
static int
omf_record (uint8_t *buf, int rectype, const uint8_t *payload, int plen)
{
  int reclen = plen + 1;	/* payload + checksum byte */
  unsigned sum;
  int i;

  buf[0] = rectype;
  put16le (buf + 1, reclen);
  memcpy (buf + 3, payload, plen);
  /* Checksum byte at position 3 + plen (the last byte of the record). */
  sum = 0;
  for (i = 0; i < 3 + plen; i++)
    sum += buf[i];
  buf[3 + plen] = (256 - (sum % 256)) & 0xff;
  return 3 + plen + 1;		/* type + length + payload + checksum */
}

/* Convenience wrapper for variable-length payload. */
#define OMF_RECORD(rt, ...)						\
  omf_record (buf, rt, (const uint8_t[]){ __VA_ARGS__ },		\
	      sizeof ((const uint8_t[]){ __VA_ARGS__ }))

/* Write an OMF index (variable-length, max 2 bytes).  Returns bytes written. */
static int
omf_index (uint8_t *buf, int idx)
{
  if (idx < 0x80)
    {
      buf[0] = idx & 0x7f;
      return 1;
    }
  buf[0] = 0x80 | ((idx >> 8) & 0x7f);
  buf[1] = idx & 0xff;
  return 2;
}

/* ------------------------------------------------------------------ */
/*  Builder context — accumulates records into a buffer                */
/* ------------------------------------------------------------------ */

#define MAX_OBJ_SIZE 4096

struct omf_buf
{
  uint8_t data[MAX_OBJ_SIZE];
  int len;
};

static void
ob_write (struct omf_buf *ob, const uint8_t *rec, int recsize)
{
  if (ob->len + recsize > MAX_OBJ_SIZE)
    {
      fprintf (stderr, "gentestomf: object too large\n");
      exit (IO_ERROR);
    }
  memcpy (ob->data + ob->len, rec, recsize);
  ob->len += recsize;
}

/* ------------------------------------------------------------------ */
/*  Individual record constructors (write directly to ob)              */
/* ------------------------------------------------------------------ */

static void
ob_theadr (struct omf_buf *ob, const char *name)
{
  int slen = strlen (name);
  uint8_t payload[256];
  payload[0] = slen;
  memcpy (payload + 1, name, slen);
  uint8_t rec[512];
  int n = omf_record (rec, 0x80, payload, 1 + slen);
  ob_write (ob, rec, n);
}

static void
ob_lnames (struct omf_buf *ob, const char *name)
{
  int slen = strlen (name);
  uint8_t payload[256];
  payload[0] = slen;
  memcpy (payload + 1, name, slen);
  uint8_t rec[512];
  int n = omf_record (rec, 0x96, payload, 1 + slen);
  ob_write (ob, rec, n);
}

/* Write a SEGDEF (0x98, 16-bit seglen) or SEGDEF386 (0x99, 32-bit seglen).
   seglen is the segment size in bytes.
   name_idx: index into LNAMES table.  */
static void
ob_segdef_ex (struct omf_buf *ob, int is_32bit, uint32_t seglen,
	      int alignment, int combination,
	      int name_idx, int class_idx, int overlay_idx,
	      int big, int use32)
{
  uint8_t payload[32];
  int plen = 0;
  uint8_t attr = (alignment << 5) | (combination << 2) | (big << 1) | use32;
  payload[plen++] = attr;
  if (is_32bit)
    {
      put32le (payload + plen, seglen);
      plen += 4;
    }
  else
    {
      put16le (payload + plen, seglen);
      plen += 2;
    }
  plen += omf_index (payload + plen, name_idx);
  plen += omf_index (payload + plen, class_idx);
  plen += omf_index (payload + plen, overlay_idx);
  uint8_t rec[512];
  int n = omf_record (rec, is_32bit ? 0x99 : 0x98, payload, plen);
  ob_write (ob, rec, n);
}

#define ob_segdef(ob, is32, seglen, align, comb, name, cls, ovl) \
  ob_segdef_ex (ob, is32, seglen, align, comb, name, cls, ovl, 0, 0)

/* Write a GRPDEF (0x9A).  Simple SEGDEF-component version.  */
static void
ob_grpdef (struct omf_buf *ob, int name_idx, const int *seg_indices, int nsegs)
{
  uint8_t payload[256];
  int plen = 0;
  plen += omf_index (payload + plen, name_idx);
  for (int i = 0; i < nsegs; i++)
    {
      payload[plen++] = 0xff;  /* SEGDEF component type */
      plen += omf_index (payload + plen, seg_indices[i]);
    }
  uint8_t rec[512];
  int n = omf_record (rec, 0x9a, payload, plen);
  ob_write (ob, rec, n);
}

/* Write an EXTDEF (0x8C).  name is the external name.  */
static void
ob_extdef (struct omf_buf *ob, const char *name, int type_idx)
{
  int slen = strlen (name);
  uint8_t payload[256];
  int plen = 0;
  payload[plen++] = slen;
  memcpy (payload + plen, name, slen);
  plen += slen;
  plen += omf_index (payload + plen, type_idx);
  uint8_t rec[512];
  int n = omf_record (rec, 0x8c, payload, plen);
  ob_write (ob, rec, n);
}

/* Write a PUBDEF (0x90) or PUBDEF386 (0x91).
   base_group, base_segment: group and segment indices (0 = none).
   Each symbol is (name, offset, type_idx).  */
static void
ob_pubdef (struct omf_buf *ob, int is_32bit,
	   int base_group, int base_segment,
	   const char *name, uint32_t offset, int type_idx)
{
  uint8_t payload[256];
  int plen = 0;
  plen += omf_index (payload + plen, base_group);
  plen += omf_index (payload + plen, base_segment);
  /* Absolute segment case: add base frame (16-bit). */
  if (base_segment == 0)
    {
      put16le (payload + plen, 0);
      plen += 2;
    }
  int slen = strlen (name);
  payload[plen++] = slen;
  memcpy (payload + plen, name, slen);
  plen += slen;
  if (is_32bit)
    {
      put32le (payload + plen, offset);
      plen += 4;
    }
  else
    {
      put16le (payload + plen, offset);
      plen += 2;
    }
  plen += omf_index (payload + plen, type_idx);
  uint8_t rec[512];
  int n = omf_record (rec, is_32bit ? 0x91 : 0x90, payload, plen);
  ob_write (ob, rec, n);
}

/* Write a LEDATA (0xA0) or LEDATA386 (0xA1).
   seg_idx: segment index (variable-length).
   offset: section-relative offset (16 or 32 bit).
   data: raw data bytes.  */
static void
ob_ledata (struct omf_buf *ob, int is_32bit,
	   int seg_idx, uint32_t offset,
	   const uint8_t *data, int dlen)
{
  uint8_t payload[2048];
  int plen = 0;
  plen += omf_index (payload + plen, seg_idx);
  if (is_32bit)
    {
      put32le (payload + plen, offset);
      plen += 4;
    }
  else
    {
      put16le (payload + plen, offset);
      plen += 2;
    }
  memcpy (payload + plen, data, dlen);
  plen += dlen;
  uint8_t rec[2048 + 16];
  int n = omf_record (rec, is_32bit ? 0xa1 : 0xa0, payload, plen);
  ob_write (ob, rec, n);
}

/* Write a LIDATA (0xA2) or LIDATA386 (0xA3) record.
   seg_idx: segment index (variable-length).
   offset: iterated data offset (16 or 32 bit).
   datablock: raw Data Block field bytes (Repeat Count + Block Count + content).
   dblen: length of datablock.  */
static void
ob_lidata (struct omf_buf *ob, int is_32bit,
	   int seg_idx, uint32_t offset,
	   const uint8_t *datablock, int dblen)
{
  uint8_t payload[4096];
  int plen = 0;
  plen += omf_index (payload + plen, seg_idx);
  if (is_32bit)
    {
      put32le (payload + plen, offset);
      plen += 4;
    }
  else
    {
      put16le (payload + plen, offset);
      plen += 2;
    }
  memcpy (payload + plen, datablock, dblen);
  plen += dblen;
  uint8_t rec[4096 + 16];
  int n = omf_record (rec, is_32bit ? 0xa3 : 0xa2, payload, plen);
  ob_write (ob, rec, n);
}

/* Write a MODEND (0x8A) or MODEND386 (0x8B).
   flags: 0x80 = main module, 0x40 = has start address.  */
static void
ob_modend (struct omf_buf *ob, int is_32bit, uint8_t flags)
{
  uint8_t payload[1] = { flags };
  uint8_t rec[16];
  int n = omf_record (rec, is_32bit ? 0x8b : 0x8a, payload, 1);
  ob_write (ob, rec, n);
}

/* Append a variable-width communal length to buf at pos.
   Returns new pos.  Encoding per §4.4:
     0x00-0x80 → 1 byte, 0x81+LE16 → 3, 0x84+LE24 → 4, 0x88+LE32 → 5.  */
static int
ob_comdef_len (uint8_t *buf, int pos, uint32_t val)
{
  if (val <= 0x80)
    { buf[pos++] = val; }
  else if (val <= 0xffff)
    {
      buf[pos++] = 0x81;
      put16le (buf + pos, val);
      pos += 2;
    }
  else if (val <= 0xffffff)
    {
      buf[pos++] = 0x84;
      buf[pos++] = val & 0xff;
      buf[pos++] = (val >> 8) & 0xff;
      buf[pos++] = (val >> 16) & 0xff;
    }
  else
    {
      buf[pos++] = 0x88;
      put32le (buf + pos, val);
      pos += 4;
    }
  return pos;
}

/* Append a COMDEF entry (NEAR or FAR) to buf at pos.
   Returns new pos.  */
static int
ob_comdef_near (uint8_t *buf, int pos, const char *name, int type_idx,
		uint32_t size)
{
  int slen = strlen (name);
  buf[pos++] = slen;
  memcpy (buf + pos, name, slen);
  pos += slen;
  pos += omf_index (buf + pos, type_idx);
  buf[pos++] = 0x62;		/* NEAR */
  pos = ob_comdef_len (buf, pos, size);
  return pos;
}

static int
ob_comdef_far (uint8_t *buf, int pos, const char *name, int type_idx,
	       uint32_t count, uint32_t elem_size)
{
  int slen = strlen (name);
  buf[pos++] = slen;
  memcpy (buf + pos, name, slen);
  pos += slen;
  pos += omf_index (buf + pos, type_idx);
  buf[pos++] = 0x61;		/* FAR */
  pos = ob_comdef_len (buf, pos, count);
  pos = ob_comdef_len (buf, pos, elem_size);
  return pos;
}

/* Write a FIXUPP (0x9C) or FIXUPP386 (0x9D) with the given subrecord bytes.
   Subrecords are already fully assembled (including their internal fields
   and following data).  The checksum is computed over the entire record.  */
static void
ob_fixupp (struct omf_buf *ob, int is_32bit,
	   const uint8_t *subrecs, int subrec_len)
{
  uint8_t payload[2048];
  memcpy (payload, subrecs, subrec_len);
  uint8_t rec[2048 + 16];
  int n = omf_record (rec, is_32bit ? 0x9d : 0x9c, payload, subrec_len);
  ob_write (ob, rec, n);
}

/* Build a THREAD subrecord byte sequence.
   buf must hold at least 6 bytes.
   Returns the number of bytes written.
   D=1 → FRAME thread, D=0 → TARGET thread.
   method: 0-6 (3-bit method from spec).
   tnum: 0-3 (thread slot number).
   index: for methods 0,1,2 and TARGET 4,5,6.  -1 = omit.
   For method 3 (explicit frame), index is the frame number (2 bytes).  */
static int
build_thread (uint8_t *buf, int is_frame, int method, int tnum, int index)
{
  int plen = 0;
  uint8_t first = (is_frame ? 0x40 : 0) | (method << 3) | (tnum & 3);
  buf[plen++] = first;
  /* Index field: present for methods 0,1,2; and for TARGET methods 4,5,6.  */
  if (method <= 2 || (!is_frame && method >= 4 && method <= 6))
    plen += omf_index (buf + plen, index >= 0 ? index : 0);
  /* Method 3 (explicit frame): index is a 16-bit frame number.
     But per spec, this is invalid/unsupported for FRAME threads.
     For TARGET threads with method 3, no index.  */
  return plen;
}

/* Build a FIXUP subrecord byte sequence.
   buf must hold at least 32 bytes (up to 3 + 2*idx + 2 + 4).
   Returns bytes written.

   Parameters mirror the spec:
   - location: 0-13
   - segrel: M bit (0=self-rel, 1=seg-rel)
   - data_rec_offset: 10-bit offset within current data record
   - fixdata: F Frame(3) T P Targt(2) as a single byte
   - frame_index / target_index: for methods requiring an index
     (-1 = omit, e.g. when F=1 or T=1)
   - displacement: for P=0 (present).  Set to 0 when P=1.
   - is_32bit: 1 for 32-bit displacement width.  */
static int
build_fixup (uint8_t *buf, int location, int segrel, int data_rec_offset,
	     int fixdata,
	     int frame_index, int target_index,
	     uint32_t displacement, int is_32bit)
{
  int plen = 0;
  /* Locat byte 0: 0x80 (FIXUP flag) | (segrel<<6) | (location<<2) | (data_rec_offset >> 8) */
  buf[plen++] = 0x80 | (segrel ? 0x40 : 0) | ((location & 0xf) << 2)
    | ((data_rec_offset >> 8) & 3);
  /* Locat byte 1: low 8 bits of data record offset */
  buf[plen++] = data_rec_offset & 0xff;
  /* Fix Data byte */
  buf[plen++] = fixdata;

  /* Conditional fields after the mandatory 3-byte prefix.  */
  int f_bit = (fixdata >> 7) & 1;
  int frame_method = (fixdata >> 4) & 7;
  int t_bit = (fixdata >> 3) & 1;
  int p_bit = (fixdata >> 2) & 1;
  int targt_method = (fixdata & 7);  /* P + Targt = 3 bits for explicit */

  /* §4.4: Explicit FRAME fields (F=0 only).  */
  if (!f_bit)
    {
      if (frame_method <= 2)
	plen += omf_index (buf + plen, frame_index >= 0 ? frame_index : 0);
      else if (frame_method == 3)
	{
	  /* F3 (explicit frame) — the backend rejects this.  We include
	     it for error-path tests.  2-byte explicit frame number.  */
	  put16le (buf + plen, frame_index >= 0 ? frame_index : 0);
	  plen += 2;
	}
      /* frame_method 4 (LEDATA) and 5 (TARGET): no extra fields.  */
    }

  /* §4.5: Explicit TARGET fields (T=0 only).  */
  if (!t_bit)
    {
      int target_method = targt_method;  /* (P:Targt) = 3-bit method */
      if (target_method <= 2 || (target_method >= 4 && target_method <= 6))
	plen += omf_index (buf + plen, target_index >= 0 ? target_index : 0);
      else if (target_method == 3 || target_method == 7)
	{
	  /* T3/T7: explicit frame number, 2 bytes (only when T=0).  */
	  put16le (buf + plen, target_index >= 0 ? target_index : 0);
	  plen += 2;
	}
    }

  /* §4.6: Displacement: present when P=0.  */
  if (!p_bit)
    {
      if (is_32bit)
	{
	  put32le (buf + plen, displacement);
	  plen += 4;
	}
      else
	{
	  put16le (buf + plen, displacement);
	  plen += 2;
	}
    }

  return plen;
}

/* ------------------------------------------------------------------ */
/*  Test case generators                                               */
/* ------------------------------------------------------------------ */

/* modend_startaddr.o — MODEND with Start Address subfield.
   Uses the worked example from modend_record_spec.md §6:
   Module Type 0xC1 (Main+Strt), F0+T0 → SEGDEF 1, displacement 0.  */
static void
gen_modend_startaddr (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "modend_startaddr");
  ob_lnames (&ob, "_TEXT");
  ob_segdef (&ob, 1, 16, 5, 2, 1, 0, 0);

  uint8_t data[16];
  memset (data, 0x90, 16);
  ob_ledata (&ob, 1, 1, 0, data, 16);

  /* MODEND (0x8A) with 6-byte start-address subfield:
     [0xC1] module type: Main=1, Strt=1
     [0x00] End Data: F=0 Frame=0 T=0 P=0 Targt=0
     [0x01] Frame Datum: SEGDEF index 1
     [0x01] Target Datum: SEGDEF index 1
     [0x0000] Target Displacement: 0 */
  uint8_t mod_payload[6];
  mod_payload[0] = 0xC1;
  mod_payload[1] = 0x00;
  mod_payload[2] = 0x01;
  mod_payload[3] = 0x01;
  put16le (mod_payload + 4, 0x0000);
  uint8_t rec[16];
  int n = omf_record (rec, 0x8a, mod_payload, 6);
  ob_write (&ob, rec, n);

  char path[256];
  snprintf (path, sizeof path, "%s/modend_startaddr.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* basic.o — minimal valid OMF object with one section.  */
static void
gen_basic (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "basic");
  ob_lnames (&ob, "_TEXT");
  ob_segdef (&ob, 1, 16, 5, 2, 1, 0, 0);  /* 386, dword align, public */

  uint8_t data[16];
  memset (data, 0x90, 16);
  ob_ledata (&ob, 1, 1, 0, data, 16);

  ob_modend (&ob, 0, 0);  /* 16-bit MODEND, no flags */

  char path[256];
  snprintf (path, sizeof path, "%s/basic.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* section_offsets.o — multiple LEDATA records at non-zero offsets.  */
static void
gen_section_offsets (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "section_offsets");
  ob_lnames (&ob, "_TEXT");
  ob_segdef (&ob, 1, 64, 5, 2, 1, 0, 0);  /* 64-byte segment */

  uint8_t data0[16] = { [0 ... 15] = 0x10 };
  uint8_t data1[32] = { [0 ... 31] = 0x20 };

  ob_ledata (&ob, 1, 1, 0, data0, 16);
  ob_ledata (&ob, 1, 1, 16, data1, 32);

  ob_modend (&ob, 0, 0);

  char path[256];
  snprintf (path, sizeof path, "%s/section_offsets.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* linsym.o — LINSYM (0xC4) record with line-number entries.  */
static void
gen_linsym (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "linsym");
  ob_lnames (&ob, "_TEXT");
  ob_lnames (&ob, "_foo");
  ob_segdef (&ob, 1, 32, 5, 2, 1, 0, 0);

  uint8_t data[16];
  memset (data, 0x90, 16);
  ob_ledata (&ob, 1, 1, 0, data, 16);

  /* LINSYM (0xC4, 16-bit offsets) with 3 entries for _foo (LNAMES idx 2).  */
  uint8_t ls_payload[64];
  int plen = 0;
  ls_payload[plen++] = 0x00;               /* flags: new instance */
  plen += omf_index (ls_payload + plen, 2); /* Public Name = "_foo" */
  put16le (ls_payload + plen, 10); plen += 2; /* line 10 */
  put16le (ls_payload + plen, 0);  plen += 2; /* offset 0 */
  put16le (ls_payload + plen, 12); plen += 2; /* line 12 */
  put16le (ls_payload + plen, 6);  plen += 2; /* offset 6 */
  put16le (ls_payload + plen, 15); plen += 2; /* line 15 */
  put16le (ls_payload + plen, 14); plen += 2; /* offset 14 */

  uint8_t rec[512];
  int n = omf_record (rec, 0xc4, ls_payload, plen);
  ob_write (&ob, rec, n);

  ob_modend (&ob, 0, 0);

  char path[256];
  snprintf (path, sizeof path, "%s/linsym.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* symbols.o — EXTDEF and PUBDEF symbols.  */
static void
gen_symbols (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "symbols");
  ob_lnames (&ob, "_TEXT");
  ob_lnames (&ob, "my_group");
  ob_segdef (&ob, 1, 16, 5, 2, 1, 0, 0);  /* _TEXT at index 1 */

  int grp_segs[] = { 1 };
  ob_grpdef (&ob, 2, grp_segs, 1);  /* my_group at index 1 (grpdef) */

  /* EXTDEF: an external symbol */
  ob_extdef (&ob, "external_func", 0);

  /* PUBDEF: a public symbol in _TEXT */
  ob_pubdef (&ob, 1, 0, 1, "public_var", 4, 0);

  uint8_t data[16];
  memset (data, 0xcc, 16);
  ob_ledata (&ob, 1, 1, 0, data, 16);

  ob_modend (&ob, 0, 0);

  char path[256];
  snprintf (path, sizeof path, "%s/symbols.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* fixups_simple.o — a FIXUPP record with simple FIXUP subrecords.
   Uses F0+T0 (SEGDEF based) with various location values.  */
static void
gen_fixups_simple (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "fixups_simple");
  ob_lnames (&ob, "_TEXT");
  ob_segdef (&ob, 1, 16, 5, 2, 1, 0, 0);

  uint8_t data[16];
  memset (data, 0x90, 16);
  ob_ledata (&ob, 1, 1, 0, data, 16);

  /* FIXUPP record (0x9C) with one FIXUP subrecord.
     F0: FRAME from SEGDEF (index=1)
     T0: TARGET from SEGDEF (index=1)
     P=0: displacement present
     location=1 (OFF16)  */
  uint8_t fixup[32];
  int fn = build_fixup (fixup, 1, 0, 0,	/* loc=1, self-rel, offset=0 */
			0x00,			/* F=0 Frame=0 T=0 P=0 Targt=0 */
			1, 1,			/* frame_idx=1, target_idx=1 */
			0, 0);			/* displacement=0, 16-bit */
  ob_fixupp (&ob, 0, fixup, fn);

  ob_modend (&ob, 0, 0);

  char path[256];
  snprintf (path, sizeof path, "%s/fixups_simple.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* fixups_threads.o — THREAD + FIXUP via thread slots.  */
static void
gen_fixups_threads (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "fixups_threads");
  ob_lnames (&ob, "_TEXT");
  ob_segdef (&ob, 1, 16, 5, 2, 1, 0, 0);

  uint8_t data[16];
  memset (data, 0x90, 16);
  ob_ledata (&ob, 1, 1, 0, data, 16);

  /* FIXUPP with THREAD + FIXUP subrecords.
     THREAD: method=0 (SEGDEF), slot=0, FRAME, index=1
     FIXUP:  F=1 (Frame via thread=0), T=0, method=0 (SEGDEF), index=1
             P=0, displacement=0, loc=1  */
  uint8_t subrecs[64];
  int plen = 0;

  /* THREAD subrecord */
  uint8_t thd[8];
  int tn = build_thread (thd, 1, 0, 0, 1);
  memcpy (subrecs + plen, thd, tn);
  plen += tn;

  /* FIXUP subrecord using FRAME via thread (F=1) */
  uint8_t fix[32];
  int fn = build_fixup (fix, 1, 0, 0,
			0x80,		/* F=1 (via thread) Frame=0 T=0 P=0 Targt=0 */
			-1, 1,		/* no frame idx (via thread), target=1 */
			0, 0);
  memcpy (subrecs + plen, fix, fn);
  plen += fn;

  ob_fixupp (&ob, 0, subrecs, plen);

  ob_modend (&ob, 0, 0);

  char path[256];
  snprintf (path, sizeof path, "%s/fixups_threads.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* fixups_all.o — comprehensive: F0-F5, T0-T7, P-bit on/off, 32-bit.  */
static void
gen_fixups_all (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "fixups_all");
  ob_lnames (&ob, "_TEXT");
  ob_lnames (&ob, "my_group");
  ob_lnames (&ob, "_DATA");
  ob_segdef (&ob, 1, 64, 5, 2, 1, 0, 0);   /* _TEXT (idx 1) */
  ob_segdef (&ob, 1, 32, 5, 2, 3, 0, 0);   /* _DATA  (idx 2) */

  int grp_segs[] = { 1 };
  ob_grpdef (&ob, 2, grp_segs, 1);           /* my_group (idx 1) */

  ob_extdef (&ob, "external_func", 0);        /* EXTDEF idx 1 */

  uint8_t data[64];
  memset (data, 0x90, 64);
  ob_ledata (&ob, 1, 1, 0, data, 64);

  /* Build a FIXUPP386 (0x9D) record with multiple subrecords.  */
  uint8_t subrecs[256];
  int plen = 0;

  /* THREAD subrecords for later use */
  uint8_t thd[8];

  /* Thread 0: FRAME, method=0 (SEGDEF), index=1 (_TEXT) */
  int tn = build_thread (thd, 1, 0, 0, 1);
  memcpy (subrecs + plen, thd, tn); plen += tn;

  /* Thread 1: FRAME, method=4 (LEDATA), no index needed */
  tn = build_thread (thd, 1, 4, 1, -1);
  memcpy (subrecs + plen, thd, tn); plen += tn;

  /* Thread 2: TARGET, method=2 (EXTDEF), index=1 (external_func) */
  tn = build_thread (thd, 0, 2, 2, 1);
  memcpy (subrecs + plen, thd, tn); plen += tn;

  /* Thread 3: TARGET, method=3 (explicit frame), no index */
  tn = build_thread (thd, 0, 3, 3, -1);
  memcpy (subrecs + plen, thd, tn); plen += tn;

  uint8_t fix[32];
  int fn;

  /* FIXUP 1: F0+T0, SEGDEF indices, disp present, loc=1, self-rel.
     fixdata = F=0 Frame=0 T=0 P=0 Targt=0 = 0x00.  */
  fn = build_fixup (fix, 1, 0, 0,
		    0x00,		/* F0 T0 */
		    1, 1,		/* frame=_TEXT, target=_TEXT */
		    0x1234, 1);		/* disp=0x1234, 32-bit */
  memcpy (subrecs + plen, fix, fn); plen += fn;

  /* FIXUP 2: F1+T2 (GRPDEF + EXTDEF), disp absent (P=1), loc=9 (OFF32), segrel.
     fixdata = F=0 Frame=1 T=0 P=1 Targt=2 = 0x16.  */
  fn = build_fixup (fix, 9, 1, 8,
		    0x16,		/* F1 T2, P=1 */
		    1, 1,		/* frame=GRPDEF1, target=EXTDEF1 */
		    0, 1);		/* no disp */
  memcpy (subrecs + plen, fix, fn); plen += fn;

  /* FIXUP 3: F4+T3 (LEDATA frame + explicit target T3), disp present.
     fixdata = F=0 Frame=4 T=0 P=0 Targt=3 = 0x43.  */
  fn = build_fixup (fix, 1, 0, 16,
		    0x43,		/* F4 T3, P=0 */
		    -1, 0xbeef,		/* no frame idx, explicit target */
		    0x1000, 1);
  memcpy (subrecs + plen, fix, fn); plen += fn;

  /* FIXUP 4: F=1 (via Thread 0), T=1 (via Thread 2).
     fixdata = F=1 Frame=0 T=1 P=0 Targt=2 = 0x8A.  */
  fn = build_fixup (fix, 1, 0, 32,
		    0x8A,		/* F=1 via thread0, T=1 via thread2 */
		    -1, -1,
		    0x5678, 1);
  memcpy (subrecs + plen, fix, fn); plen += fn;

  /* FIXUP 5: F5+T0 — FRAME via target's seg, disp present.
     fixdata = F=0 Frame=5 T=0 P=0 Targt=0 = 0x50.  */
  fn = build_fixup (fix, 1, 0, 48,
		    0x50,		/* F5 T0 */
		    -1, 1,		/* no frame idx, target=_TEXT */
		    0, 1);
  memcpy (subrecs + plen, fix, fn); plen += fn;

  ob_fixupp (&ob, 1, subrecs, plen);  /* 32-bit FIXUPP386 */

  ob_modend (&ob, 0, 0);

  char path[256];
  snprintf (path, sizeof path, "%s/fixups_all.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* error_no_leidata.o — FIXUPP record without preceding LEDATA.
   Should be rejected by the backend.  */
static void
gen_error_no_leidata (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "error_no_leidata");
  ob_lnames (&ob, "_TEXT");
  ob_segdef (&ob, 1, 16, 5, 2, 1, 0, 0);

  /* FIXUPP without LEDATA — should fail.  */
  uint8_t fix[32];
  int fn = build_fixup (fix, 1, 0, 0,
			0x00, 1, 1, 0, 0);
  ob_fixupp (&ob, 0, fix, fn);

  ob_modend (&ob, 0, 0);

  char path[256];
  snprintf (path, sizeof path, "%s/error_no_leidata.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* error_undefined_thread.o — FIXUP referencing an undefined thread.  */
static void
gen_error_undefined_thread (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "error_undefined_thread");
  ob_lnames (&ob, "_TEXT");
  ob_segdef (&ob, 1, 16, 5, 2, 1, 0, 0);

  uint8_t data[16];
  memset (data, 0x90, 16);
  ob_ledata (&ob, 1, 1, 0, data, 16);

  /* F=1 (via thread 0), but no THREAD subrecord defined.  */
  uint8_t fix[32];
  int fn = build_fixup (fix, 1, 0, 0,
			0x80,		/* F=1 Frame=0 T=0 P=0 Targt=0 */
			-1, 1,		/* no frame idx, target=SEGDEF1 */
			0, 0);
  ob_fixupp (&ob, 0, fix, fn);

  ob_modend (&ob, 0, 0);

  char path[256];
  snprintf (path, sizeof path, "%s/error_undefined_thread.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* error_bad_location.o — FIXUP with unsupported location >= 14.  */
static void
gen_error_bad_location (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "error_bad_location");
  ob_lnames (&ob, "_TEXT");
  ob_segdef (&ob, 1, 16, 5, 2, 1, 0, 0);

  uint8_t data[16];
  memset (data, 0x90, 16);
  ob_ledata (&ob, 1, 1, 0, data, 16);

  /* FIXUP with location=14 (invalid, >= 14).  */
  uint8_t fix[32];
  int fn = build_fixup (fix, 14, 0, 0,
			0x00, 1, 1, 0, 0);
  ob_fixupp (&ob, 0, fix, fn);

  ob_modend (&ob, 0, 0);

  char path[256];
  snprintf (path, sizeof path, "%s/error_bad_location.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* error_f3_frame.o — FIXUP with FRAME method F3 (invalid).  */
static void
gen_error_f3_frame (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "error_f3_frame");
  ob_lnames (&ob, "_TEXT");
  ob_segdef (&ob, 1, 16, 5, 2, 1, 0, 0);

  uint8_t data[16];
  memset (data, 0x90, 16);
  ob_ledata (&ob, 1, 1, 0, data, 16);

  /* FIXUP with Frame=3 (F3, explicit frame — invalid per spec).  */
  uint8_t fix[32];
  int fn = build_fixup (fix, 1, 0, 0,
			0x30,		/* F=0 Frame=3 T=0 P=0 Targt=0 */
			0, 1,		/* frame=0 (explicit), target=SEGDEF1 */
			0, 0);
  ob_fixupp (&ob, 0, fix, fn);

  ob_modend (&ob, 0, 0);

  char path[256];
  snprintf (path, sizeof path, "%s/error_f3_frame.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* ------------------------------------------------------------------ */
/*  COMDEF test objects                                                 */
/* ------------------------------------------------------------------ */

static void
gen_comdef (const char *outdir)
{
  /* 1. COMDEF with NEAR and FAR entries — valid.  */
  {
    struct omf_buf ob;
    ob.len = 0;

    ob_theadr (&ob, "comdef");
    ob_lnames (&ob, "_TEXT");
    ob_segdef (&ob, 0, 16, 5, 2, 1, 0, 0);

    /* Build COMDEF payload with 3 entries:
         _var   (NEAR, size 2)
         _var2  (NEAR, size 32768)
         _var3  (FAR,  count=400, element_size=1)  */
    uint8_t payload[256];
    int pos = 0;
    pos = ob_comdef_near (payload, pos, "_var", 0, 2);
    pos = ob_comdef_near (payload, pos, "_var2", 0, 32768);
    pos = ob_comdef_far  (payload, pos, "_var3", 0, 400, 1);

    uint8_t rec[512];
    int n = omf_record (rec, 0xb0, payload, pos);
    ob_write (&ob, rec, n);

    ob_modend (&ob, 0, 0);

    char path[256];
    snprintf (path, sizeof path, "%s/comdef.o", outdir);
    FILE *f = fopen (path, "wb");
    if (!f) { perror (path); exit (IO_ERROR); }
    fwrite (ob.data, 1, ob.len, f);
    fclose (f);
    printf ("  wrote %s (%d bytes)\n", path, ob.len);
  }

  /* 2. COMDEF with unknown data type — error.  */
  {
    struct omf_buf ob;
    ob.len = 0;

    ob_theadr (&ob, "comdef_bad_type");
    ob_lnames (&ob, "_TEXT");

    uint8_t payload[256];
    int pos = 0;
    pos = ob_comdef_near (payload, pos, "_bad", 0, 4);

    /* Corrupt the data type byte from 0x62 to 0x63.  */
    /* The payload structure at this point is:
         name_len name_bytes type_idx data_type length
       The data_type byte is at offset pos - 2 (before the length byte).
       Walk back: len(1) + name(4) + type_idx(1) = 6, so data_type at index 6. */
    payload[6] = 0x63; /* invalid data type */

    uint8_t rec[512];
    int n = omf_record (rec, 0xb0, payload, pos);
    ob_write (&ob, rec, n);

    ob_modend (&ob, 0, 0);

    char path[256];
    snprintf (path, sizeof path, "%s/error_comdef_bad_type.o", outdir);
    FILE *f = fopen (path, "wb");
    if (!f) { perror (path); exit (IO_ERROR); }
    fwrite (ob.data, 1, ob.len, f);
    fclose (f);
    printf ("  wrote %s (%d bytes)\n", path, ob.len);
  }
}

/* comdat.o — COMDAT386 (0xC3) record with data payload.  */
static void
gen_comdat (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "comdat");
  ob_lnames (&ob, "_TEXT");
  ob_segdef (&ob, 1, 32, 5, 2, 1, 0, 0);

  uint8_t payload[256];
  int plen = 0;
  plen += omf_index (payload + plen, 1);
  put32le (payload + plen, 0); plen += 4;
  payload[plen++] = 0;
  uint8_t cdata[16] = { [0 ... 15] = 0x90 };
  memcpy (payload + plen, cdata, 16);
  plen += 16;

  uint8_t rec[512];
  int n = omf_record (rec, 0xc3, payload, plen);
  ob_write (&ob, rec, n);

  ob_modend (&ob, 0, 0);

  char path[256];
  snprintf (path, sizeof path, "%s/comdat.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* Write an unrecognized/ignored record to test skipping by length. */
static void
ob_unrecognized (struct omf_buf *ob, int rectype, int plen)
{
  uint8_t payload[256];
  for (int i = 0; i < plen; i++)
    payload[i] = 0xAA;
  uint8_t rec[512];
  int n = omf_record (rec, rectype, payload, plen);
  ob_write (ob, rec, n);
}

/* skipped_record.o — basic OMF object containing unrecognized record types
   that the parser should skip gracefully by length. */
static void
gen_skipped_record (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "skipped_record");
  ob_lnames (&ob, "_TEXT");
  ob_segdef (&ob, 1, 16, 5, 2, 1, 0, 0);

  /* Inject an unrecognized record type 0xEE with 16 bytes of payload */
  ob_unrecognized (&ob, 0xee, 16);

  /* Inject TYPDEF (0x8E), which is also a skipped record in the spec */
  ob_unrecognized (&ob, 0x8e, 8);

  uint8_t data[16];
  memset (data, 0x90, 16);
  ob_ledata (&ob, 1, 1, 0, data, 16);

  ob_modend (&ob, 0, 0);

  char path[256];
  snprintf (path, sizeof path, "%s/skipped_record.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* ------------------------------------------------------------------ */
/*  LIDATA test objects                                                 */
/* ------------------------------------------------------------------ */

/* lidata_simple.o — TIS Example 2: 10 dup('ALPHA','BETA') in 16-bit LIDATA.
   Expected section content: 90 bytes of "ALPHABETA" repeated 10 times.  */
static void
gen_lidata_simple (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "lidata_simple");
  ob_lnames (&ob, "_TEXT");
  ob_segdef (&ob, 0, 90, 5, 2, 1, 0, 0);

  /* Data block: RepeatCount=10, BlockCount=2
       ChildA: RepeatCount=1, BlockCount=0, count=5, "ALPHA"
       ChildB: RepeatCount=1, BlockCount=0, count=4, "BETA"  */
  uint8_t datablock[] = {
    0x0A, 0x00,                              /* RepeatCount = 10 */
    0x02, 0x00,                              /* BlockCount = 2 */
      0x01, 0x00,  0x00, 0x00,  0x05,       /* ChildA leaf */
      'A', 'L', 'P', 'H', 'A',
      0x01, 0x00,  0x00, 0x00,  0x04,       /* ChildB leaf */
      'B', 'E', 'T', 'A'
  };
  ob_lidata (&ob, 0, 1, 0, datablock, sizeof (datablock));

  ob_modend (&ob, 0, 0);

  char path[256];
  snprintf (path, sizeof path, "%s/lidata_simple.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* lidata_nested.o — TIS Example 1 style: nested data blocks.
   Outer: RepeatCount=2, BlockCount=2
     Child1: RepeatCount=3, BlockCount=0, count=2, bytes=40 41
     Child2: RepeatCount=2, BlockCount=0, count=2, bytes=50 51
   Expected expansion: 20 bytes.  */
static void
gen_lidata_nested (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "lidata_nested");
  ob_lnames (&ob, "_TEXT");
  ob_segdef (&ob, 0, 20, 5, 2, 1, 0, 0);

  uint8_t datablock[] = {
    0x02, 0x00,                              /* RepeatCount = 2 */
    0x02, 0x00,                              /* BlockCount = 2 */
      0x03, 0x00,  0x00, 0x00,  0x02,       /* Child1: repeat=3, leaf, count=2 */
      0x40, 0x41,
      0x02, 0x00,  0x00, 0x00,  0x02,       /* Child2: repeat=2, leaf, count=2 */
      0x50, 0x51
  };
  ob_lidata (&ob, 0, 1, 0, datablock, sizeof (datablock));

  ob_modend (&ob, 0, 0);

  char path[256];
  snprintf (path, sizeof path, "%s/lidata_nested.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* lidata_32bit.o — LIDATA386 record with 32-bit iterated data offset.  */
static void
gen_lidata_32bit (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "lidata_32bit");
  ob_lnames (&ob, "_TEXT");
  ob_segdef (&ob, 1, 64, 5, 2, 1, 0, 0);

  /* Simplest possible: RepeatCount=1, BlockCount=0, count=4, data=DE AD BE EF */
  uint8_t datablock[] = {
    0x01, 0x00, 0x00, 0x00,                  /* RepeatCount = 1 (32-bit) */
    0x00, 0x00,                              /* BlockCount = 0 */
    0x04,                                    /* count = 4 */
    0xDE, 0xAD, 0xBE, 0xEF
  };
  ob_lidata (&ob, 1, 1, 0x10, datablock, sizeof (datablock));

  ob_modend (&ob, 0, 0);

  char path[256];
  snprintf (path, sizeof path, "%s/lidata_32bit.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* lidata_truncated.o — leaf content truncated (negative test).
   RepeatCount=1, BlockCount=0, count=2, but only 1 data byte follows.  */
static void
gen_lidata_truncated (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "lidata_truncated");
  ob_lnames (&ob, "_TEXT");
  ob_segdef (&ob, 0, 16, 5, 2, 1, 0, 0);

  uint8_t datablock[] = {
    0x01, 0x00,                              /* RepeatCount = 1 */
    0x00, 0x00,                              /* BlockCount = 0 */
    0x02,                                    /* count = 2, but only 1 byte */
    0x40
  };
  ob_lidata (&ob, 0, 1, 0, datablock, sizeof (datablock));

  ob_modend (&ob, 0, 0);

  char path[256];
  snprintf (path, sizeof path, "%s/lidata_truncated.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* lidata_zero_segidx.o — zero segment index (negative test).  */
static void
gen_lidata_zero_segidx (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "lidata_zero_segidx");
  ob_lnames (&ob, "_TEXT");
  ob_segdef (&ob, 0, 16, 5, 2, 1, 0, 0);

  /* Valid data block but with seg_idx=0 (which must be rejected). */
  uint8_t datablock[] = {
    0x01, 0x00,  0x00, 0x00,  0x02,  0x41, 0x42
  };
  ob_lidata (&ob, 0, 0, 0, datablock, sizeof (datablock));

  ob_modend (&ob, 0, 0);

  char path[256];
  snprintf (path, sizeof path, "%s/lidata_zero_segidx.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* lidata_overflow.o — expansion exceeds non-COMDAT segment bounds (negative test).  */
static void
gen_lidata_overflow (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "lidata_overflow");
  ob_lnames (&ob, "_TEXT");
  /* Declare segment as only 4 bytes, but data block claims 6.  */
  ob_segdef (&ob, 0, 4, 5, 2, 1, 0, 0);

  uint8_t datablock[] = {
    0x01, 0x00,                              /* RepeatCount = 1 */
    0x00, 0x00,                              /* BlockCount = 0 */
    0x06,                                    /* count = 6 */
    0x01, 0x02, 0x03, 0x04, 0x05, 0x06
  };
  ob_lidata (&ob, 0, 1, 0, datablock, sizeof (datablock));

  ob_modend (&ob, 0, 0);

  char path[256];
  snprintf (path, sizeof path, "%s/lidata_overflow.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* ------------------------------------------------------------------ */
/*  SEGDEF ACBP bit tests: B (Big), A (alignment), zero name indices    */
/* ------------------------------------------------------------------ */

/* big_bit.o — 16-bit SEGDEF with B=1, seglen=0.
   Expected: section size = 0x10000 (65536), not 0.  */
static void
gen_big_bit (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "big_bit");
  ob_lnames (&ob, "_TEXT");
  /* big=1, use32=0, alignment=5 (dword), combination=2 (public) */
  ob_segdef_ex (&ob, 0, 0, 5, 2, 1, 0, 0, 1, 0);

  uint8_t data[16];
  memset (data, 0x90, 16);
  ob_ledata (&ob, 0, 1, 0, data, 16);

  ob_modend (&ob, 0, 0);

  char path[256];
  snprintf (path, sizeof path, "%s/big_bit.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* alignment_6.o — 32-bit SEGDEF with alignment=6 (LTL, paragraph).
   Expected: alignment_power = 4 (2**4 = 16-byte paragraph).  */
static void
gen_alignment_6 (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "alignment_6");
  ob_lnames (&ob, "_TEXT");
  /* alignment=6, combination=2, big=0, use32=0 */
  ob_segdef_ex (&ob, 1, 16, 6, 2, 1, 0, 0, 0, 0);

  uint8_t data[16];
  memset (data, 0x90, 16);
  ob_ledata (&ob, 1, 1, 0, data, 16);

  ob_modend (&ob, 0, 0);

  char path[256];
  snprintf (path, sizeof path, "%s/alignment_6.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* alignment_7.o — 32-bit SEGDEF with alignment=7 (undefined).
   Expected: does not reject; fallback alignment_power = 0 (byte-aligned).  */
static void
gen_alignment_7 (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "alignment_7");
  ob_lnames (&ob, "_TEXT");
  /* alignment=7, combination=2, big=0, use32=0 */
  ob_segdef_ex (&ob, 1, 16, 7, 2, 1, 0, 0, 0, 0);

  uint8_t data[16];
  memset (data, 0x90, 16);
  ob_ledata (&ob, 1, 1, 0, data, 16);

  ob_modend (&ob, 0, 0);

  char path[256];
  snprintf (path, sizeof path, "%s/alignment_7.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* zero_indices.o — SEGDEF with name/class/overlay indices all zero.
   Expected: parser accepts gracefully with default names.  */
static void
gen_zero_indices (const char *outdir)
{
  struct omf_buf ob;
  ob.len = 0;

  ob_theadr (&ob, "zero_indices");
  ob_lnames (&ob, "_TEXT");
  /* name_idx=0, class_idx=0, overlay_idx=0 — all zero.
     Only LNAMES[1]="_TEXT" exists, but we pass 0 for all.
     The BFD backend substitutes "UNNAMED"/"" for zero indices.  */
  ob_segdef (&ob, 1, 16, 5, 2, 0, 0, 0);

  uint8_t data[16];
  memset (data, 0x90, 16);
  ob_ledata (&ob, 1, 1, 0, data, 16);

  ob_modend (&ob, 0, 0);

  char path[256];
  snprintf (path, sizeof path, "%s/zero_indices.o", outdir);
  FILE *f = fopen (path, "wb");
  if (!f) { perror (path); exit (IO_ERROR); }
  fwrite (ob.data, 1, ob.len, f);
  fclose (f);
  printf ("  wrote %s (%d bytes)\n", path, ob.len);
}

/* ------------------------------------------------------------------ */
/*  main                                                                */
/* ------------------------------------------------------------------ */

static void
usage (void)
{
  fprintf (stderr, "Usage: gentestomf <output-directory>\n");
  exit (INCORRECT_USAGE);
}

int
main (int argc, char **argv)
{
  const char *outdir;

  if (argc != 2)
    usage ();

  outdir = argv[1];

  if (chdir (outdir) != 0)
    {
      perror (outdir);
      return IO_ERROR;
    }

  printf ("gentestomf: generating OMF test objects in %s\n", outdir);

  gen_basic (outdir);
  gen_modend_startaddr (outdir);
  gen_section_offsets (outdir);
  gen_symbols (outdir);
  gen_linsym (outdir);
  gen_fixups_simple (outdir);
  gen_fixups_threads (outdir);
  gen_fixups_all (outdir);
  gen_error_no_leidata (outdir);
  gen_error_undefined_thread (outdir);
  gen_error_bad_location (outdir);
  gen_error_f3_frame (outdir);
  gen_comdef (outdir);
  gen_comdat (outdir);
  gen_skipped_record (outdir);

  gen_lidata_simple (outdir);
  gen_lidata_nested (outdir);
  gen_lidata_32bit (outdir);
  gen_lidata_truncated (outdir);
  gen_lidata_zero_segidx (outdir);
  gen_lidata_overflow (outdir);

  gen_big_bit (outdir);
  gen_alignment_6 (outdir);
  gen_alignment_7 (outdir);
  gen_zero_indices (outdir);

  printf ("gentestomf: done\n");
  return 0;
}
