/* BFD back-end for ix86 OMF objects.
   Copyright 2007 Free Software Foundation, Inc.
   Written by Bernd Jendrissek <bernd.jendrissek@gmail.com>
   Based on bfd/binary.c.

   Copyright (C) 2024-2026 sillyc0n <sillyc0n@proton.me>.

   This file is part of BFD, the Binary File Descriptor library.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1301, USA.  */

#include "sysdep.h"
#include "bfd.h"
#include "safe-ctype.h"
#include "libbfd.h"
#include "strtab.h"

/* Runtime debug output controlled by OMF_DEBUG environment variable.  */
static bool omf_debug;

static void __attribute__((constructor))
omf_init_debug (void)
{
  omf_debug = getenv ("OMF_DEBUG") != NULL;
}

#define OMF_RECORD_THEADR      0x80
#define OMF_RECORD_LHEADR      0x82
#define OMF_RECORD_COMENT      0x88
#define OMF_RECORD_MODEND      0x8a
#define OMF_RECORD_MODEND386   0x8b
#define OMF_RECORD_EXTDEF      0x8c
#define OMF_RECORD_TYPDEF      0x8e
#define OMF_RECORD_PUBDEF      0x90
#define OMF_RECORD_PUBDEF386   0x91
#define OMF_RECORD_LINNUM      0x94
#define OMF_RECORD_LINNUM386   0x95
#define OMF_RECORD_LNAMES      0x96
#define OMF_RECORD_SEGDEF      0x98
#define OMF_RECORD_SEGDEF386   0x99
#define OMF_RECORD_GRPDEF      0x9a
#define OMF_RECORD_FIXUPP      0x9c
#define OMF_RECORD_FIXUPP386   0x9d
#define OMF_RECORD_LEDATA      0xa0
#define OMF_RECORD_LEDATA386   0xa1
#define OMF_RECORD_LIDATA      0xa2
#define OMF_RECORD_LIDATA386   0xa3
#define OMF_RECORD_COMDEF      0xb0
#define OMF_RECORD_BAKPAT      0xb2
#define OMF_RECORD_LEXTDEF     0xb4
#define OMF_RECORD_LEXTDEF386  0xb5
#define OMF_RECORD_LPUBDEF     0xb6
#define OMF_RECORD_LPUBDEF386  0xb7
#define OMF_RECORD_LCOMDEF     0xb8
#define OMF_RECORD_CEXTDEF     0xbc
#define OMF_RECORD_COMDAT      0xc2
#define OMF_RECORD_COMDAT386   0xc3
#define OMF_RECORD_LINSYM      0xc4
#define OMF_RECORD_LINSYM386   0xc5
#define OMF_RECORD_ALIAS       0xc6
#define OMF_RECORD_NBKPAT      0xc8
#define OMF_RECORD_LLNAMES     0xca
#define OMF_RECORD_VERNUM      0xcc
#define OMF_RECORD_VENDEXT     0xce

#define OMF_COMENT_TRANSLATOR          0x00
#define OMF_COMENT_INTEL_COPYRIGHT     0x01
#define OMF_COMENT_DEFAULT_LIBRARY_OBS 0x81
#define OMF_COMENT_WAT_PROC_MODEL      0x9b
#define OMF_COMENT_MSDOS_VERSION       0x9c
#define OMF_COMENT_MEMORY_MODEL        0x9d
#define OMF_COMENT_DOSSEG              0x9e
#define OMF_COMENT_DEFAULT_LIBRARY     0x9f
#define OMF_COMENT_EXT                 0xa0
#define OMF_COMENT_EXT_IMPDEF          0x01
#define OMF_COMENT_EXT_EXPDEF          0x02
#define OMF_COMENT_EXT_INCDEF          0x03
#define OMF_COMENT_EXT_PROTMEM         0x04
#define OMF_COMENT_EXT_LNKDIR          0x05
#define OMF_COMENT_EXT_BIGENDIAN       0x06
#define OMF_COMENT_EXT_PRECOMP         0x07
#define OMF_COMENT_DLL_ENTRY           0xa0
#define OMF_COMENT_NEWEXT              0xa1
#define OMF_COMENT_PASS_SEPARATOR      0xa2
#define OMF_COMENT_LIBMOD              0xa3
#define OMF_COMENT_EXESTR              0xa4
#define OMF_COMENT_INCERR              0xa6
#define OMF_COMENT_NOPAD               0xa7
#define OMF_COMENT_WKEXT               0xa8
#define OMF_COMENT_LZEXT               0xa9
#define OMF_COMENT_EASY_OMF            0xaa
#define OMF_COMENT_RANDOM_COMMENT      0xda
#define OMF_COMENT_COMPILER            0xdb
#define OMF_COMENT_DATE                0xdc
#define OMF_COMENT_TIME                0xdd
#define OMF_COMENT_USER                0xdf
#define OMF_COMENT_SYMBOL_TYPE_EXTDEF  0xe0
#define OMF_COMENT_SYMBOL_TYPE_PUBDEF  0xe1
#define OMF_COMENT_STRUCT_MEMBER       0xe2
#define OMF_COMENT_TYPDEF              0xe3
#define OMF_COMENT_ENUM_MEMBER         0xe4
#define OMF_COMENT_SCOPE_BEGIN         0xe5
#define OMF_COMENT_LOCALS              0xe6
#define OMF_COMENT_SCOPE_END           0xe7
#define OMF_COMENT_SOURCE_FILE         0xe8
#define OMF_COMENT_DEPENDENCIES        0xe9
#define OMF_COMENT_COMPILE_PARAMETERS  0xea
#define OMF_COMENT_MATCHED_TYPE_EXTDEF 0xeb
#define OMF_COMENT_MATCHED_TYPE_PUBDEF 0xec
#define OMF_COMENT_CLASSDEF            0xed
#define OMF_COMENT_COVERAGE_OFFSET     0xee
#define OMF_COMENT_LARGE_SCOPE_BEGIN   0xf5
#define OMF_COMENT_LARGE_LOCALS        0xf6
#define OMF_COMENT_LARGE_SCOPE_END     0xf7
#define OMF_COMENT_MEMBER_FUNCTION     0xf8
#define OMF_COMENT_DEBUG_VERSION       0xf9
#define OMF_COMENT_OPT_FLAGS           0xfa
#define OMF_COMENT_LINKER_DIRECTIVE    0xfe
#define OMF_COMENT_LINKER_DIRECTIVE2   0xfd
#define OMF_COMENT_COMMAND_LINE        0xff
#define OMF_COMENT_LIBRARY_COMMENT     0xff

#define OMF_MODEND_MAIN_MODULE         0x80
#define OMF_MODEND_START_ADDRESS       0x40

#define OMF_PUBDEF_SEGMENT_ABSOLUTE    OMF_SEGDEF_NONE

#define OMF_LNAMES_NONE                0

#define OMF_SEGDEF_NONE                0

#define OMF_SEGDEF_ALIGNMENT_MASK              0xe0
#define OMF_SEGDEF_ALIGNMENT_SHIFT             5
#define OMF_SEGDEF_ALIGNMENT_ABSOLUTE          0
#define OMF_SEGDEF_ALIGNMENT_RELOC_BYTE        1
#define OMF_SEGDEF_ALIGNMENT_RELOC_WORD        2
#define OMF_SEGDEF_ALIGNMENT_RELOC_PARA        3
#define OMF_SEGDEF_ALIGNMENT_RELOC_PAGE        4
#define OMF_SEGDEF_ALIGNMENT_RELOC_DWORD       5
#define OMF_SEGDEF_ALIGNMENT_UNNAMED_ABSOLUTE  OMF_SEGDEF_ALIGNMENT_RELOC_DWORD
#define OMF_SEGDEF_ALIGNMENT_LTL_PARA          6
#define OMF_SEGDEF_ALIGNMENT_UNDEFINED         7

#define OMF_SEGDEF_COMBINATION_MASK            0x1c
#define OMF_SEGDEF_COMBINATION_SHIFT           2
#define OMF_SEGDEF_COMBINATION_PRIVATE         0
#define OMF_SEGDEF_COMBINATION_RESERVED_1      1
#define OMF_SEGDEF_COMBINATION_COMMON_INTEL    OMF_SEGDEF_COMBINATION_RESERVED_1
#define OMF_SEGDEF_COMBINATION_PUBLIC_2        2
#define OMF_SEGDEF_COMBINATION_RESERVED_3      3
#define OMF_SEGDEF_COMBINATION_PUBLIC_4        4
#define OMF_SEGDEF_COMBINATION_STACK           5
#define OMF_SEGDEF_COMBINATION_COMMON          6
#define OMF_SEGDEF_COMBINATION_PUBLIC_7        7
#define OMF_SEGDEF_COMBINATION_PUBLIC          OMF_SEGDEF_COMBINATION_PUBLIC_2

/* FIXUPP record constants — see fixupp_record_spec.md for full spec.
   §4.3: Fix Data byte layout: F(1) Frame(3) T(1) P(1) Targt(2). */
/* Subrecord type detection: high bit of first subrecord byte.  §2.  */
#define OMF_FIXUPP_FIXUP               0x80     /* 1000 0000 — FIXUP vs THREAD */

/* TARGET method constants.  §3.2 TARGET methods (D=0).  */
#define OMF_FIXUPP_TARGET_SEGDEF       0        /* T0: segment index + displacement */
#define OMF_FIXUPP_TARGET_GRPDEF       1        /* T1: group index + displacement */
#define OMF_FIXUPP_TARGET_EXTDEF       2        /* T2: external index + displacement */
#define OMF_FIXUPP_TARGET_EXPLICIT     3        /* T3: explicit frame number (16-bit) */
#define OMF_FIXUPP_TARGET_NODISP       4        /* mask bit: when set, displacement=0 */

/* FRAME method constants.  §3.2 FRAME methods (D=1).  */
#define OMF_FIXUPP_FRAME_SEGDEF        0        /* F0: SEGDEF index */
#define OMF_FIXUPP_FRAME_GRPDEF        1        /* F1: GRPDEF index */
#define OMF_FIXUPP_FRAME_EXTDEF        2        /* F2: EXTDEF index */
#define OMF_FIXUPP_FRAME_EXPLICIT      3        /* F3: explicit frame — invalid/unsupported */
#define OMF_FIXUPP_FRAME_LEIDATA       4        /* F4: frame = preceding LEDATA's segment */
#define OMF_FIXUPP_FRAME_TARGET        5        /* F5: frame = TARGET's segment/group/external */

/* §4.2: Locat field byte0 bit assignments.  Byte0 carries the HIGH-order bits.  */
#define OMF_FIXUP_SEGREL               0x40     /* bit 6: M (1=segment-relative, 0=self-relative) */
#define OMF_FIXUP_LOCATION_MASK        0x3c     /* bits 5-2: Location (4 bits) */
#define OMF_FIXUP_LOCATION_SHIFT       2

/* §4.3: Fix Data byte bit masks.  */
#define OMF_FIX_DATA_FRAME_THREAD      0x80     /* bit 7: F (1=FRAME from thread) */
#define OMF_FIX_DATA_FRAME_MASK        0x70     /* bits 6-4: Frame (3 bits: thread# or method) */
#define OMF_FIX_DATA_FRAME_SHIFT       4
#define OMF_FIX_DATA_TARGET_THREAD     0x08     /* bit 3: T (1=TARGET from thread) */
#define OMF_FIX_DATA_P_MASK            0x04     /* bit 2: P (0=displacement present) */
#define OMF_FIX_DATA_TARGT_MASK        0x03     /* bits 1-0: Targt (thread# or low method bits) */

/* Combined explicit target method mask (P + Targt = 3 bits).  */
#define OMF_FIX_DATA_TARGET_METHOD_MASK \
  (OMF_FIX_DATA_TARGT_MASK | OMF_FIX_DATA_P_MASK)

/* §3.1: THREAD subrecord first byte bit masks.
   Layout: 0 D MMM TT  (1+1+3+2 bits; bit 2 is unused/reserved).  */
#define OMF_FIXUP_THREAD_DATA_D_FIELD_MASK    0x40     /* bit 6: D (1=FRAME, 0=TARGET) */
#define OMF_FIXUP_THREAD_DATA_D_FIELD_SHIFT   6
#define OMF_FIXUP_THREAD_DATA_METHOD_MASK     0x38     /* bits 5-3: Method (3 bits) */
#define OMF_FIXUP_THREAD_DATA_METHOD_SHIFT    3
#define OMF_FIXUP_THREAD_DATA_THREAD_NUMBER   0x3      /* bits 1-0: Thred (2 bits, 0-3) */

/* §6.4 item 5: Borland COMDAT uses segment indices 0x4000+ that are
   synthesized on-the-fly from COMDEF entries.  */
#define OMF_COMDAT_SEGIDX_BASE         0x4000

#define OMF_GRPDEF_NONE                0
#define OMF_GRPDEF_COMPONENT_SEGMENT   0xff
#define OMF_GRPDEF_COMPONENT_EXTERNAL  0xfe
#define OMF_GRPDEF_COMPONENT_NAMES     0xfd
#define OMF_GRPDEF_COMPONENT_LTL       0xfb
#define OMF_GRPDEF_COMPONENT_ABSOLUTE  0xfa

/* Some (few) record types have fixed minimum lengths. */
#define OMF_RECORD_HEADER              3
#define OMF_RECORD_HEADER_COMENT       2
#define OMF_RECORD_HEADER_MODEND       1

#define OMF_INDEX_LOWMASK              0x7f
#define OMF_INDEX_2BYTES               0x80

#define OMF_MSDOS_DATE_YEAR_WIDTH      7
#define OMF_MSDOS_DATE_YEAR_SHIFT      9
#define OMF_MSDOS_DATE_MONTH_WIDTH     4
#define OMF_MSDOS_DATE_MONTH_SHIFT     5
#define OMF_MSDOS_DATE_DAY_WIDTH       5
#define OMF_MSDOS_DATE_DAY_SHIFT       0
#define OMF_MSDOS_TIME_HOUR_WIDTH      5
#define OMF_MSDOS_TIME_HOUR_SHIFT      11
#define OMF_MSDOS_TIME_MINUTE_WIDTH    6
#define OMF_MSDOS_TIME_MINUTE_SHIFT    5
#define OMF_MSDOS_TIME_2SECOND_WIDTH   5
#define OMF_MSDOS_TIME_2SECOND_SHIFT   0

#define OMF_COMDEF_DATA_SEG_TYPE_FAR   0x61
#define OMF_COMDEF_DATA_SEG_TYPE_NEAR  0x62
#define OMF_COMDEF_DATA_TYPE_MIN_BORLAND 0x01
#define OMF_COMDEF_DATA_TYPE_MAX_BORLAND 0x5F

#define OMF_COMDEF_LENGTH_PREFIX_16BIT 0x81
#define OMF_COMDEF_LENGTH_PREFIX_24BIT 0x84
#define OMF_COMDEF_LENGTH_PREFIX_32BIT 0x88

#define W2M(x) ((1 << (x)) - 1)

struct i386omf_symbol;

enum i386omf_offset_size
{
  I386OMF_OFFSET_SIZE_16,
  I386OMF_OFFSET_SIZE_32,
};

struct counted_string
{
  bfd_size_type len;
  char* data;
};

struct i386omf_segment
{
  struct bfd_section* asect;
  struct strtab* relocs;
  struct strtab* pubdef;
  int combination;
  int name_index;
  int class_index;
  int overlay_index;
  bfd_vma last_data_offset;     /* §7: section-relative offset of most recent LE/LIDATA record */
  bool big;                     /* B bit (ACBP bit 1): segment uses maximum length (64KB or 4GB) */
  bool use32;                   /* P bit (ACBP bit 0): 1 = Use32, 0 = Use16 */
};

struct i386omf_group_entry
{
  enum
  {
    GRPDEF_ENTRY_SEGDEF = 0xff,
  } type;
  union
  {
    int segdef;
  } u;
};

struct i386omf_group
{
  int name_index;
  struct strtab* entries;
  struct strtab* pubdef;
  struct i386omf_symbol* symbol;
};

struct i386omf_symbol
{
  asymbol base;
  struct counted_string name;
  int type_index;
  struct i386omf_segment* seg;
  struct i386omf_group* group;
};

/* §3.4: Per-thread-slot state (4 FRAME + 4 TARGET threads).
   Defined by THREAD subrecords; consumed by FIXUP subrecords.
   Thread persistence: once defined, a thread remains valid
   across subsequent FIXUPP records until explicitly redefined.  */
struct i386_fixup_thread
{
  int index;           /* SEGDEF/GRPDEF/EXTDEF index, or 0 for method 3 */
  int thread_number;   /* slot number 0-3 (duplicated from array index) */
  bool is_frame;       /* true=FRAME thread, false=TARGET thread */
  int method;          /* 0-6 (full 3-bit method from THREAD subrecord) */
};

struct i386omf_start_addr
{
  bool has_start;
  int frame_method;
  int frame_idx;
  int target_method;
  int target_idx;
  bfd_vma displacement;
};

struct i386omf_obj_data
{
  bfd_byte* image;
  char* translator;
  struct counted_string module_name;
  bool is_main_module;
  bool has_start_address;
  struct i386omf_start_addr start_addr;
  struct strtab* lnames;
  struct strtab* segdef;
  struct strtab* grpdef;
  struct strtab* externs;
  struct strtab* abs_pubdef;
  struct strtab* dependencies;
  struct i386omf_segment* last_leidata;        /* §1: most recent LEDATA/LIDATA/COMDAT for FIXUPP */
  struct i386omf_segment* last_comdat_seg;     /* last synthetic COMDAT segment (continuation) */
  int last_comdat_name_idx;                    /* last COMDAT's Public Name Index (-1 = none) */
  struct i386_fixup_thread frame_threads[4];   /* §3: FRAME thread slots 0-3 */
  struct i386_fixup_thread target_threads[4];  /* §3: TARGET thread slots 0-3 */
  bool frame_thread_used[4];                   /* §7 item 2: thread-defined check */
  bool target_thread_used[4];
  struct i386omf_segment **comdat_segments;    /* §6.4: Borland COMDAT segments (sparse indices) */
  int num_comdat_segments;                     /* number of COMDAT segments allocated */
  int max_comdat_segments;                     /* capacity of comdat_segments array */
};

struct i386omf_relent
{
  arelent base;
  asymbol* symbol;
};

enum reloc_type
{
  R_I386OMF_LO8,          /* 0 */
  R_I386OMF_OFF16,        /* 1 */
  R_I386OMF_SEG,          /* 2 */
  R_I386OMF_FAR16,        /* 3 */
  R_I386OMF_HI8,          /* 4 */
  R_I386OMF_OFF16_LOADER, /* 5; PharLap: OFF32 */
  R_I386OMF_RESERVED_6,   /* 6; PharLap: FAR32 */
  R_I386OMF_RESERVED_7,   /* 7 */
  R_I386OMF_RESERVED_8,   /* 8 */
  R_I386OMF_OFF32,        /* 9 */
  R_I386OMF_RESERVED_10,  /* 10 */
  R_I386OMF_FAR32,        /* 11 */
  R_I386OMF_RESERVED_12,  /* 12 */
  R_I386OMF_OFF32_LOADER, /* 13 */

  /* Some relocs to support other-than-target frames. */
  R_I386OMF_WRT_FRAME,
};

struct i386omf_borland_dependency
{
  struct counted_string filename;
  int time;
  int date;
};

static const char* const thread_method[8]
    = { "Segment Index + Displacement (SEGDEF)",
        "Group Index + Displacement (GRPDEF)",
        "External Index + Displacement (EXTDEF)",
        "Frame number",
        "Segment Index (SEGDEF)",
        "Group Index (GRPDEF)",
        "External Index (EXTDEF)",
        "Frame number" };

static bfd_reloc_status_type
i386omf_fix_wrt_frame(bfd* abfd, arelent* reloc_entry, asymbol* symbol,
                      void* data, asection* input_section, bfd* output_bfd,
                      char** error_message);

/* Look up a segment by SEGDEF or COMDAT index.
   COMDAT indices (0x4000+N) are stored in a separate sparse array.  */
static struct i386omf_segment*
i386omf_find_segment (struct i386omf_obj_data *tdata, int segidx)
{
  struct i386omf_segment *seg;

  seg = strtab_lookup (tdata->segdef, segidx);
  if (seg != NULL)
    return seg;

  if (segidx >= OMF_COMDAT_SEGIDX_BASE)
    {
      int ci = segidx - OMF_COMDAT_SEGIDX_BASE - 1;
      if (ci >= 0 && ci < tdata->num_comdat_segments)
        return tdata->comdat_segments[ci];
    }

  return NULL;
}

/* Create a COMDAT-synthesized segment.
   The segment index is implicit from the order of creation (starts
   at OMF_COMDAT_SEGIDX_BASE + 1).  Returns the new segment, or NULL
   on error.  The segment is NOT added to tdata->segdef (strtab is
   dense); instead it goes into the sparse comdat_segments array.  */
static struct i386omf_segment*
i386omf_create_comdat_segment (bfd *abfd)
{
  struct i386omf_obj_data *tdata = abfd->tdata.any;
  struct i386omf_segment *seg;
  struct i386omf_symbol *seg_sym;
  char buf[64];
  int ci;

  seg = bfd_zalloc (abfd, sizeof (*seg));
  if (seg == NULL)
    return NULL;
  seg->combination = 2;
  seg->name_index = 0;
  seg->class_index = 0;
  seg->overlay_index = 0;
  seg->relocs = strtab_new (abfd);
  if (seg->relocs == NULL)
    return NULL;
  seg->pubdef = strtab_new (abfd);
  if (seg->pubdef == NULL)
    return NULL;

  ci = tdata->num_comdat_segments;
  snprintf (buf, sizeof buf, "COMDAT_%d",
            OMF_COMDAT_SEGIDX_BASE + 1 + ci);
  {
    char *sname = bfd_alloc (abfd, strlen (buf) + 1);
    if (sname == NULL)
      return NULL;
    strcpy (sname, buf);
    seg->asect = bfd_make_section_anyway (abfd, sname);
    if (seg->asect == NULL)
      return NULL;
  }

  seg_sym = (struct i386omf_symbol *) seg->asect->symbol;
  seg_sym->name.len = strlen (buf);
  seg_sym->name.data = bfd_alloc (abfd, seg_sym->name.len + 1);
  if (seg_sym->name.data == NULL)
    return NULL;
  strcpy (seg_sym->name.data, buf);

  seg->asect->used_by_bfd = seg;
  seg->asect->flags |= (SEC_CODE | SEC_ALLOC);

  /* Grow the sparse array.  */
  if (ci >= tdata->max_comdat_segments)
    {
      int newmax = ci + 8;
      struct i386omf_segment **newarr;
      size_t amt;

      if (_bfd_mul_overflow (newmax, sizeof (*newarr), &amt))
        {
          bfd_set_error (bfd_error_file_too_big);
          return NULL;
        }
      newarr = bfd_realloc_or_free (tdata->comdat_segments, amt);
      if (newarr == NULL)
        return NULL;
      memset (newarr + tdata->max_comdat_segments, 0,
              (newmax - tdata->max_comdat_segments) * sizeof (*newarr));
      tdata->comdat_segments = newarr;
      tdata->max_comdat_segments = newmax;
    }

  tdata->comdat_segments[ci] = seg;
  tdata->num_comdat_segments = ci + 1;
  return seg;
}

/**
 * FIXUPP (0x9C / 0x9D) record handler — see fixupp_record_spec.md.
 *
 * Record type 9DH is new for LINK386; it has a Target Displacement field
 * of 32 bits rather than 16 bits, and the Location field of the Locat
 * word has been extended to 4 bits (using the previously unused higher
 * order S bit) to allow new LOCATION values of 9, 11, and 13.
 *
 * §4.2: Location values for howto selection.  Note vendor conflicts
 * at values 5,6,9,11 between MS/IBM and PharLap conventions (§8).  */
/* There are no HOWTO entries for far pointer relocs, as we expand them to
   a tuple of SEG and OFF relocs.  Neither does gas generate FAR relocs.  */
reloc_howto_type howto_table_i386omf_pcrel[] = {
 /*  type                       rs  size    bsz     pcrel   bp  ovrf                         sf  name        part_inpl  readmask    setmask     pcdone */
 HOWTO(R_I386OMF_LO8,           0,  0,      8,      true,   0,  complain_overflow_signed,    0,  "PC8LO",    false,     0xff,       0xff,       false),     // 0 - Low-order byte
 HOWTO(R_I386OMF_OFF16,         0,  1,      16,     true,   0,  complain_overflow_bitfield,  0,  "OFFPC16",  false,     0xffff,     0xffff,     false),     // 1 - Offset
 EMPTY_HOWTO(R_I386OMF_SEG), /* PC-relative SEG relocs don't make sense. */                                                                                 // 2 - Segment
 EMPTY_HOWTO(R_I386OMF_FAR16),                                                                                                                              // 3 - Pointer (sgment:offset)
 HOWTO(R_I386OMF_HI8,           0,  0,      8,      true,   0,  complain_overflow_dont,      0,  "PC8HI",    false,     0xff,       0xff,       false),     // 4 - high-order byte (not regognized by Link
 HOWTO(R_I386OMF_OFF16_LOADER,  0,  1,      16,     true,   0,  complain_overflow_bitfield,  0,  "OFFPC16L", false,     0xffff,     0xffff,     false),     // 5 - Loader-resolved offset (treated as loc=1 by the linker)
 EMPTY_HOWTO(R_I386OMF_RESERVED_6),
 EMPTY_HOWTO(R_I386OMF_RESERVED_7),
 EMPTY_HOWTO(R_I386OMF_RESERVED_8),
 HOWTO(R_I386OMF_OFF32,         0,  2,      32,     true,   0,  complain_overflow_bitfield,  0,  "OFFPC32",  false,     0xffffffff, 0xffffffff, false),     // 9 -
 EMPTY_HOWTO(R_I386OMF_RESERVED_10),
 EMPTY_HOWTO(R_I386OMF_FAR32),
 EMPTY_HOWTO(R_I386OMF_RESERVED_12),
 HOWTO(R_I386OMF_OFF32_LOADER,  0,  2,      32,     true,   0,  complain_overflow_bitfield,  0,  "OFFPC32L", false,     0xffffffff, 0xffffffff, false),     // 13
};

reloc_howto_type howto_table_i386omf_segrel[] = {
 /*    type                      rs size    bitsz   pcrel   bp  ovrf                        sfunc  name      part_inpl  smask       dmask       pcreloffset */
 HOWTO(R_I386OMF_LO8,            0, 0,      8,      false,  0,  complain_overflow_signed,    0,  "8LO",      false,     0xff,       0xff,       false), /* XXX Which overflow type? */
 HOWTO(R_I386OMF_OFF16,          0, 1,      16,     false,  0,  complain_overflow_bitfield,  0,  "OFF16",    false,     0xffff,     0xffff,     false),
 HOWTO(R_I386OMF_SEG,            0, 1,      16,     false,  0,  complain_overflow_unsigned,  0,  "SEG",      false,     0xffff,     0xffff,     false),
 EMPTY_HOWTO(R_I386OMF_FAR16),
 HOWTO(R_I386OMF_HI8,            0, 0,      8,      false,  0,  complain_overflow_dont,      0,  "8HI",      false,     0xff,       0xff,       false), /* XXX Which overflow type? */
 HOWTO(R_I386OMF_OFF16_LOADER,   0, 1,      16,     false,  0,  complain_overflow_bitfield,  0,  "OFF16L",   false,     0xffff,     0xffff,     false),
 EMPTY_HOWTO(R_I386OMF_RESERVED_6),
 EMPTY_HOWTO(R_I386OMF_RESERVED_7),
 EMPTY_HOWTO(R_I386OMF_RESERVED_8),
 HOWTO(R_I386OMF_OFF32,          0, 2,      32,     false, 0,  complain_overflow_bitfield,   0,  "OFF32",    false,     0xffffffff, 0xffffffff, false),
 EMPTY_HOWTO(R_I386OMF_RESERVED_10),
 EMPTY_HOWTO(R_I386OMF_FAR32),
 EMPTY_HOWTO(R_I386OMF_RESERVED_12),
 HOWTO(R_I386OMF_OFF32_LOADER,   0, 2,      32,     false, 0,  complain_overflow_bitfield,   0,  "OFF32L",   false,     0xffffffff, 0xffffffff, false),
};
/*
 * OMF supports relocations that are relative to things other than just the
 * segment to which the reloc symbol belongs.  To represent these in BFD,
 * use two consecutive relocs at the same address:
 *   lea 0x0,%ax
 *     OFF16 foo
 *     WRTSEG bar
 * This representation is more familiar to assembly-language programmers,
 * compared with the alternative of a more stateless expression-like set of
 * relocations.  This nearly direct representation of OMF reloc info into
 * BFD relocs also makes it easier to convert back from BFD to OMF format.
 *
 * A possible downside of this representation is that it requires the linker
 * to remember previous relocations in order to make use of WRTSEG, but
 * presumably any linker reading OMF input objects would have to keep track
 * of which relocs act on a particular address anyway.
 *
 * To generate weird WRTSEG relocs with nasm:
 *   extern foo
 *   extern bar
 *   lea ax, foo wrt seg bar
 *
 * It *is* possible to represent absolute-frame-relative relocs in MZ EXE
 * relocations: just add the PSP frame 65520 times!  (It may break the DOS
 * EXE loader though.)
 */
reloc_howto_type howto_wrt_segdef
    = HOWTO(R_I386OMF_WRT_FRAME, 0, 3, 16, false, 0, complain_overflow_bitfield,
            &i386omf_fix_wrt_frame, "WRTSEG", false, 0xffff, 0xffff, false);

/*
INTERNAL_FUNCTION
    hexdump

SYNOPSIS
    static void hexdump (bfd_byte const *p, bfd_size_type len);

DESCRIPTION
    Print a hex dump of a memory region for debugging.
    Formats up to 333 bytes from the given pointer as a space-separated
    string of two-digit hex values, and prints it using _bfd_error_handler.
    If the region is too large, the output is truncated and a warning is printed.

    @param p   Pointer to the start of the memory region.
    @param len Number of bytes to print.
*/
static void
hexdump(bfd_byte const* p, bfd_size_type len)
{
  bfd_size_type i;
  char* s;
  size_t amt;

  /* XXX - 1000 is the size of _bfd_default_error_handler()'s buffer. */
  if (len > 1000 / 3)
  {
    if (omf_debug) fprintf(stderr, "(truncated hexdump)\n");
    len = 1000 / 3;
  }

  if (_bfd_mul_overflow(len + 1, 3, &amt))
  {
    bfd_set_error(bfd_error_file_too_big);
  }

  s = bfd_malloc(amt); /* +1 for NUL. */
  if (s == NULL)
    return;
  for (i = 0; i < len; i++)
  {
    sprintf(s + i * 3, " %02x", (unsigned int) p[i]);
  }
  if (omf_debug) fprintf(stderr, "%s", s);
  free(s);
}

/*
    i386omf_read_index

SYNOPSIS
    static bool i386omf_read_index(bfd *abfd, int *idx, bfd_byte const **p, bfd_size_type *reclen);

DESCRIPTION
    Read an OMF variable-length index from the input buffer.
    OMF indices are either one or two bytes long. This function reads the index,
    updates the pointer and remaining length, and stores the result in *idx.
    On error (e.g., truncated input), it reports an error and returns false.

    @param abfd   The BFD file handle.
    @param idx    Pointer to store the decoded index value.
    @param p      Pointer to the current buffer pointer (updated on success).
    @param reclen Pointer to the remaining record length (updated on success).
    @return       true on success, false on error.
*/
static bool
i386omf_read_index(bfd* abfd, int* idx, bfd_byte const** p,
                   bfd_size_type* reclen)
{
  struct i386omf_obj_data* tdata = abfd->tdata.any;
  int v;

  if (*reclen < 1)
  {
    (*_bfd_error_handler)("Index truncated at 0x%lx.", *p - tdata->image);
    bfd_set_error(bfd_error_wrong_format);
    return false;
  }

  v = *(*p)++;
  (*reclen)--;
  if (v & OMF_INDEX_2BYTES)
  {
    if (*reclen < 1)
    {
      (*_bfd_error_handler)("Index truncated at 0x%lx.", *p - tdata->image);
      bfd_set_error(bfd_error_wrong_format);
      return false;
    }
    v = (v & OMF_INDEX_LOWMASK) * 256 + *(*p)++;
    (*reclen)--;
  }

  *idx = v;

  return true;
}

/*
    i386omf_read_offset

SYNOPSIS
    static bool i386omf_read_offset(bfd *abfd, bfd_vma *offset, bfd_byte const **p,
                                   bfd_size_type *reclen, enum i386omf_offset_size sz);

DESCRIPTION
    Read an OMF offset value from the input buffer.
    The offset can be either 16 or 32 bits, depending on the 'sz' parameter.
    This function reads the offset, updates the pointer and remaining length,
    and stores the result in *offset. On error (e.g., truncated input),
    it reports an error and returns false.

    @param abfd   The BFD file handle.
    @param offset Pointer to store the decoded offset value.
    @param p      Pointer to the current buffer pointer (updated on success).
    @param reclen Pointer to the remaining record length (updated on success).
    @param sz     The size of the offset (16 or 32 bits).
    @return       true on success, false on error.
*/
static bool
i386omf_read_offset(bfd* abfd, bfd_vma* offset, bfd_byte const** p,
                    bfd_size_type* reclen, enum i386omf_offset_size sz)
{
  struct i386omf_obj_data* tdata = abfd->tdata.any;
  bfd_size_type offset_len = 0;

  switch (sz)
  {
    case I386OMF_OFFSET_SIZE_16:
      offset_len = 2;
      break;
    case I386OMF_OFFSET_SIZE_32:
      offset_len = 4;
      break;
  }

  /* TODO: Handle 32-bit OMF records. */
  if (*reclen < offset_len)
  {
    (*_bfd_error_handler)("Offset truncated at 0x%lx.", *p - tdata->image);
    bfd_set_error(bfd_error_wrong_format);
    return false;
  }

  if (offset)
  {
    switch (sz)
    {
      case I386OMF_OFFSET_SIZE_16:
        *offset = bfd_get_16(abfd, *p);
        break;
      case I386OMF_OFFSET_SIZE_32:
        *offset = bfd_get_32(abfd, *p);
        break;
    }
  }

  *p += offset_len;
  *reclen -= offset_len;

  return true;
}

/*
    i386omf_read_string

SYNOPSIS
    static bfd_size_type i386omf_read_string(bfd* abfd, struct counted_string* s, bfd_byte const* p, bfd_size_type reclen);

DESCRIPTION
    Reads a counted string from the input buffer. The first byte is the length,
    followed by the string data. Allocates memory for the string and stores it in 's'.
    Returns the total number of bytes consumed (length + 1), or 0 on error.

    @param abfd   The BFD file handle.
    @param s      Pointer to a counted_string struct to fill.
    @param p      Pointer to the current buffer position.
    @param reclen Remaining record length.
    @return       Number of bytes consumed, or 0 on error.
*/
static bfd_size_type
i386omf_read_string(bfd* abfd, struct counted_string* s, bfd_byte const* p,
                    bfd_size_type reclen)
{
  struct i386omf_obj_data* tdata = abfd->tdata.any;
  bfd_size_type slen = *p;

  if (slen + 1 > reclen)
  {
    (*_bfd_error_handler)("Counted string at 0x%lx overflows its record.",
                          p - tdata->image);
    bfd_set_error(bfd_error_wrong_format);
    return 0;
  }

  s->len = slen;
  s->data = bfd_alloc(abfd, slen + 1);
  if (s->data == NULL)
    return 0;
  memcpy(s->data, p + 1, slen);
  s->data[slen] = 0;

  return (slen + 1);
}

/*
    i386omf_lookup_string

SYNOPSIS
    static char const* i386omf_lookup_string(struct strtab* tab, int i, char const* def);

DESCRIPTION
    Looks up a string by index in a string table. If the index is 0, returns the default string.
    If the index is invalid or the string is missing, reports an error and returns NULL.

    @param tab    Pointer to the string table.
    @param i      Index to look up.
    @param def    Default string to return if index is 0.
    @return       Pointer to the string, or NULL on error.
*/
static char const*
i386omf_lookup_string(struct strtab* tab, int i, char const* def)
{
  struct counted_string* s;

  if (i == 0)
    return def;

  s = strtab_lookup(tab, i);

  if (s && s->data)
    return s->data;

  (*_bfd_error_handler)("Bad name index requested from string table at %p",
                        tab);
  bfd_set_error(bfd_error_wrong_format);

  return NULL;
}

/* Create a binary object.  Invoked via bfd_set_format.  */
/*

    binary_mkobject

SYNOPSIS
    static bool binary_mkobject(bfd* abfd);

DESCRIPTION
    Initializes a binary object. This function is a stub and always returns true.

    @param abfd   The BFD file handle.
    @return       true.
*/
static bool
binary_mkobject(bfd* abfd ATTRIBUTE_UNUSED)
{
  return true;
}

/*
    i386omf_read_coment

SYNOPSIS
    static bool i386omf_read_coment(bfd* abfd, bfd_byte const* p, bfd_size_type reclen);

DESCRIPTION
    Reads and processes an OMF COMENT record. Handles various comment classes,
    including dependencies and translator strings. Reports errors for unknown or malformed records.

    @param abfd   The BFD file handle.
    @param p      Pointer to the record data.
    @param reclen Length of the record data.
    @return       true on success, false on error.
*/
static bool
i386omf_read_coment(bfd* abfd, bfd_byte const* p, bfd_size_type reclen)
{
  struct i386omf_obj_data* tdata = abfd->tdata.any;
  int comment_type, comment_class;

  if (reclen < OMF_RECORD_HEADER_COMENT)
  {
    (*_bfd_error_handler)("Truncated COMENT record.");
    bfd_set_error(bfd_error_wrong_format);
    return false;
  }

  comment_type = bfd_get_8(abfd, p + 0);
  comment_class = bfd_get_8(abfd, p + 1);
  p += OMF_RECORD_HEADER_COMENT;
  reclen -= OMF_RECORD_HEADER_COMENT;

  switch (comment_class)
  {
    case OMF_COMENT_TRANSLATOR:
      if (tdata->translator)
        if (omf_debug) fprintf(stderr, "Translator already set to %s\n",
                                tdata->translator);
      if (reclen && !ISPRINT(bfd_get_8(abfd, p))
          && bfd_get_8(abfd, p) == reclen - 1)
      {
        /* Looks like a length+data style string!
           XXX The OMF specification wants a string whose length is
           implicit in reclen, but NASM 0.92 and above seem deliberately
           to generate a length+data string.  If there appears to be a
           length byte that happens to match the reclen-derived length,
           omit it from the translator string.  Remove ISPRINT if any
           obscure tools turn up whose translator string length byte
           happens to encode a printable ASCII character.  */
        p++;
        reclen--;
      }
      tdata->translator = bfd_alloc(abfd, reclen + 1);
      strncpy(tdata->translator, (char const*) p, reclen);
      tdata->translator[reclen] = 0;
      break;
    case OMF_COMENT_PASS_SEPARATOR: /* We don't care about it. */
      break;
    case OMF_COMENT_SYMBOL_TYPE_EXTDEF:
    case OMF_COMENT_SYMBOL_TYPE_PUBDEF:
    case OMF_COMENT_STRUCT_MEMBER:
    case OMF_COMENT_TYPDEF:
    case OMF_COMENT_ENUM_MEMBER:
    case OMF_COMENT_SCOPE_BEGIN:
    case OMF_COMENT_LOCALS:
    case OMF_COMENT_SCOPE_END:
    case OMF_COMENT_SOURCE_FILE:
      /* http://webster.cs.ucr.edu/Page_TechDocs/boa.txt has record formats. */
      break;
    case OMF_COMENT_DEPENDENCIES:
      while (reclen)
      {
        struct i386omf_borland_dependency* dep;
        bfd_size_type slen;

        if (reclen < 5)
        {
          _bfd_error_handler(_("Truncated Borland dependency list at 0x%lx"),
                             (unsigned long) (p - tdata->image));
          break;
        }

        dep = bfd_alloc(abfd, sizeof(*dep));
        if (dep == NULL)
          return false;

        /* Some sort of timestamp. */
        dep->time = bfd_get_16(abfd, p + 0);
        dep->date = bfd_get_16(abfd, p + 2);
        p += 4;
        reclen -= 4;

        /* Source filename. */
        slen = i386omf_read_string(abfd, &dep->filename, p, reclen);
        if (slen < 1)
          break;

        strtab_add(tdata->dependencies, dep);
        p += slen;
        reclen -= slen;
      }
      break;
    case OMF_COMENT_COMPILE_PARAMETERS:
    case OMF_COMENT_MATCHED_TYPE_EXTDEF:
    case OMF_COMENT_MATCHED_TYPE_PUBDEF:
    case OMF_COMENT_CLASSDEF:
    case OMF_COMENT_COVERAGE_OFFSET:
    case OMF_COMENT_LARGE_SCOPE_BEGIN:
    case OMF_COMENT_LARGE_LOCALS:
    case OMF_COMENT_LARGE_SCOPE_END:
    case OMF_COMENT_MEMBER_FUNCTION:
    case OMF_COMENT_DEBUG_VERSION:
    case OMF_COMENT_OPT_FLAGS:
      /* http://webster.cs.ucr.edu/Page_TechDocs/boa.txt has record formats. */
      break;
    case OMF_COMENT_EASY_OMF:
    case OMF_COMENT_WAT_PROC_MODEL:
    case OMF_COMENT_LINKER_DIRECTIVE:
    case OMF_COMENT_LINKER_DIRECTIVE2:
    case OMF_COMENT_DLL_ENTRY:
    case OMF_COMENT_WKEXT:
    case OMF_COMENT_LZEXT:
    case OMF_COMENT_DEFAULT_LIBRARY:
    case OMF_COMENT_MEMORY_MODEL:
    case OMF_COMENT_NEWEXT:
      break;
    default:
      _bfd_error_handler(
          "Unknown record COMENT type: 0x%02x class: 0x%02x at 0x%04lx",
          comment_type, comment_class, (unsigned long) (p - tdata->image - 1));
      bfd_set_error(bfd_error_wrong_format);
      return false;
  }

  return true;
}

/*
    i386omf_read_modend

SYNOPSIS
    static bool i386omf_read_modend(bfd* abfd, bfd_byte const* p, bfd_size_type reclen,
                                     int is_32bit);

DESCRIPTION
    Reads and processes an OMF MODEND record (0x8A/0x8B), which marks the end
    of a module.  Parses the Module Type byte per §3 and, when the Strt bit is
    set, consumes the Start Address subfield (§4) using the same Fix Data /
    Frame Datum / Target Datum / Target Displacement encoding as FIXUPP.

    @param abfd    The BFD file handle.
    @param p       Pointer to the record data.
    @param reclen  Length of the record data (excluding checksum).
    @param is_32bit Non-zero for 0x8B (32-bit Target Displacement).
    @return        true on success, false on error.
*/
static bool
i386omf_read_modend(bfd* abfd, bfd_byte const* p, bfd_size_type reclen,
                     int is_32bit)
{
  struct i386omf_obj_data* tdata = abfd->tdata.any;
  int module_type;
  bool has_start;

  if (reclen < OMF_RECORD_HEADER_MODEND)
  {
    (*_bfd_error_handler)("Truncated MODEND record.");
    bfd_set_error(bfd_error_wrong_format);
    return false;
  }

  /* §3: Module Type byte — Main(bit7), Strt(bit6), SegmentBit(bit5), X(bit4) */
  module_type = bfd_get_8(abfd, p);
  p++;
  reclen--;

  tdata->is_main_module = (module_type >> 7) & 1;
  has_start = (module_type >> 6) & 1;
  tdata->has_start_address = has_start;
  tdata->start_addr.has_start = false;

  if (module_type & ~(OMF_MODEND_MAIN_MODULE | OMF_MODEND_START_ADDRESS))
  {
    if (omf_debug) fprintf(stderr, "MODEND Module Type 0x%02x has non-standard bits set.\n",
                            module_type);
  }

  /* §MODEND-specific rule: if START (bit 6) is set, the relocatable bit
     (bit 0) must also be set.  LINK does not support an absolute
     (non-relocatable) start address.  */
  if (has_start && (module_type & 0x01) == 0)
    {
      (*_bfd_error_handler)("MODEND start address is not relocatable (bit 0 must be set).");
      bfd_set_error(bfd_error_wrong_format);
      return false;
    }

  if (!has_start)
    return true;

  /* --- Start Address subfield (§4) --- */

  /* End Data (Fix Data) byte — same layout as FIXUPP Fix Data §4.3:
     F(1) Frame(3) T(1) P(1) Targt(2) */
  int fixdata;

  if (reclen < 1)
  {
    (*_bfd_error_handler)("MODEND start address subfield truncated at End Data.");
    bfd_set_error(bfd_error_wrong_format);
    return false;
  }
  fixdata = bfd_get_8(abfd, p);
  p++;
  reclen--;

  /* §4.1: P must be 0 — Target Displacement is always present */
  if (fixdata & OMF_FIX_DATA_P_MASK)
  {
    (*_bfd_error_handler)("MODEND start address has P=1 (must be 0).");
    bfd_set_error(bfd_error_wrong_format);
    return false;
  }

  /* --- FRAME (§4.4) --- */
  if (fixdata & OMF_FIX_DATA_FRAME_THREAD)
  {
    /* F=1: FRAME from thread slot */
    int frame_tnum = (fixdata & OMF_FIX_DATA_FRAME_MASK)
                     >> OMF_FIX_DATA_FRAME_SHIFT & 3;
    if (frame_tnum > 3 || !tdata->frame_thread_used[frame_tnum])
    {
      (*_bfd_error_handler)("MODEND start address references undefined FRAME thread %d",
                             frame_tnum);
      bfd_set_error(bfd_error_wrong_format);
      return false;
    }
    tdata->start_addr.frame_method = tdata->frame_threads[frame_tnum].method;
    tdata->start_addr.frame_idx = tdata->frame_threads[frame_tnum].index;
  }
  else
  {
    int frame_method = (fixdata & OMF_FIX_DATA_FRAME_MASK)
                        >> OMF_FIX_DATA_FRAME_SHIFT;
    tdata->start_addr.frame_method = frame_method;

    switch (frame_method)
    {
      case OMF_FIXUPP_FRAME_SEGDEF:   /* F0 */
      case OMF_FIXUPP_FRAME_GRPDEF:   /* F1 */
      case OMF_FIXUPP_FRAME_EXTDEF:   /* F2 */
        if (!i386omf_read_index(abfd, &tdata->start_addr.frame_idx,
                                 &p, &reclen))
          return false;
        break;
      case OMF_FIXUPP_FRAME_EXPLICIT: /* F3 — invalid */
      case 6:                          /* F6 — invalid */
        (*_bfd_error_handler)("MODEND start address invalid FRAME method %d.",
                               frame_method);
        bfd_set_error(bfd_error_wrong_format);
        return false;
      /* F4 (LEIDATA), F5 (TARGET): no datum to read */
    }
  }

  /* --- TARGET (§4.5) --- */
  if (fixdata & OMF_FIX_DATA_TARGET_THREAD)
  {
    int target_tnum = fixdata & OMF_FIX_DATA_TARGT_MASK;
    if (target_tnum > 3 || !tdata->target_thread_used[target_tnum])
    {
      (*_bfd_error_handler)("MODEND start address references undefined TARGET thread %d",
                             target_tnum);
      bfd_set_error(bfd_error_wrong_format);
      return false;
    }
    /* P is always 0 for MODEND, so effective method = stored low 2 bits */
    tdata->start_addr.target_method = tdata->target_threads[target_tnum].method & 3;
    tdata->start_addr.target_idx = tdata->target_threads[target_tnum].index;
  }
  else
  {
    tdata->start_addr.target_method = fixdata & OMF_FIX_DATA_TARGET_METHOD_MASK;

    switch (tdata->start_addr.target_method & 3)
    {
      case OMF_FIXUPP_TARGET_SEGDEF:  /* T0/T4 */
      case OMF_FIXUPP_TARGET_GRPDEF:  /* T1/T5 */
      case OMF_FIXUPP_TARGET_EXTDEF:  /* T2/T6 */
        if (!i386omf_read_index(abfd, &tdata->start_addr.target_idx,
                                 &p, &reclen))
          return false;
        break;
      case OMF_FIXUPP_TARGET_EXPLICIT: /* T3/T7: explicit 2-byte frame number */
        if (reclen < 2)
        {
          (*_bfd_error_handler)("MODEND start address truncated at TARGET datum.");
          bfd_set_error(bfd_error_wrong_format);
          return false;
        }
        tdata->start_addr.target_idx = bfd_get_16(abfd, p);
        p += 2;
        reclen -= 2;
        break;
    }
  }

  /* --- Target Displacement (§4.6) — always present since P=0 --- */
  if (!i386omf_read_offset(abfd, &tdata->start_addr.displacement,
                            &p, &reclen,
                            is_32bit ? I386OMF_OFFSET_SIZE_32
                                     : I386OMF_OFFSET_SIZE_16))
    return false;

  tdata->start_addr.has_start = true;
  return true;
}

/*
    i386omf_read_linsym

SYNOPSIS
    static bool i386omf_read_linsym(bfd* abfd, bfd_byte const* p, bfd_size_type reclen,
                                     int is_32bit);

DESCRIPTION
    Reads and processes an OMF LINSYM record (0xC4/0xC5), which provides
    source line-number-to-offset mappings for COMDAT symbols.

    Parses: Flags (1 byte), Public Name OMF index (1-2 bytes),
    then (Line Number, Offset) entries repeating until the record body
    is exhausted.  Entry size is 4 bytes for 0xC4 (2+2) and 6 bytes for
    0xC5 (2+4).  Validates that the body after the Public Name index
    forms whole entries.

    @param abfd    The BFD file handle.
    @param p       Pointer to the record data.
    @param reclen  Length of the record data (excluding checksum).
    @param is_32bit Non-zero for 0xC5 (32-bit Line Number Offset).
    @return        true on success, false on error.
*/
static bool
i386omf_read_linsym(bfd* abfd, bfd_byte const* p, bfd_size_type reclen,
                     int is_32bit)
{
  struct i386omf_obj_data *tdata = abfd->tdata.any;
  bfd_size_type entry_size = is_32bit ? 6 : 4;
  bfd_size_type remaining;
  int name_index;

  if (reclen < 2)
  {
    (*_bfd_error_handler)("Truncated LINSYM record.");
    bfd_set_error(bfd_error_wrong_format);
    return false;
  }

  p++; reclen--; /* skip Flags byte — we parse structure without storing */

  if (!i386omf_read_index(abfd, &name_index, &p, &reclen))
    return false;

  /* A subsequent FIXUPP record attaches relocations to the COMDAT segment
     identified by this LINSYM's Public Name Index.  Update last_leidata
     so the FIXUPP handler finds the right segment.
     FIXME: this only matches the most recent COMDAT segment.  A complete
     fix would maintain a name-index-to-segment map for all COMDATs.  */
  if (tdata->last_comdat_seg != NULL
      && tdata->last_comdat_name_idx == name_index)
    tdata->last_leidata = tdata->last_comdat_seg;

  remaining = reclen;
  if (remaining % entry_size != 0)
  {
    (*_bfd_error_handler)("LINSYM record has %llu remaining bytes, "
                           "not a multiple of entry size %llu.",
                           (unsigned long long) remaining,
                           (unsigned long long) entry_size);
    bfd_set_error(bfd_error_wrong_format);
    return false;
  }

  /* Advance through all entries to consume the bytes */
  p += remaining;
  reclen -= remaining;

  return true;
}

/*
    i386omf_read_comdef_length

SYNOPSIS
    static bool i386omf_read_comdef_length(bfd* abfd, bfd_byte const** p,
                                           bfd_size_type* reclen, bfd_vma* value);

DESCRIPTION
    Reads a variable-width COMDEF communal length field per OMF v1.1 §4.4.
    The first byte is a prefix that determines the width:

        lead 0x00-0x80  →  1 byte,  value = lead
        lead 0x81       →  3 bytes, value = LE u16
        lead 0x84       →  4 bytes, value = LE 24-bit
        lead 0x88       →  5 bytes, value = LE u32
        other           →  error

    @param abfd   The BFD file handle.
    @param p      Pointer to current buffer pointer (advanced on success).
    @param reclen Pointer to remaining record length (decremented on success).
    @param value  Pointer to store the decoded value.
    @return       true on success, false on error.
*/
static bool
i386omf_read_comdef_length(bfd* abfd, bfd_byte const** p,
                           bfd_size_type* reclen, bfd_vma* value)
{
  struct i386omf_obj_data* tdata = abfd->tdata.any;

  if (*reclen < 1)
  {
    (*_bfd_error_handler)("COMDEF length truncated at 0x%lx",
                          *p - tdata->image);
    bfd_set_error(bfd_error_wrong_format);
    return false;
  }

  unsigned int lead = bfd_get_8(abfd, *p);

  if (lead <= 0x80)
  {
    *value = lead;
    *p += 1;
    *reclen -= 1;
  }
  else if (lead == OMF_COMDEF_LENGTH_PREFIX_16BIT)
  {
    if (*reclen < 3)
    {
      (*_bfd_error_handler)("COMDEF 16-bit length truncated at 0x%lx",
                            *p - tdata->image);
      bfd_set_error(bfd_error_wrong_format);
      return false;
    }
    *value = bfd_get_16(abfd, *p + 1);
    *p += 3;
    *reclen -= 3;
  }
  else if (lead == OMF_COMDEF_LENGTH_PREFIX_24BIT)
  {
    if (*reclen < 4)
    {
      (*_bfd_error_handler)("COMDEF 24-bit length truncated at 0x%lx",
                            *p - tdata->image);
      bfd_set_error(bfd_error_wrong_format);
      return false;
    }
    *value = bfd_get_8(abfd, *p + 1)
           | (bfd_get_8(abfd, *p + 2) << 8)
           | (bfd_get_8(abfd, *p + 3) << 16);
    *p += 4;
    *reclen -= 4;
  }
  else if (lead == OMF_COMDEF_LENGTH_PREFIX_32BIT)
  {
    if (*reclen < 5)
    {
      (*_bfd_error_handler)("COMDEF 32-bit length truncated at 0x%lx",
                            *p - tdata->image);
      bfd_set_error(bfd_error_wrong_format);
      return false;
    }
    *value = bfd_get_32(abfd, *p + 1);
    *p += 5;
    *reclen -= 5;
  }
  else
  {
    (*_bfd_error_handler)("COMDEF invalid length prefix 0x%02x at 0x%lx",
                          lead, (unsigned long)(*p - tdata->image));
    bfd_set_error(bfd_error_wrong_format);
    return false;
  }

  return true;
}

/*
    i386omf_read_comdef

SYNOPSIS
    static bool i386omf_read_comdef(bfd* abfd, bfd_byte const* p, bfd_size_type reclen);

DESCRIPTION
    Reads and processes an OMF COMDEF record (0xB0), which defines communal
    variables.  Per OMF v1.1 §"B0H COMDEF—Communal Names Definition Record":

      Each entry: [Communal Name] [Type Index] [Data Type] [Communal Length(s)]

    Data Type determines the layout:
      0x61 (FAR)   →  count + element_size (two variable-width fields)
      0x62 (NEAR)  →  total size (one variable-width field)
      0x01-0x5F    →  Borland segment index (no length follows)

    All entries share the external index space with EXTDEF/LEXTDEF/LCOMDEF
    records per §8 "FIXUP ordering".  Symbols are added to tdata->externs.

    @param abfd   The BFD file handle.
    @param p      Pointer to the record data.
    @param reclen Length of the record data.
    @return       true on success, false on error.
*/
static bool
i386omf_read_comdef(bfd* abfd, bfd_byte const* p, bfd_size_type reclen)
{
  struct i386omf_obj_data* tdata = abfd->tdata.any;

  while (reclen)
  {
    struct i386omf_symbol* extdef;
    bfd_size_type slen;
    int data_type;

    extdef = (struct i386omf_symbol*) bfd_make_empty_symbol(abfd);
    if (extdef == NULL)
      return false;
    abfd->flags |= HAS_SYMS;

    /* Communal Name */
    slen = i386omf_read_string(abfd, &extdef->name, p, reclen);
    if (slen < 1)
      return false;
    p += slen;
    reclen -= slen;

    /* Type Index (not inspected by linkers, per spec) */
    if (!i386omf_read_index(abfd, &extdef->type_index, &p, &reclen))
      return false;

    /* Data Type byte */
    if (reclen < 1)
    {
      (*_bfd_error_handler)("COMDEF data type truncated at 0x%lx",
                            p - tdata->image);
      bfd_set_error(bfd_error_wrong_format);
      return false;
    }
    data_type = bfd_get_8(abfd, p);
    p++;
    reclen--;

    extdef->base.name = extdef->name.data;
    extdef->base.flags |= BSF_GLOBAL;
    extdef->seg = NULL;
    extdef->base.section = bfd_com_section_ptr;

    switch (data_type)
    {
      case OMF_COMDEF_DATA_SEG_TYPE_FAR:
      {
        /* FAR: count + element_size, each in variable-width encoding. */
        bfd_vma count, element_size;

        if (!i386omf_read_comdef_length(abfd, &p, &reclen, &count)
            || !i386omf_read_comdef_length(abfd, &p, &reclen, &element_size))
          return false;

        extdef->base.value = count * element_size;
        break;
      }
      case OMF_COMDEF_DATA_SEG_TYPE_NEAR:
      {
        /* NEAR: single total size in variable-width encoding. */
        if (!i386omf_read_comdef_length(abfd, &p, &reclen, &extdef->base.value))
          return false;
        break;
      }
      default:
        if (data_type >= OMF_COMDEF_DATA_TYPE_MIN_BORLAND
            && data_type <= OMF_COMDEF_DATA_TYPE_MAX_BORLAND)
        {
          /* Borland segment index (§9): data_type IS the segment
             index — consumed by the switch dispatch and COMDAT
             segment creation below, NOT stored in base.value.
             The 1-byte field that follows is the communal symbol
             size in bytes.  Pre-create the COMDAT segment so FIXUPP
             can reference it before any LEDATA arrives.  */
          if (reclen < 1)
            {
              (*_bfd_error_handler)(
                  "COMDEF Borland size truncated at 0x%lx",
                  p - tdata->image);
              bfd_set_error(bfd_error_wrong_format);
              return false;
            }
          extdef->base.value = bfd_get_8(abfd, p); /* size only */
          p++;
          reclen--;

          /* Create COMDAT segment for this Borland COMDEF entry.  */
          {
            struct i386omf_segment *cs
              = i386omf_create_comdat_segment (abfd);
            if (cs == NULL)
              return false;
          }
        }
        else
        {
          (*_bfd_error_handler)("COMDEF unknown data type 0x%02x at 0x%lx",
                                data_type, (unsigned long)(p - tdata->image));
          bfd_set_error(bfd_error_wrong_format);
          return false;
        }
        break;
    }

    strtab_add(tdata->externs, extdef);
  }

  return true;
}

/*
    i386omf_read_extdef

SYNOPSIS
    static bool i386omf_read_extdef(bfd* abfd, bfd_byte const* p, bfd_size_type reclen);

DESCRIPTION
    Reads and processes an OMF EXTDEF record, which defines external symbols.
    Adds each symbol to the externs string table. Reports errors for malformed records.

    @param abfd   The BFD file handle.
    @param p      Pointer to the record data.
    @param reclen Length of the record data.
    @return       true on success, false on error.
*/
static bool
i386omf_read_extdef(bfd* abfd, bfd_byte const* p, bfd_size_type reclen)
{
  struct i386omf_obj_data* tdata = abfd->tdata.any;

  while (reclen)
  {
    struct i386omf_symbol* extdef;
    bfd_size_type slen;

    extdef = (struct i386omf_symbol*) bfd_make_empty_symbol(abfd);
    if (extdef == NULL)
      return false;
    abfd->flags |= HAS_SYMS;

    slen = i386omf_read_string(abfd, &extdef->name, p, reclen);
    if (slen < 1)
      return false;
    p += slen;
    reclen -= slen;

    if (!i386omf_read_index(abfd, &extdef->type_index, &p, &reclen))
      return false;

    extdef->base.name = extdef->name.data;
    /* Maybe? extdef->base.flags |= BSF_WEAK; */
    extdef->base.value = 0;
    extdef->seg = NULL;
    extdef->base.section = bfd_und_section_ptr;

    strtab_add(tdata->externs, extdef);
  }

  return true;
}

/*

    i386omf_read_pubdef

SYNOPSIS
    static bool i386omf_read_pubdef(bfd* abfd, bfd_byte const* p, bfd_size_type reclen, int is32);

DESCRIPTION
    Reads and processes an OMF PUBDEF record, which defines public (exported) symbols.
    Handles both absolute and segment-relative symbols. Adds symbols to the appropriate tables.
    Reports errors for malformed records.

    @param abfd   The BFD file handle.
    @param p      Pointer to the record data.
    @param reclen Length of the record data.
    @param is32   Nonzero if the record uses 32-bit offsets.
    @return       true on success, false on error.
*/
static bool
i386omf_read_pubdef(bfd* abfd, bfd_byte const* p, bfd_size_type reclen,
                    int is32)
{
  struct i386omf_obj_data* tdata = abfd->tdata.any;
  int base_group, base_segment;
  bfd_vma base_frame = 0;

  /* Base Group and Base Segment: OMF indices shared by all entries.  */
  if (!i386omf_read_index(abfd, &base_group, &p, &reclen))
    return false;
  if (!i386omf_read_index(abfd, &base_segment, &p, &reclen))
    return false;

  /* When Base Segment is 0, a 2-byte Base Frame follows (§3.3).
     Contents are ignored per spec; consumed only for wire-position
     correctness and debug diagnostics.  */
  if (base_segment == OMF_PUBDEF_SEGMENT_ABSOLUTE)
  {
    if (reclen < 2)
    {
      _bfd_error_handler("Truncated base frame in PUBDEF at 0x%lX",
                         (unsigned long) (p - tdata->image));
      bfd_set_error(bfd_error_wrong_format);
      return false;
    }
    base_frame = bfd_get_16(abfd, p);
    p += 2;
    reclen -= 2;
    if (omf_debug) fprintf(stderr, "PUBDEF with base frame 0x%04x\n",
                            (unsigned int) base_frame);
  }

  /* Per-entry loop: [Name][Offset][TypeIndex] repeats until body exhausted.  */
  while (reclen)
  {
    struct i386omf_symbol* pubdef;
    bfd_size_type slen;
    bfd_vma offset;

    pubdef = (struct i386omf_symbol*) bfd_make_empty_symbol(abfd);
    abfd->flags |= HAS_SYMS;

    /* Length-prefixed public name string (1–255 bytes, non-empty).  */
    slen = i386omf_read_string(abfd, &pubdef->name, p, reclen);
    if (slen < 1)
      return false;
    p += slen;
    reclen -= slen;

    /* Public Offset: 2B for 0x90, 4B for 0x91.  */
    if (!i386omf_read_offset(abfd, &offset, &p, &reclen,
                             is32 ? I386OMF_OFFSET_SIZE_32
                                  : I386OMF_OFFSET_SIZE_16))
      return false;

    /* Type Index (0 = no type data; informational only).  */
    if (!i386omf_read_index(abfd, &pubdef->type_index, &p, &reclen))
      return false;

    pubdef->base.name = pubdef->name.data;
    pubdef->base.flags |= BSF_GLOBAL;
    /* Per TIS v1.1 §5: the public offset alone is the symbol value.
       When Base Segment = 0, the Base Frame field is present but
       explicitly ignored by linkers — do not incorporate it here.  */
    pubdef->base.value = offset;
    pubdef->seg = strtab_lookup(tdata->segdef, base_segment);
    if (pubdef->seg)
    {
      pubdef->base.section = pubdef->seg->asect;
    }
    else
    {
      pubdef->base.section = bfd_und_section_ptr;
    }

    pubdef->group = strtab_lookup(tdata->grpdef, base_group);

    /* Dispatch on addressing mode (see §3.4 addressing summary).  */
    if (base_segment != OMF_PUBDEF_SEGMENT_ABSOLUTE)
    {
      /* Case 1: segment-relative (Base Segment ≠ 0).
         Group may be 0 (no group) or nonzero.  */
      struct i386omf_segment* seg;

      seg = strtab_lookup(tdata->segdef, base_segment);
      if (seg == NULL)
      {
        (*_bfd_error_handler)("PUBDEF %s in unknown SEGDEF %d",
                              pubdef->base.name, base_segment);
        bfd_set_error(bfd_error_wrong_format);
        return false;
      }
      strtab_add(seg->pubdef, pubdef);
    }
    else if (base_group != OMF_GRPDEF_NONE)
    {
      /* Case 2: group-relative (Seg = 0, Group ≠ 0).
         Base Frame is present on wire but ignored per spec.  */
      struct i386omf_group* group;

      group = strtab_lookup(tdata->grpdef, base_group);
      if (group == NULL)
      {
        _bfd_error_handler("PUBDEF %s in unknown GRPDEF %lu", pubdef->base.name,
                           (unsigned long) base_group);
        bfd_set_error(bfd_error_wrong_format);
        return false;
      }
      strtab_add(group->pubdef, pubdef);

      if (base_frame)
      {
        if (omf_debug) fprintf(stderr, "PUBDEF %s has nonzero base frame 0x%04lx\n",
                                pubdef->base.name, (unsigned long) base_frame);
      }
    }
    else
    {
      /* Case 3: absolute symbol (Seg = Group = 0).
         Offset alone is the symbol value; frame is ignored.  */
      strtab_add(tdata->abs_pubdef, pubdef);
    }
  }

  return true;
}

/*
    i386omf_read_lnames

SYNOPSIS
    static bool i386omf_read_lnames(bfd *abfd, bfd_byte const *p, bfd_size_type reclen);

DESCRIPTION
    Reads and processes an OMF LNAMES record, which defines a list of names.
    Adds each name to the lnames string table. Reports errors for malformed records.

    @param abfd   The BFD file handle.
    @param p      Pointer to the record data.
    @param reclen Length of the record data.
    @return       true on success, false on error.
*/
static bool
i386omf_read_lnames(bfd *abfd, bfd_byte const *p, bfd_size_type reclen) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;

    while (reclen) {
        struct counted_string *lname;
        bfd_size_type slen;

        lname = bfd_alloc(abfd, sizeof(*lname));
        if (lname == NULL)
            return false;
        slen = i386omf_read_string(abfd, lname, p, reclen);
        if (slen < 1)
            return false;
        strtab_add(tdata->lnames, lname);

        /* Advance to next string. */
        reclen -= slen;
        p += slen;
    }

    return true;
}

/*
    i386omf_read_segdef

SYNOPSIS
    static bool i386omf_read_segdef(bfd *abfd, bfd_byte const *p, bfd_size_type reclen, int is32);

DESCRIPTION
    Reads and processes an OMF SEGDEF record, which defines a segment.
    Creates a segment structure, sets its properties, and adds it to the segment table.
    Reports errors for malformed records.

    @param abfd   The BFD file handle.
    @param p      Pointer to the record data.
    @param reclen Length of the record data.
    @param is32   Nonzero if the record uses 32-bit offsets.
    @return       true on success, false on error.
*/
/* A=4 page-size tables indexed by ACBP alignment field (bits 7-5).
   _16: Intel 8086 convention = 256-byte page (2^8).
   _32: IBM convention = 4096-byte page (2^12), used by 32-bit toolchains.
   A=6 (LTL) is paragraph-aligned (16 bytes, 2^4) per the Intel spec.  */
static const unsigned int alignment_powers_16[] = {0, 0, 1, 4,  8, 2, 4, (unsigned int)-1};
static const unsigned int alignment_powers_32[] = {0, 0, 1, 4, 12, 2, 4, (unsigned int)-1};

static bool
i386omf_read_segdef(bfd *abfd, bfd_byte const *p, bfd_size_type reclen, int is32) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;
    const unsigned int *alignment_powers = is32 ? alignment_powers_32 : alignment_powers_16;
    int segdefs_seen = 0;

    while (reclen) {
        int alignment, combination, big, use32;
        int name_index, class_index, overlay_index;
        struct i386omf_segment *seg;
        struct i386omf_symbol *seg_sym;
        char const *segment_name;
        bfd_vma seglen;
        bfd_byte attr;
        bfd_byte const *rec_start;

        rec_start = p;
        attr = *p;

        alignment = (attr & OMF_SEGDEF_ALIGNMENT_MASK) >> OMF_SEGDEF_ALIGNMENT_SHIFT;
        combination = (attr & OMF_SEGDEF_COMBINATION_MASK) >> OMF_SEGDEF_COMBINATION_SHIFT;
        big = (attr >> 1) & 1;
        use32 = attr & 1;

        if (alignment == OMF_SEGDEF_ALIGNMENT_ABSOLUTE) {
            /* Absolute segment; get frame number and offset. */
            if (reclen < 4) {
                _bfd_error_handler("SEGDEF at 0x%lx is truncated, only %lu bytes remain.",
                                      (unsigned long)(p - tdata->image),
                                      (unsigned long)reclen);
                bfd_set_error(bfd_error_wrong_format);
                return false;
            }
            p += 4;
            reclen -= 4;
        } else {
            p += 1;
            reclen--;
        }

        if (!i386omf_read_offset(abfd, &seglen, &p, &reclen,
                                 is32 ? I386OMF_OFFSET_SIZE_32 : I386OMF_OFFSET_SIZE_16))
            return false;

        if (big && seglen != 0) {
            _bfd_error_handler("SEGDEF at 0x%lx has B=1 but segment length is non-zero (%lu)",
                               (unsigned long)(rec_start - tdata->image), (unsigned long)seglen);
            bfd_set_error(bfd_error_wrong_format);
            return false;
        }
        if (big) {
            if (is32) {
                if (sizeof(bfd_vma) < 8) {
                    _bfd_error_handler("SEGDEF at 0x%lx: B=1 Use32 segment requires 64-bit bfd_vma, "
                                       "capping size to 0xffffffff",
                                       (unsigned long)(rec_start - tdata->image));
                    seglen = (bfd_vma)-1;
                } else {
                    seglen = 0x100000000ULL;
                }
            } else {
                seglen = 0x10000;
            }
        }

        if (!i386omf_read_index(abfd, &name_index, &p, &reclen))
            return false;
        if (!i386omf_read_index(abfd, &class_index, &p, &reclen))
            return false;
        if (!i386omf_read_index(abfd, &overlay_index, &p, &reclen))
            return false;

        if (omf_debug) {
            if (name_index == 0)
                fprintf(stderr, "SEGDEF at 0x%lx: segment name index is zero (using default)\n",
                        (unsigned long)(rec_start - tdata->image));
            if (class_index == 0)
                fprintf(stderr, "SEGDEF at 0x%lx: class name index is zero (using default)\n",
                        (unsigned long)(rec_start - tdata->image));
            if (overlay_index == 0)
                fprintf(stderr, "SEGDEF at 0x%lx: overlay name index is zero\n",
                        (unsigned long)(rec_start - tdata->image));
        }

        if (alignment == OMF_SEGDEF_ALIGNMENT_UNDEFINED) {
            _bfd_error_handler("SEGDEF at 0x%lx: alignment value 7 is undefined, "
                               "treating as byte-aligned",
                               (unsigned long)(rec_start - tdata->image));
            alignment = OMF_SEGDEF_ALIGNMENT_RELOC_BYTE;
        } else if (alignment == OMF_SEGDEF_ALIGNMENT_LTL_PARA) {
            if (omf_debug)
                fprintf(stderr, "SEGDEF at 0x%lx: alignment value 6 (LTL) is not supported per Intel spec, treating as paragraph-aligned\n",
                        (unsigned long)(rec_start - tdata->image));
        }

        seg = bfd_alloc(abfd, sizeof(*seg));
        if (seg == NULL)
            return false;
        seg->combination = combination;
        seg->name_index = name_index;
        seg->class_index = class_index;
        seg->overlay_index = overlay_index;
        seg->big = big;
        seg->use32 = use32;   /* stored for future use; downstream handlers
                                 currently use the record-type is32 flag */
        if (omf_debug) fprintf(stderr, _("SEGDEF name_index:  %x, class_index: %x, overlay_index: %x"),
                            name_index,
                            class_index,
                            overlay_index);
        seg->pubdef = strtab_new(abfd);
        if (seg->pubdef == NULL)
            return false;

        seg->relocs = strtab_new(abfd);
        if (seg->relocs == NULL)
            return false;

        strtab_add(tdata->segdef, seg);

        segment_name = i386omf_lookup_string(tdata->lnames,
                                             name_index,
                                             "UNNAMED");
        if (segment_name == NULL) {
            bfd_set_error(bfd_error_wrong_format);
            return false;
        }

        seg->asect = bfd_make_section_anyway(abfd, segment_name);
        seg_sym = (struct i386omf_symbol *) seg->asect->symbol;
        seg_sym->name.len = strlen(segment_name);
        seg_sym->name.data = bfd_alloc(abfd, seg_sym->name.len + 1);
        if (seg_sym->name.data == NULL)
            return false;
        strcpy(seg_sym->name.data, segment_name);
        seg_sym->type_index = 0;
        seg_sym->seg = seg;
        seg_sym->group = NULL;
        seg->asect->used_by_bfd = seg;
        seg->asect->size = seglen;

        /* Use class name to guess if section should be SEC_CODE or SEC_DATA. */
        if (strstr(i386omf_lookup_string(tdata->lnames, class_index, ""),
                   "CODE"))
            seg->asect->flags |= SEC_CODE;
        if (strstr(i386omf_lookup_string(tdata->lnames, class_index, ""),
                   "DATA")) {
            if (seg->asect->flags & SEC_CODE)
                seg->asect->flags &= ~SEC_CODE;
            else
                seg->asect->flags |= SEC_DATA;
        }

        seg->asect->alignment_power = alignment_powers[alignment];

        segdefs_seen++;
    }

    if (reclen || segdefs_seen != 1)
        if (omf_debug) fprintf(stderr, "SEGDEF record doesn't contain exactly one segment definition\n");

    return true;
}

/*
    i386omf_read_grpdef

SYNOPSIS
    static bool i386omf_read_grpdef(bfd *abfd, bfd_byte const *p, bfd_size_type reclen);

DESCRIPTION
    Reads and processes an OMF GRPDEF record, which defines a group of segments.
    Adds the group and its entries to the group table. Reports errors for malformed records.

    @param abfd   The BFD file handle.
    @param p      Pointer to the record data.
    @param reclen Length of the record data.
    @return       true on success, false on error.
*/
static bool
i386omf_read_grpdef(bfd *abfd, bfd_byte const *p, bfd_size_type reclen) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;
    struct i386omf_group *grpdef;
    struct counted_string *s;

    grpdef = bfd_alloc(abfd, sizeof(*grpdef));
    if (grpdef == NULL)
        return false;

    if (!i386omf_read_index(abfd, &grpdef->name_index, &p, &reclen))
        return false;

    grpdef->entries = strtab_new(abfd);
    if (grpdef->entries == NULL)
        return false;
    grpdef->pubdef = strtab_new(abfd);
    if (grpdef->pubdef == NULL)
        return false;

    while (reclen) {
        struct i386omf_group_entry *entry;

        entry = bfd_alloc(abfd, sizeof(*entry));
        if (entry == NULL)
            return false;

        switch (*p) {
            case OMF_GRPDEF_COMPONENT_SEGMENT:
                p++;
                reclen--;
                entry->type = GRPDEF_ENTRY_SEGDEF;
                if (!i386omf_read_index(abfd, &entry->u.segdef, &p, &reclen))
                    return false;
                strtab_add(grpdef->entries, entry);
                break;
            default:
                (*_bfd_error_handler)("Unknown GRPDEF component type 0x%02x", *p);
                bfd_set_error(bfd_error_wrong_format);
                return false;
        }
    }

    s = strtab_lookup(tdata->lnames, grpdef->name_index);
    if (s == NULL) {
        (*_bfd_error_handler)("GRPDEF name is not an LNAME");
        bfd_set_error(bfd_error_wrong_format);
        return false;
    }

    grpdef->symbol = (struct i386omf_symbol *) bfd_make_empty_symbol(abfd);
    grpdef->symbol->name = *s;
    grpdef->symbol->base.name = grpdef->symbol->name.data;
    grpdef->symbol->base.value = 0;
    grpdef->symbol->base.flags |= BSF_SECTION_SYM;
    grpdef->symbol->base.section = bfd_und_section_ptr;
    abfd->flags |= HAS_SYMS;

    strtab_add(tdata->grpdef, grpdef);

    return true;
}

/*
    i386omf_fix_wrt_frame

SYNOPSIS
    static bfd_reloc_status_type i386omf_fix_wrt_frame(bfd *abfd, arelent *reloc_entry, asymbol *symbol, void *data, asection *input_section, bfd *output_bfd, char **error_message);

DESCRIPTION
    Relocation handler for WRT (with respect to) frame fixups.
    This implementation always returns bfd_reloc_continue.

    @param abfd           The BFD file handle.
    @param reloc_entry    The relocation entry.
    @param symbol         The symbol being relocated.
    @param data           Relocation data.
    @param input_section  The input section.
    @param output_bfd     The output BFD.
    @param error_message  Pointer to error message string.
    @return               bfd_reloc_continue.
*/
static bfd_reloc_status_type
i386omf_fix_wrt_frame(bfd *abfd ATTRIBUTE_UNUSED,
                      arelent *reloc_entry ATTRIBUTE_UNUSED,
                      asymbol *symbol ATTRIBUTE_UNUSED,
                      void *data ATTRIBUTE_UNUSED,
                      asection *input_section ATTRIBUTE_UNUSED,
                      bfd *output_bfd ATTRIBUTE_UNUSED,
                      char **error_message ATTRIBUTE_UNUSED) {
    return bfd_reloc_continue;
}

/*
    i386omf_read_fixupp

SYNOPSIS
    static bool i386omf_read_fixupp(bfd *abfd, bfd_byte const *p, bfd_size_type reclen);

DESCRIPTION
    Reads and processes an OMF FIXUPP record (0x9C or 0x9D), which describes
    relocations and fixups.  Handles both fixup subrecords (§4) and thread
    subrecords (§3), updating relocation tables as needed.

    Implements fixupp_record_spec.md — the specification derived from the
    TIS OMF v1.1 §"9CH or 9DH FIXUPP—Fixup Record".

    Record layout per §2:
      [ rectype 9C/9D ][ length lo ][ length hi ][ subrecords... ][ checksum ]
    The caller passes p pointing past the 3-byte header, reclen = length - 1
    (checksum already excluded).  See process_record() at line 2219.

    @param abfd    The BFD file handle.
    @param p       Pointer to the record body (after header, excl. checksum).
    @param reclen  Length of the record body.
    @param is_32bit Nonzero if record type == 0x9D (32-bit FIXUPP386).
    @return        true on success, false on error.
*/
static bool
i386omf_read_fixupp(bfd *abfd, bfd_byte const *p, bfd_size_type reclen, int is_32bit) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;
    bfd_byte const *q;

    while (reclen) {
        int subrec;

        /* §2: Subrecord type detection via high bit of first byte.
           bit 7 = 1 → FIXUP subrecord (§4)
           bit 7 = 0 → THREAD subrecord (§3)  */
        subrec = bfd_get_8(abfd,
                           p);
        if (subrec & OMF_FIXUPP_FIXUP) {    // this is a fixup field (§4)
            int location, fixdata;
            int frame_method, frame = 0, target_method, target = 0;
            bfd_size_type offset, displacement = 0;
            struct i386omf_relent *target_relent, *frame_relent;
            struct i386omf_symbol *sym, *frame_sym;
            reloc_howto_type *howto;

            /* §7 item 1: Reject FIXUP subrecord if no preceding data record.  */
            if (tdata->last_leidata == NULL) {
                _bfd_error_handler(_("FIXUP record without LEIDATA"));
                bfd_set_error(bfd_error_wrong_format);
                return false;
            }

            /* Minimum: Locat (2) + Fix Data (1) = 3 bytes always present.  */
            if (reclen < 3) {
                (*_bfd_error_handler)("FIXUP subrecord truncated at 0x%lx.",
                                      p - tdata->image);
                bfd_set_error(bfd_error_wrong_format);
                return false;
            }

            /* §4.2 + §4.3: Read the mandatory 3-byte prefix.
               Locat field byte order (§4.2): byte0 (subrec) carries the
               HIGH-order bits (type flag, M, Location, offset_hi);
               byte1 (p+1) carries the low 8 bits of the Data Record Offset.
               This is reversed from normal Intel byte order — reading the
               two bytes as big-endian on the wire yields the correct value.  */
            location = (subrec & OMF_FIXUP_LOCATION_MASK)          // bits 5-2: Location (4 bits, 0-13)
                    >> OMF_FIXUP_LOCATION_SHIFT;
            offset = bfd_get_8(abfd, p + 1) + 256 * (subrec & 3); // Data Record Offset = hi<<8 | lo (10 bits, 0-1023)
            fixdata = bfd_get_8(abfd, p + 2);                     // Fix Data byte (§4.3): F Frame(3) T P Targt(2)
            p += 3;
            reclen -= 3;
            if (omf_debug) fprintf(stderr, " FIXUP subrec at [%p]: %02x, M: %02x, location: %02x, offset: %02llx, fixdata: %02llx\n",
                    p, subrec, (subrec & OMF_FIXUP_SEGREL) >> 6, location, (unsigned long long)offset, (unsigned long long)fixdata);

            /* §4.4: Resolve FRAME.
               F=1 (OMF_FIX_DATA_FRAME_THREAD): FRAME from thread slot.
               F=0: explicit FRAME method F0-F5 in the Frame field.  */
            if (fixdata & OMF_FIX_DATA_FRAME_THREAD) {
                /* F=1: FRAME from thread.  */
                struct i386_fixup_thread *frame_thread;

                /* Mask Frame field bits 6-4 to 2 bits for thread number 0-3,
                   per §4.3: "FRAME thread number (use Frame & 3)".  */
                int frame_tnum = (fixdata & OMF_FIX_DATA_FRAME_MASK) >> OMF_FIX_DATA_FRAME_SHIFT & 3;
                /* §7 item 2: Reject reference to undefined thread.  */
                if (frame_tnum > 3 || !tdata->frame_thread_used[frame_tnum]) {
                    _bfd_error_handler("FIXUP at 0x%lx references undefined FRAME thread %d",
                                       (unsigned long)(p - tdata->image), frame_tnum);
                    bfd_set_error(bfd_error_wrong_format);
                    return false;
                }
                frame_thread = &tdata->frame_threads[frame_tnum];
                if (omf_debug) fprintf(stderr, "  fixup FRAME thread_number: %x, method: %d, is_frame: %d, index: %x\n",
                                        frame_thread->thread_number,
                                        frame_thread->method,
                                        frame_thread->is_frame,
                                        frame_thread->index);
                frame_method = frame_thread->method;
                frame = frame_thread->index;
            /* F=0: explicit FRAME method F0-F5.  */
            } else {
                /* FRAME method is explicitly defined in this fixup field. */
                /* frame field contains 0,1, 2, 4, or 5, corresponding to one of the methods of specifying a FRAME listed in Table 19-2. */
                frame_method = (fixdata & OMF_FIX_DATA_FRAME_MASK) >> OMF_FIX_DATA_FRAME_SHIFT;
            }

            /* §4.4: Resolve FRAME by method.  */
            switch (frame_method) {
                struct i386omf_segment *segdef;
                struct i386omf_group *grp;

                case OMF_FIXUPP_FRAME_SEGDEF:        /* F0: SEGDEF index.  */
                    if (!(fixdata & OMF_FIX_DATA_FRAME_THREAD)
                        && !i386omf_read_index(abfd, &frame, &p, &reclen))
                        return false;
                    segdef = i386omf_find_segment(tdata, frame);
                    if (segdef == NULL) {
                        _bfd_error_handler("FIXUP at 0x%lx references undefined segment [%d]",
                                           (unsigned long)(p - tdata->image), frame);
                        bfd_set_error(bfd_error_wrong_format);
                        return false;
                    }
                    frame_sym = (struct i386omf_symbol *) segdef->asect->symbol;
                    break;
                case OMF_FIXUPP_FRAME_GRPDEF:        /* F1: GRPDEF index.  */
                    if (!(fixdata & OMF_FIX_DATA_FRAME_THREAD)
                        && !i386omf_read_index(abfd, &frame, &p, &reclen))
                        return false;
                    grp = strtab_lookup(tdata->grpdef, frame);
                    if (grp == NULL) {
                        _bfd_error_handler("FIXUP at 0x%lx references undefined group [%d]",
                                           (unsigned long)(p - tdata->image), frame);
                        bfd_set_error(bfd_error_wrong_format);
                        return false;
                    }
                    frame_sym = grp->symbol;
                    break;
                case OMF_FIXUPP_FRAME_EXTDEF:        /* F2: EXTDEF index.  */
                    if (!(fixdata & OMF_FIX_DATA_FRAME_THREAD)
                        && !i386omf_read_index(abfd, &frame, &p, &reclen))
                        return false;
                    frame_sym = strtab_lookup(tdata->externs, frame);
                    break;
                case OMF_FIXUPP_FRAME_EXPLICIT:      /* F3: explicit frame — invalid.  §7 item 3.  */
                    _bfd_error_handler("FIXUP at 0x%lx invalid explicit frame method F3",
                                       (unsigned long)(p - tdata->image));
                    bfd_set_error(bfd_error_wrong_format);
                    return false;
                case OMF_FIXUPP_FRAME_LEIDATA:       /* F4: frame = preceding LEDATA's segment.  */
                    frame_sym = (struct i386omf_symbol *) tdata->last_leidata->asect->symbol;
                    break;
                case OMF_FIXUPP_FRAME_TARGET:        /* F5: frame = TARGET's segment/group/external.
                                                         Resolved in second pass (§4.4, §7 item 10).  */
                    frame_sym = NULL;
                    break;
                default:                             /* F6+ invalid.  §7 item 3.  */
                    bfd_set_error(bfd_error_wrong_format);
                    return false;
            }

            /* §4.5: Resolve TARGET.
               T=1 (OMF_FIX_DATA_TARGET_THREAD): TARGET from thread slot.
               T=0: explicit method (P:Targt) as 3-bit selector.  */
            if (fixdata & OMF_FIX_DATA_TARGET_THREAD) {     // T=1: via thread
                struct i386_fixup_thread *target_thread;

                int target_tnum = fixdata & OMF_FIX_DATA_TARGT_MASK;
                /* §7 item 2: Reject reference to undefined thread.  */
                if (target_tnum > 3 || !tdata->target_thread_used[target_tnum]) {
                    _bfd_error_handler("FIXUP at 0x%lx references undefined TARGET thread %d",
                                       (unsigned long)(p - tdata->image), target_tnum);
                    bfd_set_error(bfd_error_wrong_format);
                    return false;
                }
                target_thread = &tdata->target_threads[target_tnum];
                /* §4.5: Effective method = (P << 2) | (stored_method & 3).
                   The thread stores only low 2 method bits; the high bit
                   (method 4 vs 0, etc.) comes from THIS FIXUP's P bit.  */
                target_method = ((fixdata & OMF_FIX_DATA_P_MASK) >> 2) << 2
                              | (target_thread->method & 3);
                target = target_thread->index;   // datum from thread store
            } else {   // T=0: explicit method
                target_method = fixdata & OMF_FIX_DATA_TARGET_METHOD_MASK;   // (P:Targt) = 3-bit method
            }
            target_relent = bfd_alloc(abfd, sizeof(*target_relent));
            if (target_relent == NULL)
                return false;

            q = p;
            /* §4.4 conditional field presence + §4.5 method dispatch.
               Index field present for methods 0,1,2,4,5,6;
               absent for method 3 (explicit frame number reads 2 bytes below).
               When T=1 ("via thread"), datum comes from thread store —
               skip stream read.  */
            switch (target_method) {
                struct i386omf_segment *segdef;
                struct i386omf_group *grpdef;

                case OMF_FIXUPP_TARGET_SEGDEF:               // T0: SEGDEF index + displacement
                case OMF_FIXUPP_TARGET_NODISP | OMF_FIXUPP_TARGET_SEGDEF:  // T4: SEGDEF index only
                    if (!(fixdata & OMF_FIX_DATA_TARGET_THREAD) && !i386omf_read_index(abfd, &target, &p, &reclen))
                        return false;
                    segdef = i386omf_find_segment(tdata, target);
                    if (segdef == NULL) {
                        _bfd_error_handler("FIXUP at 0x%lx wants phantom segment [%d]",
                                            (unsigned long)(q - tdata->image), target);
                        bfd_set_error(bfd_error_wrong_format);
                        return false;
                    }
                    target_relent->symbol = segdef->asect->symbol;
                    break;
                case OMF_FIXUPP_TARGET_GRPDEF:               // T1: GRPDEF index + displacement
                case OMF_FIXUPP_TARGET_NODISP | OMF_FIXUPP_TARGET_GRPDEF:  // T5: GRPDEF index only
                    if (!(fixdata & OMF_FIX_DATA_TARGET_THREAD) && !i386omf_read_index(abfd, &target, &p, &reclen))
                        return false;
                    grpdef = strtab_lookup(tdata->grpdef, target);
                    if (grpdef == NULL) {
                        _bfd_error_handler("FIXUP at 0x%lx wants displacement but none given [%d]",
                            (unsigned long)(q - tdata->image), target);
                        bfd_set_error(bfd_error_wrong_format);
                        return false;
                    }
                    target_relent->symbol = NULL;
                    break;
                case OMF_FIXUPP_TARGET_EXTDEF:               // T2: EXTDEF index + displacement
                case OMF_FIXUPP_TARGET_NODISP | OMF_FIXUPP_TARGET_EXTDEF:  // T6: EXTDEF index only
                    if (!(fixdata & OMF_FIX_DATA_TARGET_THREAD) && !i386omf_read_index(abfd, &target, &p, &reclen))
                        return false;
                    sym = strtab_lookup(tdata->externs, target);
                    if (sym == NULL) {
                        _bfd_error_handler("FIXUP at 0x%lx wants phantom extern [%d]",
                                           (unsigned long)(q - tdata->image),
                                           target);
                        bfd_set_error(bfd_error_wrong_format);
                        return false;
                    }
                    target_relent->symbol = &sym->base;                 // base is arelent
                    break;
                case OMF_FIXUPP_TARGET_EXPLICIT:              // T3: explicit frame number
                case OMF_FIXUPP_TARGET_NODISP | OMF_FIXUPP_TARGET_EXPLICIT: // T7: explicit frame, no disp
                    /* §4.4: When T=0, a 2-byte explicit frame number follows.
                       When T=1, the datum comes from the thread store — skip
                       the stream read (the thread stores the frame number as
                       its index, or 0 if none was provided).  */
                    if (!(fixdata & OMF_FIX_DATA_TARGET_THREAD)) {
                        target = (int) bfd_get_16(abfd, p);
                        p += 2;
                        reclen -= 2;
                    }
                    target_relent->symbol = NULL;
                    break;
            }
            /* §4.6 + §7 item 5: Target Displacement present if P=0.
               Width: 2 bytes for 0x9C (FIXUPP), 4 bytes for 0x9D (FIXUPP386).  */
            if (!(fixdata & OMF_FIX_DATA_P_MASK)) {
                if (!i386omf_read_offset(abfd, &displacement, &p, &reclen,
                                         is_32bit ? I386OMF_OFFSET_SIZE_32 : I386OMF_OFFSET_SIZE_16)) {
                    _bfd_error_handler("FIXUP at 0x%lx wants displacement but none given [%d]",
                                        (unsigned long)(q - tdata->image),
                                        target);
                    bfd_set_error(bfd_error_wrong_format);
                    return false;
                }
            }

            /* §4.2.1: Validate Location value (0-13, values 7,8,10,12 reserved).  */
            if (location >= 14) {
                _bfd_error_handler("FIXUP at 0x%lx unsupported location type %d",
                                   (unsigned long)(q - tdata->image), location);
                bfd_set_error(bfd_error_wrong_format);
                return false;
            }

            /* §7 (BFD relocation generation): Build arelent from decoded fixup.
               howto selected by (location, mode=M bit).  */
            target_relent->base.sym_ptr_ptr = &target_relent->symbol;
            target_relent->base.address = tdata->last_leidata->last_data_offset + offset;
            howto = &(subrec & OMF_FIXUP_SEGREL                     // M bit: 1=segrel, 0=self-rel
                      ? howto_table_i386omf_segrel
                      : howto_table_i386omf_pcrel)[location];
            target_relent->base.addend
                    = displacement + (subrec & OMF_FIXUP_SEGREL
                                      ? 0
                                      : -bfd_get_reloc_size(howto));
            target_relent->base.howto = howto;
            strtab_add(tdata->last_leidata->relocs, target_relent);

            /* §7 item 10: second-pass frame-reloc emission.
               F4 (FRAME_LEIDATA): frame is already the LEIDATA segment,
               implicit in context — no frame reloc needed.
               F5 (FRAME_TARGET): derive frame symbol from TARGET's
               segment/group/external now that TARGET is known.  */
            switch (frame_method) {
                case OMF_FIXUPP_FRAME_LEIDATA:
                    break;
                case OMF_FIXUPP_FRAME_TARGET:
                    /* Frame derived from target's segment/group/external. */
                    frame_sym = (struct i386omf_symbol *) target_relent->symbol;
                    if (frame_sym == NULL)
                        break;
                    /* fall through */
                default:
                    frame_relent = bfd_alloc(abfd, sizeof(*frame_relent));
                    if (frame_relent == NULL)
                        return false;
                    frame_relent->symbol = frame_sym ? &frame_sym->base : NULL;
                    frame_relent->base.sym_ptr_ptr = &frame_relent->symbol;
                    frame_relent->base.address = tdata->last_leidata->last_data_offset + offset;
                    frame_relent->base.addend = 0;
                    frame_relent->base.howto = &howto_wrt_segdef;
                    strtab_add(tdata->last_leidata->relocs, frame_relent);
                    break;
            }

            abfd->flags |= HAS_SYMS;
            tdata->last_leidata->asect->flags |= SEC_RELOC;

        } else {    // THREAD subrecord (§3)
            int threaddata, index = 0;

            /* §3.1: First byte.  Layout: 0 D MMM TT (1+1+3+2 bits; bit 2 unused).  */
            threaddata = bfd_get_8(abfd, p++);
            reclen--;

            int thmethod = (threaddata & OMF_FIXUP_THREAD_DATA_METHOD_MASK) >> OMF_FIXUP_THREAD_DATA_METHOD_SHIFT;  // bits 5-3: Method
            int tnum = threaddata & OMF_FIXUP_THREAD_DATA_THREAD_NUMBER;       // bits 1-0: Thred (0-3)
            bool is_frame_thread = (threaddata & OMF_FIXUP_THREAD_DATA_D_FIELD_MASK) >> OMF_FIXUP_THREAD_DATA_D_FIELD_SHIFT;  // bit 6: D

            /* §3.2 + §3.3: Index field present for methods 0,1,2 (SEGDEF/GRPDEF/EXTDEF)
               and TARGET-only methods 4,5,6.  Absent for method 3 (explicit frame)
               and FRAME methods 4,5.  Uses variable-length OMF index (§3.3).  */
            if (thmethod <= 2 || (!is_frame_thread && thmethod >= 4 && thmethod <= 6))
                i386omf_read_index(abfd, &index, &p, &reclen);

            if (omf_debug) fprintf(stderr,
                    " THREAD subrec: %02x, D(%x): %s, method: %d - %s, thread number: %d, index: %d",
                    threaddata,
                    (threaddata & 0x40) >> 6,
                    is_frame_thread ? "FRAME" : "TARGET",
                    thmethod,
                    thread_method[thmethod],
                    tnum,
                    index
            );

            /* §3.4: Store thread into the appropriate slot.
               §7 item 9: Threads persist across FIXUPP records — state is
               not reset between records.  */
            if (is_frame_thread) {
                tdata->frame_threads[tnum].index = index;
                tdata->frame_threads[tnum].thread_number = tnum;
                tdata->frame_threads[tnum].is_frame = true;
                tdata->frame_threads[tnum].method = thmethod;
                tdata->frame_thread_used[tnum] = true;
            } else {
                tdata->target_threads[tnum].index = index;
                tdata->target_threads[tnum].thread_number = tnum;
                tdata->target_threads[tnum].is_frame = false;
                tdata->target_threads[tnum].method = thmethod;
                tdata->target_thread_used[tnum] = true;
            }
        }
    }

    return true;
}

/* LIDATA expansion context: a growable byte buffer for assembling
   one record's fully expanded data before a single bounds-checked
   copy into the section (see §6.1 of the LIDATA spec).  */
struct i386omf_bytebuf
{
  bfd_byte      *data;
  bfd_size_type  len;
  bfd_size_type  cap;
};

static bool
i386omf_bytebuf_append (struct i386omf_bytebuf *buf,
                         bfd_byte const *src, bfd_size_type n)
{
  if (buf->len + n > buf->cap)
    {
      bfd_size_type newcap = buf->cap ? buf->cap * 2 : 256;
      while (newcap < buf->len + n)
        newcap *= 2;
      bfd_byte *p = bfd_realloc (buf->data, newcap);
      if (p == NULL)
        return false;
      buf->data = p;
      buf->cap = newcap;
    }
  memcpy (buf->data + buf->len, src, n);
  buf->len += n;
  return true;
}

/* Maximum recursive nesting depth for a LIDATA Data Block field.
   This is an implementation safety bound, not a TIS-specified limit.
   64 levels is far beyond any legitimate translator output (canonical
   dup()-style use cases nest only 1-2 levels deep).  */
#define I386OMF_LIDATA_MAX_DEPTH 64

/*
    i386omf_expand_lidata_block

SYNOPSIS
    static bool i386omf_expand_lidata_block(bfd *abfd, bfd_byte const *p,
                                             bfd_size_type avail,
                                             bfd_size_type *consumed,
                                             struct i386omf_bytebuf *out,
                                             int is_32, int depth);

DESCRIPTION
    Recursively parses and expands exactly one LIDATA Data Block field (§3).
    Appends the fully expanded bytes to *out.  Sets *consumed to the
    number of bytes read from p for this one Data Block field.

    @param abfd     The BFD file handle.
    @param p        Pointer to the start of this Data Block field.
    @param avail    Bytes available at p.
    @param consumed Out: bytes consumed for this one Data Block.
    @param out      Expanded bytes are appended here.
    @param is_32    Non-zero if Repeat Count is 32-bit (0xA3 record).
    @param depth    Current recursion depth.
    @return         true on success, false on error.
*/
static bool
i386omf_expand_lidata_block (bfd *abfd, bfd_byte const *p,
                              bfd_size_type avail,
                              bfd_size_type *consumed,
                              struct i386omf_bytebuf *out,
                              int is_32, int depth)
{
  struct i386omf_obj_data *tdata = abfd->tdata.any;
  bfd_size_type repeat_count;
  bfd_size_type block_count;
  bfd_size_type eaten = 0;

  if (depth > I386OMF_LIDATA_MAX_DEPTH)
    {
      _bfd_error_handler ("LIDATA nesting exceeds the implementation "
                           "limit of %d levels at 0x%lx",
                           I386OMF_LIDATA_MAX_DEPTH,
                           (unsigned long)(p - tdata->image));
      bfd_set_error (bfd_error_wrong_format);
      return false;
    }

  /* Repeat Count width depends on record type (is_32).  Block Count
     is always 16-bit per §4 constraint #2.  */
  if (is_32)
    {
      if (avail < 6)
        {
          _bfd_error_handler ("LIDATA data block truncated at 0x%lx",
                               (unsigned long)(p - tdata->image));
          bfd_set_error (bfd_error_wrong_format);
          return false;
        }
      repeat_count = bfd_get_32 (abfd, p);
      eaten += 4;
    }
  else
    {
      if (avail < 4)
        {
          _bfd_error_handler ("LIDATA data block truncated at 0x%lx",
                               (unsigned long)(p - tdata->image));
          bfd_set_error (bfd_error_wrong_format);
          return false;
        }
      repeat_count = bfd_get_16 (abfd, p);
      eaten += 2;
    }

  block_count = bfd_get_16 (abfd, p + eaten);
  eaten += 2;

  p     += eaten;
  avail -= eaten;

  if (block_count == 0)
    {
      /* ── Leaf case: 1-byte count + count data bytes ──── */
      bfd_size_type i;
      int licount;

      if (avail < 1)
        {
          _bfd_error_handler ("LIDATA leaf content truncated at 0x%lx",
                               (unsigned long)(p - tdata->image));
          bfd_set_error (bfd_error_wrong_format);
          return false;
        }

      licount = bfd_get_8 (abfd, p);

      if (avail < (bfd_size_type)(1 + licount))
        {
          _bfd_error_handler ("LIDATA leaf content truncated at 0x%lx "
                               "(declares %d bytes, only %lu available)",
                               (unsigned long)(p - tdata->image),
                               licount, (unsigned long)(avail - 1));
          bfd_set_error (bfd_error_wrong_format);
          return false;
        }

      for (i = 0; i < repeat_count; i++)
        {
          if (!i386omf_bytebuf_append (out, p + 1, (bfd_size_type)licount))
            {
              _bfd_error_handler ("Out of memory expanding LIDATA at 0x%lx",
                                   (unsigned long)(p - tdata->image));
              bfd_set_error (bfd_error_no_memory);
              return false;
            }
        }

      eaten += 1 + (bfd_size_type)licount;
    }
  else
    {
      /* ── Recursive case: Block Count child Data Block fields ────
       *
       * Children are expanded and concatenated FIRST into a temporary
       * buffer.  Only the resulting concatenation, as a whole, is
       * repeated Repeat Count times (§3.1 correctness property).  */
      struct i386omf_bytebuf children = { NULL, 0, 0 };
      bfd_size_type i;
      bfd_byte const *child_p = p;
      bfd_size_type   child_avail = avail;

      for (i = 0; i < block_count; i++)
        {
          bfd_size_type child_consumed = 0;

          if (!i386omf_expand_lidata_block (abfd, child_p, child_avail,
                                             &child_consumed, &children,
                                             is_32, depth + 1))
            {
              free (children.data);
              return false;
            }

          if (child_consumed > child_avail)
            {
              _bfd_error_handler ("LIDATA internal consistency error "
                                   "at 0x%lx",
                                   (unsigned long)(child_p - tdata->image));
              bfd_set_error (bfd_error_wrong_format);
              free (children.data);
              return false;
            }

          child_p     += child_consumed;
          child_avail -= child_consumed;
          eaten       += child_consumed;
        }

      for (i = 0; i < repeat_count; i++)
        {
          if (!i386omf_bytebuf_append (out, children.data, children.len))
            {
              _bfd_error_handler ("Out of memory expanding LIDATA at 0x%lx",
                                   (unsigned long)(p - tdata->image));
              bfd_set_error (bfd_error_no_memory);
              free (children.data);
              return false;
            }
        }

      free (children.data);
    }

  *consumed = eaten;
  return true;
}

/*
    i386omf_add_expanded_lidata

SYNOPSIS
    static bool i386omf_add_expanded_lidata(bfd *abfd,
                                             struct bfd_section *asect,
                                             bfd_vma offset,
                                             bfd_byte const *data,
                                             bfd_size_type len);

DESCRIPTION
    Copies a fully expanded LIDATA byte sequence into a section's
    contents at the given offset, lazily allocating the section's
    backing storage if needed.

    @param abfd    The BFD file handle.
    @param asect   The destination section.
    @param offset  Byte offset within the section to write at.
    @param data    Pointer to the expanded byte sequence.
    @param len     Length of the expanded byte sequence.
    @return        true on success, false on error.
*/
static bool
i386omf_add_expanded_lidata (bfd *abfd, struct bfd_section *asect,
                              bfd_vma offset, bfd_byte const *data,
                              bfd_size_type len)
{
  if ((asect->flags & SEC_IN_MEMORY) == 0)
    {
      asect->contents = bfd_zalloc (abfd, asect->size);
      if (asect->contents == NULL)
        {
          _bfd_error_handler ("Out of memory for %s section contents",
                               bfd_section_name (asect));
          return false;
        }
      asect->flags |= SEC_IN_MEMORY;
    }

  if ((asect->size < offset) || (asect->size - offset < len))
    {
      _bfd_error_handler ("Expanded LIDATA overflows section %s "
                           "(offset 0x%lx, length 0x%lx, section size 0x%lx)",
                           bfd_section_name (asect),
                           (unsigned long)offset,
                           (unsigned long)len,
                           (unsigned long)asect->size);
      bfd_set_error (bfd_error_wrong_format);
      return false;
    }

  memcpy (asect->contents + offset, data, len);
  return true;
}

/*
    i386omf_read_lidata

SYNOPSIS
    static bool i386omf_read_lidata(bfd *abfd, bfd_byte const *p,
                                     bfd_size_type reclen, int rectype);

DESCRIPTION
    Reads an OMF LIDATA (0xA2) or LIDATA386 (0xA3) record and expands
    its iterated data block into the target segment's section contents.
    Expansion uses a scratch buffer for correctness (§6.1 of spec).

    @param abfd    The BFD file handle.
    @param p       Pointer to the record body, after the 3-byte header.
    @param reclen  Length of the record body, excluding checksum.
    @param rectype OMF_RECORD_LIDATA or OMF_RECORD_LIDATA386.
    @return        true on success, false on error.
*/
static bool
i386omf_read_lidata (bfd *abfd, bfd_byte const *p,
                      bfd_size_type reclen, int rectype)
{
  struct i386omf_obj_data *tdata = abfd->tdata.any;
  struct i386omf_segment  *segdef;
  bfd_vma  offset;
  int      seg_index;
  int      is_32 = rectype & 1;

  if (!i386omf_read_index (abfd, &seg_index, &p, &reclen))
    return false;

  if (seg_index <= OMF_SEGDEF_NONE)
    {
      _bfd_error_handler ("LIDATA at 0x%lx has no segment "
                           "(segment index must be nonzero)",
                           (unsigned long)(p - tdata->image));
      bfd_set_error (bfd_error_wrong_format);
      return false;
    }

  segdef = i386omf_find_segment (tdata, seg_index);
  if (segdef == NULL)
    {
      if (seg_index >= OMF_COMDAT_SEGIDX_BASE)
        {
          segdef = i386omf_create_comdat_segment (abfd);
          if (segdef == NULL)
            return false;
        }
      else
        {
          _bfd_error_handler ("LIDATA at 0x%lx wants phantom segment [%d]",
                               (unsigned long)(p - tdata->image),
                               seg_index);
          bfd_set_error (bfd_error_wrong_format);
          return false;
        }
    }

  tdata->last_leidata = segdef;

  if (!i386omf_read_offset (abfd, &offset, &p, &reclen,
                             is_32 ? I386OMF_OFFSET_SIZE_32
                                   : I386OMF_OFFSET_SIZE_16))
    return false;

  segdef->last_data_offset = offset;

  struct i386omf_bytebuf expanded = { NULL, 0, 0 };
  bfd_size_type consumed = 0;

  if (!i386omf_expand_lidata_block (abfd, p, reclen, &consumed,
                                     &expanded, is_32, 0))
    {
      free (expanded.data);
      return false;
    }

  if (consumed != reclen)
    {
      _bfd_error_handler ("LIDATA at 0x%lx has %lu leftover byte(s) "
                           "after the iterated data block",
                           (unsigned long)(p - tdata->image),
                           (unsigned long)(reclen - consumed));
      bfd_set_error (bfd_error_wrong_format);
      free (expanded.data);
      return false;
    }

  if (offset + expanded.len > segdef->asect->size)
    {
      if (seg_index >= OMF_COMDAT_SEGIDX_BASE)
        {
          segdef->asect->size = offset + expanded.len;
        }
      else
        {
          _bfd_error_handler ("LIDATA at 0x%lx expands to %lu bytes at "
                               "offset 0x%lx, overflowing section %s "
                               "(declared size 0x%lx)",
                               (unsigned long)(p - tdata->image),
                               (unsigned long)expanded.len,
                               (unsigned long)offset,
                               bfd_section_name (segdef->asect),
                               (unsigned long)segdef->asect->size);
          bfd_set_error (bfd_error_wrong_format);
          free (expanded.data);
          return false;
        }
    }

  if (!i386omf_add_expanded_lidata (abfd, segdef->asect, offset,
                                     expanded.data, expanded.len))
    {
      free (expanded.data);
      return false;
    }

  free (expanded.data);

  segdef->asect->flags |= (SEC_HAS_CONTENTS | SEC_LOAD | SEC_ALLOC);

  return true;
}

/*
    i386omf_add_section_data

SYNOPSIS
    static bool i386omf_add_section_data(bfd *abfd, struct bfd_section *asect, bfd_vma offset, bfd_byte const *p, bfd_size_type reclen, int rectype);

DESCRIPTION
    Adds LEDATA record data to a section.  Allocates memory for the section
    contents if needed.  Reports errors for overflows or allocation failures.

    Note: LIDATA records no longer reach this function — they have their
    own dedicated handler (i386omf_read_lidata) that performs scratch-buffer
    expansion before a single bounds-checked copy.

    @param abfd    The BFD file handle.
    @param asect   The section to add data to.
    @param offset  Offset in the section to start writing.
    @param p       Pointer to the record data.
    @param reclen  Length of the record data.
    @param rectype Record type (LEDATA or LEDATA386).
    @return        true on success, false on error.
*/
static bool
i386omf_add_section_data(bfd *abfd,
                         struct bfd_section *asect,
                         bfd_vma offset,
                         bfd_byte const *p,
                         bfd_size_type reclen, int rectype ATTRIBUTE_UNUSED) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;

    /* Lazily allocate memory for section data. */
    if ((asect->flags & SEC_IN_MEMORY) == 0) {
        asect->contents = bfd_zalloc(abfd, asect->size);
        if (asect->contents == NULL) {
            _bfd_error_handler("Out of memory for %s section contents",
                                bfd_section_name(asect));
            return false;
        }
        asect->flags |= SEC_IN_MEMORY;
    }

    if ((asect->size < offset) || (asect->size - offset < reclen)) {
        _bfd_error_handler("LEDATA at 0x%lx overflows section %s",
                  (unsigned long)(p - tdata->image),
                  bfd_section_name(asect));
        bfd_set_error(bfd_error_wrong_format);
        return false;
    }

    memcpy(asect->contents + offset, p, reclen);

    return true;
}

/*
    i386omf_read_leidata

SYNOPSIS
    static bool i386omf_read_leidata(bfd *abfd, bfd_byte const *p, bfd_size_type reclen, int rectype);

DESCRIPTION
    Reads and processes an OMF LEDATA or LIDATA record, adding its contents to the appropriate section.
    Updates the section's flags and handles segment lookup and offset calculation.

    @param abfd    The BFD file handle.
    @param p       Pointer to the record data.
    @param reclen  Length of the record data.
    @param rectype Record type (LE/LIDATA).
    @return        true on success, false on error.
*/
static bool
i386omf_read_leidata(bfd *abfd, bfd_byte const *p,
                     bfd_size_type reclen, int rectype) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;
    struct i386omf_segment *segdef;
    bfd_vma offset;
    int seg_index;

    if (!i386omf_read_index(abfd, &seg_index, &p, &reclen))
        return false;

    if (seg_index <= OMF_SEGDEF_NONE) {
        (*_bfd_error_handler)("LEDATA at 0x%lx has no segment",
                             p - tdata->image);
        bfd_set_error(bfd_error_wrong_format);
        return false;
    }

    segdef = i386omf_find_segment (tdata, seg_index);
    if (segdef == NULL) {
      if (seg_index >= OMF_COMDAT_SEGIDX_BASE)
        {
          segdef = i386omf_create_comdat_segment (abfd);
          if (segdef == NULL)
            return false;
        }
      else
        {
          _bfd_error_handler("LEDATA at 0x%lx wants phantom segment [%d]",
                              p - tdata->image,
                              seg_index);
          bfd_set_error(bfd_error_wrong_format);
          return false;
        }
    }

    /* We'll need to know which section FIXUP records refer to. */
    tdata->last_leidata = segdef;

    if (!i386omf_read_offset(abfd, &offset, &p, &reclen,
                             rectype & 1 ? I386OMF_OFFSET_SIZE_32 : I386OMF_OFFSET_SIZE_16))
        return false;

    segdef->last_data_offset = offset;

    /* COMDAT segments are created with size 0.  Grow the section
       to accommodate the data.  */
    if (offset + reclen > segdef->asect->size)
      segdef->asect->size = offset + reclen;

    if (!i386omf_add_section_data(abfd, segdef->asect, offset,
                                  p, reclen, rectype))
        return false;

    segdef->asect->flags |= (SEC_HAS_CONTENTS |
                             SEC_LOAD |
                             SEC_ALLOC);

    return true;
}

/*
    i386omf_read_comdat

SYNOPSIS
    static bool i386omf_read_comdat(bfd *abfd, bfd_byte const *p,
                                    bfd_size_type reclen, int rectype);

DESCRIPTION
    Reads and processes an OMF COMDAT (0xC2) or COMDAT386 (0xC3) record
    per TIS v1.1 §6.4.

    The COMDAT layout is:

      [Flags 1B][Attributes 1B][Align 1B][Enumerated Data Offset 2/4B]
      [Type Index 1-2B][Public Base *conditional*][Public Name 1-2B][Data]

    Attributes low nibble = Allocation Type.  If Explicit (0x00), the
    Public Base fields (Base Group, Base Segment, optional Base Frame)
    are present.

    Handles:
      - Iterated Data (Flags bit 1): expands LIDATA-format data payload.
      - Continuation (Flags bit 0): reuses the previous COMDAT's synthetic
        segment, identified by Public Name Index match.

    @param abfd    The BFD file handle.
    @param p       Pointer to the record data.
    @param reclen  Length of the record data.
    @param rectype Record type (0xC2 or 0xC3).
    @return        true on success, false on error.
*/
static bool
i386omf_read_comdat(bfd *abfd, bfd_byte const *p,
                    bfd_size_type reclen, int rectype) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;
    struct i386omf_segment *segdef;
    bfd_vma offset;
    unsigned int flags, attributes, alloc_type, align_byte, sel_criteria;
    int type_idx, base_group, base_segment, base_frame, name_idx;
    int is_32 = rectype & 1;

    /* Record start for diagnostic offsets.  */
    bfd_byte const *rec_start = p;

    /* 1. Flags byte.  */
    if (reclen < 1) goto trunc;
    flags = *p++; reclen--;

    /* 2. Attributes byte — low nibble = Allocation Type.  */
    if (reclen < 1) goto trunc;
    attributes = *p++; reclen--;
    alloc_type = attributes & 0x0F;
    sel_criteria = (attributes >> 4) & 0x0F;

    /* Reserve Selection Criteria values 0x4-0xF are spec violations.  */
    if (sel_criteria > 3 && omf_debug)
        fprintf(stderr, "COMDAT at 0x%lx: reserved selection criteria "
                "value %d\n",
                (unsigned long)(rec_start - tdata->image), sel_criteria);

    /* 3. Alignment byte.  */
    if (reclen < 1) goto trunc;
    align_byte = *p++; reclen--;

    /* Reserved align values 6,7 are spec violations.  */
    if (align_byte >= 6 && omf_debug)
        fprintf(stderr, "COMDAT at 0x%lx: reserved alignment value %d\n",
                (unsigned long)(rec_start - tdata->image), align_byte);

    /* 4. Enumerated Data Offset.  */
    if (!i386omf_read_offset(abfd, &offset, &p, &reclen,
                             is_32 ? I386OMF_OFFSET_SIZE_32
                                   : I386OMF_OFFSET_SIZE_16))
        return false;

    /* 5. Type Index — references a COMDEF definition.  */
    if (!i386omf_read_index(abfd, &type_idx, &p, &reclen))
        return false;

    /* 6. Public Base — present only for Explicit allocation.  */
    base_group = 0;
    base_segment = 0;
    base_frame = 0;
    if (alloc_type == 0x00) {
        if (!i386omf_read_index(abfd, &base_group, &p, &reclen))
            return false;
        if (!i386omf_read_index(abfd, &base_segment, &p, &reclen))
            return false;
        if (base_segment == 0) {
            if (reclen < 2) goto trunc;
            base_frame = (int) bfd_get_16(abfd, p);
            p += 2; reclen -= 2;
        }
    }

    /* 7. Public Name Index — logical name in the LNAMES table.  */
    if (!i386omf_read_index(abfd, &name_idx, &p, &reclen))
        return false;

    /* 8. Segment selection / creation.
       Continuation (bit 0): reuse previous COMDAT segment for same symbol.
       Otherwise: create a new synthetic segment.  */
    if ((flags & 0x01) && tdata->last_comdat_name_idx == name_idx
        && tdata->last_comdat_seg != NULL) {
        segdef = tdata->last_comdat_seg;
    } else {
        segdef = i386omf_create_comdat_segment(abfd);
        if (segdef == NULL)
            return false;
        tdata->last_comdat_seg = segdef;
        tdata->last_comdat_name_idx = name_idx;
    }

    /* §7 item 1: COMDAT is a valid predecessor for FIXUPP.  */
    tdata->last_leidata = segdef;

    /* 9. Compute write position.
       For continuation (bit 0), the offset is relative to the previous
       COMDAT's data base (segdef->last_data_offset from the first record).
       For non-continuation, the offset is absolute within the section.  */
    bfd_vma write_offset = offset;
    if (flags & 0x01)
        write_offset = segdef->last_data_offset + offset;

    /* Store the absolute section position so FIXUPP (which adds its own
       relative offset to this base) computes correct relocation addresses,
       and so chained continuations accumulate correctly.  */
    segdef->last_data_offset = write_offset;

    /* 10. Enforce 1024-byte max wire data payload (§6.4).
           Iterated data may expand to a larger size after LIDATA expansion.  */
    if (reclen > 1024) {
        _bfd_error_handler("COMDAT at 0x%lx data payload %lu exceeds "
                           "1024 byte maximum",
                           (unsigned long)(rec_start - tdata->image),
                           (unsigned long)reclen);
        bfd_set_error(bfd_error_wrong_format);
        return false;
    }

    /* 11. Copy / expand data payload.  */
    if (flags & 0x02) {
        /* Iterated Data — expand LIDATA-format blocks.  */
        struct i386omf_bytebuf expanded = { NULL, 0, 0 };
        bfd_size_type consumed = 0;

        if (!i386omf_expand_lidata_block(abfd, p, reclen, &consumed,
                                         &expanded, is_32, 0)) {
            free(expanded.data);
            return false;
        }

        if (consumed != reclen) {
            _bfd_error_handler("COMDAT iterated data at 0x%lx has %lu "
                               "leftover byte(s)",
                               (unsigned long)(rec_start - tdata->image),
                               (unsigned long)(reclen - consumed));
            bfd_set_error(bfd_error_wrong_format);
            free(expanded.data);
            return false;
        }

        if (write_offset + expanded.len > segdef->asect->size)
            segdef->asect->size = write_offset + expanded.len;

        if (!i386omf_add_expanded_lidata(abfd, segdef->asect, write_offset,
                                         expanded.data, expanded.len)) {
            free(expanded.data);
            return false;
        }

        free(expanded.data);
    } else {
        /* Enumerated data — raw bytes.  */
        if (write_offset + reclen > segdef->asect->size)
            segdef->asect->size = write_offset + reclen;

        if (!i386omf_add_section_data(abfd, segdef->asect, write_offset,
                                      p, reclen, rectype))
            return false;
    }

    segdef->asect->flags |= SEC_HAS_CONTENTS | SEC_LOAD | SEC_ALLOC;

    if (omf_debug) fprintf(stderr, "COMDAT: flags=0x%02x attr=0x%02x "
                           "alloc_type=%d align_byte=%d offset=0x%lx "
                           "type_idx=%d base_grp=%d base_seg=%d "
                           "base_frm=0x%04x name_idx=%d data=%lu B\n",
                           flags, attributes, alloc_type,
                           (unsigned)align_byte, (unsigned long)offset,
                           type_idx, base_group, base_segment, base_frame,
                           name_idx, (unsigned long)reclen);

    return true;

trunc:
    _bfd_error_handler("Truncated COMDAT record at 0x%lx",
                       (unsigned long)(rec_start - tdata->image));
    bfd_set_error(bfd_error_wrong_format);
    return false;
}

/*
    process_record

SYNOPSIS
    static bool process_record(bfd *abfd, int rectype, bfd_size_type reclen, bfd_byte const *p);

DESCRIPTION
    Dispatches processing of an OMF record based on its type.
    Calls the appropriate handler for each record type and reports errors for unknown types.

    @param abfd    The BFD file handle.
    @param rectype Record type.
    @param reclen  Length of the record data.
    @param p       Pointer to the record data.
    @return        true on success, false on error.
*/
static bool
process_record(bfd *abfd,
               int rectype,
               bfd_size_type reclen,
               bfd_byte const *p) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;
    bool record_ok;
    if (omf_debug) fprintf(stderr, "i386omf process_record rectype: 0x%2x, reclen: %llu\n", rectype, (unsigned long long)reclen);

    /* Clear COMDAT continuation tracking on non-COMDAT, non-FIXUPP, non-LINSYM records.
       This prevents stale re-use if unrelated records appear between two
       continuation COMDATs. LINSYM is excluded because it sits between a COMDAT
       and its associated FIXUPP in the record stream and must not break the chain.  */
    if (rectype != OMF_RECORD_COMDAT && rectype != OMF_RECORD_COMDAT386
        && rectype != OMF_RECORD_FIXUPP && rectype != OMF_RECORD_FIXUPP386
        && rectype != OMF_RECORD_LINSYM && rectype != OMF_RECORD_LINSYM386) {
        tdata->last_comdat_seg = NULL;
        tdata->last_comdat_name_idx = -1;
    }

    switch (rectype) {
        case OMF_RECORD_THEADR: /* Translator header. */
            record_ok = i386omf_read_string(abfd, &tdata->module_name,
                                            p, reclen);
            break;
        case OMF_RECORD_COMENT:
            record_ok = i386omf_read_coment(abfd, p, reclen);
            break;
        case OMF_RECORD_MODEND:
        case OMF_RECORD_MODEND386:
            record_ok = i386omf_read_modend(abfd, p, reclen, rectype & 1);
            break;
        case OMF_RECORD_EXTDEF:
        case OMF_RECORD_LEXTDEF:
        case OMF_RECORD_LEXTDEF386:
            record_ok = i386omf_read_extdef(abfd, p, reclen);
            break;
        case OMF_RECORD_LPUBDEF:
        case OMF_RECORD_LPUBDEF386:
        case OMF_RECORD_PUBDEF:
        case OMF_RECORD_PUBDEF386:
            record_ok = i386omf_read_pubdef(abfd, p, reclen, rectype & 1);
            break;
        case OMF_RECORD_LINNUM:
        case OMF_RECORD_LINNUM386:
            record_ok = true; /* Line numbers record.  Too lazy now. */
            break;
        case OMF_RECORD_LNAMES: /* List of names. */
            record_ok = i386omf_read_lnames(abfd, p, reclen);
            break;
        case OMF_RECORD_SEGDEF:
        case OMF_RECORD_SEGDEF386:
            record_ok = i386omf_read_segdef(abfd, p, reclen, rectype & 1);
            break;
        case OMF_RECORD_GRPDEF:
            record_ok = i386omf_read_grpdef(abfd, p, reclen);
            break;
        /* §1 + §2: FIXUPP records (0x9C/0x9D).
           The caller has already removed the 3-byte header and excluded the
           checksum byte.  is_32bit = rectype & 1 distinguishes 9D vs 9C.  */
        case OMF_RECORD_FIXUPP:
        case OMF_RECORD_FIXUPP386:
            record_ok = i386omf_read_fixupp(abfd, p, reclen, rectype & 1);
            break;
        case OMF_RECORD_LEDATA:
        case OMF_RECORD_LEDATA386:
            record_ok = i386omf_read_leidata(abfd, p, reclen, rectype);
            break;
        case OMF_RECORD_LIDATA:
        case OMF_RECORD_LIDATA386:
            record_ok = i386omf_read_lidata(abfd, p, reclen, rectype);
            break;
        case OMF_RECORD_COMDAT:
        case OMF_RECORD_COMDAT386:
            record_ok = i386omf_read_comdat(abfd, p, reclen, rectype);
            break;
        case OMF_RECORD_LINSYM:
        case OMF_RECORD_LINSYM386:
            record_ok = i386omf_read_linsym(abfd, p, reclen, rectype & 1);
            break;
        case OMF_RECORD_COMDEF:
        case OMF_RECORD_LCOMDEF:
            record_ok = i386omf_read_comdef(abfd, p, reclen);
            break;
        default:
            if (omf_debug)
                fprintf(stderr, "Skipping unrecognized record type 0x%02x at 0x%X\n",
                        rectype,
                        (unsigned int)(p - tdata->image - OMF_RECORD_HEADER));
            record_ok = true;
            break;
    }

    return record_ok;
}

/*
    i386omf_setup_tdata

SYNOPSIS
    static bool i386omf_setup_tdata(bfd *abfd);

DESCRIPTION
    Allocates and initializes all string tables and data structures needed for OMF processing.
    Cleans up on failure.

    @param abfd   The BFD file handle.
    @return       true on success, false on error.
*/
static bool
i386omf_setup_tdata(bfd *abfd) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;
    struct strtab **strtabs[] = {
            &tdata->lnames,
            &tdata->segdef,
            &tdata->grpdef,
            &tdata->externs,
            &tdata->abs_pubdef,
            &tdata->dependencies,
            NULL
    };
    signed int i;

    for (i = 0; strtabs[i] != NULL; i++) {
        *strtabs[i] = strtab_new(abfd);
        if (*strtabs[i] == NULL) {
            /* Unwind all the allocated strtabs, but no others. */
            while (i >= 0) {
                strtab_free(*strtabs[i]);
                i--;
            }
            return false;
        }
    }

    for (i = 0; i < 4; i++) {
        tdata->frame_thread_used[i] = false;
        tdata->target_thread_used[i] = false;
    }

    tdata->last_comdat_name_idx = -1;

    return true;
}

/*
    i386omf_teardown_tdata

SYNOPSIS
    static void i386omf_teardown_tdata(bfd *abfd);

DESCRIPTION
    Frees all string tables and data structures allocated for OMF processing.

    @param abfd   The BFD file handle.
*/
static void
i386omf_teardown_tdata(bfd *abfd) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;
    struct strtab **strtabs[] = {
            &tdata->lnames,
            &tdata->segdef,
            &tdata->grpdef,
            &tdata->externs,
            &tdata->abs_pubdef,
            &tdata->dependencies,
            NULL
    };
    int i;

    /* SEGDEF records refer to sub-strtab objects.  Free them. */
    for (i = 0; i < strtab_size(tdata->segdef); i++) {
        struct i386omf_segment *seg = strtab_lookup(tdata->segdef, i);
        if (seg != NULL) {
            strtab_free(seg->relocs);
            strtab_free(seg->pubdef);
        }
    }

    for (i = 0; strtabs[i] != NULL; i++) {
        strtab_free(*strtabs[i]);
    }

  if (tdata->image)
  {
    free (tdata->image);
    tdata->image = NULL;
  }
}

/* omf_verify_checksum
 *
 * Verifies the OMF record checksum per TIS OMF v1.1 §"The Object Record
 * Format".  The checksum byte is the last byte of the record and is defined
 * as the negative sum (mod 256) of all other bytes, so the byte-wise sum of
 * the entire record must equal 0.
 *
 * A stored checksum of 0x00 is ALWAYS accepted without complaint: the spec
 * explicitly permits compilers to write 0 instead of computing the real value.
 * Real linkers (including LINK386) ignore the checksum entirely, so this
 * function only emits a debug warning; it never sets a BFD error or returns
 * false.
 *
 * @param abfd    The BFD file handle.
 * @param rec     Pointer to the start of the full record (type byte).
 * @param reclen  The 16-bit record length field value as read from the file.
 *                This includes the checksum byte but excludes the 3-byte
 *                header.  reclen == 0 is valid (empty record, no checksum).
 */
static void
omf_verify_checksum (bfd *abfd, bfd_byte const *rec, bfd_size_type reclen)
{
  /* reclen == 0 means the record has no body and no checksum byte.  */
  if (reclen == 0)
    return;

  bfd_byte stored = bfd_get_8 (abfd, rec + OMF_RECORD_HEADER + reclen - 1);

  /* A stored value of 0x00 is always accepted per the TIS spec.  */
  if (stored == 0x00)
    return;

  bfd_size_type total = OMF_RECORD_HEADER + reclen;
  unsigned int sum = 0;
  bfd_size_type i;

  for (i = 0; i < total; i++)
    sum += bfd_get_8 (abfd, rec + i);

  if ((sum & 0xff) != 0)
    {
      struct i386omf_obj_data *tdata = abfd->tdata.any;
      if (omf_debug)
        fprintf (stderr,
                 "OMF checksum mismatch at record 0x%lx "
                 "(type 0x%02x): stored 0x%02x, sum mod 256 = 0x%02x\n",
                 (unsigned long)(rec - tdata->image),
                 (unsigned int) bfd_get_8 (abfd, rec),
                 (unsigned int) stored,
                 sum & 0xff);
    }
}

/*
    i386omf_readobject

SYNOPSIS
    static bool i386omf_readobject(bfd *abfd, bfd_size_type osize, unsigned long *machine);

DESCRIPTION
    Reads the entire OMF object file into memory and processes all records.
    Sets up the machine type and validates the file format.

    @param abfd    The BFD file handle.
    @param osize   Size of the object file.
    @param machine Pointer to store the detected machine type.
    @return        true on success, false on error.
*/
static bool
i386omf_readobject (bfd *abfd, bfd_size_type osize, unsigned long *machine)
{
  struct i386omf_obj_data *tdata = abfd->tdata.any;
  bfd_byte const *p;

  if (bfd_seek (abfd, 0, SEEK_SET) != 0)
    return false;

  tdata->image = _bfd_malloc_and_read (abfd, osize, osize);

  if (tdata->image == NULL)
  {
    bfd_set_error (bfd_error_system_call);
    return false;
  }

  strtab_add (tdata->lnames, NULL);
  strtab_add (tdata->segdef, NULL);
  strtab_add (tdata->grpdef, NULL);
  strtab_add (tdata->externs, NULL);

    /* A quick cheap check for the right file format. */
    if (!osize || bfd_get_8(abfd, tdata->image) != OMF_RECORD_THEADR) {
        bfd_set_error(bfd_error_wrong_format);
        return false;
    }

    for (p = tdata->image; osize > OMF_RECORD_HEADER;) {
        int rectype;
        bfd_size_type reclen;

        rectype = bfd_get_8(abfd, p);
        reclen = bfd_get_16(abfd, p + 1);

        if (rectype & 1 && machine) {
            *machine = bfd_mach_i386_i386;
        }

        if (reclen + OMF_RECORD_HEADER > osize) {
            (*_bfd_error_handler)("Record at 0x%lx overruns input file",
                                  p - tdata->image);
            bfd_set_error(bfd_error_wrong_format);
            return false;
        }

        /* Optional diagnostic checksum verification (never fatal).  */
        omf_verify_checksum (abfd, p, reclen);

        /* §2 + §7 item 11: The OMF length field includes the 1-byte checksum
           trailer.  Subtract 1 to get the body length; the checksum byte is
           otherwise skipped.  */
        if (!process_record(abfd, rectype,
                            reclen ? reclen - 1 : 0, p + OMF_RECORD_HEADER)) {
            switch (bfd_get_error()) {
                case bfd_error_no_error:
                    break;
                case bfd_error_wrong_format:
                    /* Silent exit. */
                    return false;
                default:
                    (*_bfd_error_handler)("process_record() failed at 0x%lx",
                                          p - tdata->image);
                    hexdump(p, reclen + OMF_RECORD_HEADER);
                    (*_bfd_error_handler)("BFD error = %d", bfd_get_error());
                    return false;
            }
        }

        osize -= reclen + OMF_RECORD_HEADER;
        p += reclen + OMF_RECORD_HEADER;
    }

    if (osize > 0) {
        (*_bfd_error_handler)("input file has trailing garbage at 0x%lx",
                              p - tdata->image);
        bfd_set_error(bfd_error_wrong_format);
        return false;
    }

    return true;
}

/*
    i386omf_object_p

SYNOPSIS
    static bfd_cleanup i386omf_object_p(bfd *abfd);

DESCRIPTION
    Checks if the file is a valid i386 OMF object and sets up all necessary data structures.
    Returns a cleanup function pointer or NULL on error.

    @param abfd   The BFD file handle.
    @return       Cleanup function pointer, or NULL on error.
*/
static bfd_cleanup
i386omf_object_p (bfd *abfd)
{
  // struct bfd_preserve preserve;
  struct stat statbuf;
  unsigned long machine = bfd_mach_i386_i8086;;

    abfd->symcount = 0;

    /* Find the file size.  */
    if (bfd_stat(abfd, &statbuf) < 0) {
        bfd_set_error(bfd_error_system_call);
        return NULL;
    }

    abfd->tdata.any = bfd_zalloc(abfd, sizeof(struct i386omf_obj_data));

    if (abfd->tdata.any == NULL) {
        bfd_set_error(bfd_error_no_memory);
        return NULL;
    }

    if (!i386omf_setup_tdata(abfd)) {
        return NULL;
    }

    machine = bfd_mach_i386_i8086;
    if (!i386omf_readobject(abfd, statbuf.st_size, &machine)) {
        /* Tear tdata down before bfd_preserve_restore invalidates it. */
        i386omf_teardown_tdata(abfd);

        return NULL;
    }

    if (bfd_get_arch_info(abfd) == NULL
        || bfd_get_arch_info(abfd)->arch == bfd_arch_unknown)
        bfd_set_arch_info(abfd, bfd_lookup_arch
                (bfd_arch_i386, machine));

    //return abfd->xvec;
    return _bfd_no_cleanup;
}

/*
    i386omf_close_and_cleanup

SYNOPSIS
    static bool i386omf_close_and_cleanup(bfd *abfd);

DESCRIPTION
    Cleans up all data structures and memory allocated for the OMF object.

    @param abfd   The BFD file handle.
    @return       true.
*/
static bool
i386omf_close_and_cleanup(bfd *abfd) {
    i386omf_teardown_tdata(abfd);
    return true;
}

#define i386omf_bfd_free_cached_info  _bfd_generic_bfd_free_cached_info
#define i386omf_new_section_hook      _bfd_generic_new_section_hook

/*
    i386omf_get_section_contents

SYNOPSIS
    static bool i386omf_get_section_contents(bfd* abfd, asection* section, void* location, file_ptr offset, bfd_size_type count);

DESCRIPTION
    Copies section contents into the provided buffer, or zeroes the buffer if the section is not in memory.

    @param abfd     The BFD file handle.
    @param section  The section to read.
    @param location Buffer to copy data into.
    @param offset   Offset in the section.
    @param count    Number of bytes to copy.
    @return         true.
*/
static bool
i386omf_get_section_contents(bfd* abfd ATTRIBUTE_UNUSED, asection* section,
                             void* location, file_ptr offset,
                             bfd_size_type count)
{
  if (section->flags & SEC_IN_MEMORY)
    memcpy(location, section->contents + offset, count);
  else
    memset(location, 0, count);

  return true;
}


/*
    i386omf_get_symtab_upper_bound

SYNOPSIS
    static long i386omf_get_symtab_upper_bound(bfd *abfd);

DESCRIPTION
    Returns the amount of memory needed to read the symbol table.

    @param abfd   The BFD file handle.
    @return       Size in bytes required for the symbol table.
*/
static long
i386omf_get_symtab_upper_bound(bfd *abfd) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;
    struct i386omf_segment *seg;
    long n = 0;
    int i;

    for (i = 1; (seg = strtab_lookup(tdata->segdef, i)) != NULL; i++)
        n += strtab_size(seg->pubdef);
    n += strtab_size(tdata->externs) - 1;

    return n * sizeof(asymbol * );
}

/* Return the symbol table.  */
/*
    i386omf_canonicalize_symtab

SYNOPSIS
    static long i386omf_canonicalize_symtab(bfd *abfd, asymbol **alocation);

DESCRIPTION
    Fills the provided array with pointers to all symbols in the object file.
    Returns the number of symbols found.

    @param abfd      The BFD file handle.
    @param alocation Array to fill with symbol pointers.
    @return          Number of symbols.
*/
static long
i386omf_canonicalize_symtab(bfd *abfd, asymbol **alocation) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;
    struct i386omf_segment *seg;
    struct i386omf_symbol *sym;
    int j;
    long n = 0;

    for (j = 1; (seg = strtab_lookup(tdata->segdef, j)) != NULL; j++) {
        int i;

        for (i = 0; (sym = strtab_lookup(seg->pubdef, i)) != NULL; i++)
            alocation[n++] = &sym->base;
    }

    for (j = 1; (sym = strtab_lookup(tdata->externs, j)) != NULL; j++)
        alocation[n++] = &sym->base;

    abfd->symcount += n;

    return n;
}

#define i386omf_bfd_copy_private_bfd_data \
    _bfd_generic_bfd_copy_private_bfd_data
#define i386omf_bfd_merge_private_bfd_data \
    _bfd_generic_bfd_merge_private_bfd_data
#define i386omf_bfd_copy_private_section_data \
    _bfd_generic_bfd_copy_private_section_data
#define i386omf_bfd_copy_private_symbol_data \
    _bfd_generic_bfd_copy_private_symbol_data
#define i386omf_bfd_copy_private_header_data \
    _bfd_generic_bfd_copy_private_header_data


/*
    i386omf_make_empty_symbol

SYNOPSIS
    static asymbol *i386omf_make_empty_symbol(bfd *abfd);

DESCRIPTION
    Allocates and returns a new, zero-initialized OMF symbol structure.

    @param abfd   The BFD file handle.
    @return       Pointer to the new symbol.
*/
static asymbol *
i386omf_make_empty_symbol(bfd *abfd) {
    bfd_size_type amt = sizeof(struct i386omf_symbol);
    asymbol *new = bfd_zalloc(abfd, amt);
    if (new)
        new->the_bfd = abfd;
    return new;
}

/*
    i386omf_print_symbol

SYNOPSIS
    static void i386omf_print_symbol(bfd *abfd, void *afile, struct bfd_symbol *sym, bfd_print_symbol_type how);

DESCRIPTION
    Prints information about a symbol in various formats, depending on the 'how' parameter.

    @param abfd   The BFD file handle.
    @param afile  File stream to print to.
    @param sym    Symbol to print.
    @param how    Print format selector.
*/
static void
i386omf_print_symbol(bfd *abfd, void *afile, struct bfd_symbol *sym, bfd_print_symbol_type how) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;
    struct i386omf_symbol *bigsym = (struct i386omf_symbol *) sym;
    char const *groupname;
    FILE *f = afile;

    switch (how) {
        case bfd_print_symbol_name:
        default:
            if (sym->name)
                fprintf(f, "%s%s", sym->name, tdata->has_start_address ? "" : "");
            break;
        case bfd_print_symbol_more:
            fprintf(f, "%3d", bigsym->type_index);
            break;
        case bfd_print_symbol_all:
            bfd_print_symbol_vandf(abfd, (void *) f, sym);
            if (bigsym->group)
                groupname = i386omf_lookup_string(tdata->lnames,
                                                  bigsym->group->name_index, "");
            else
                groupname = "";
            fprintf(f, " %-16s %-10s %3d", sym->name, groupname,
                    bigsym->type_index);
            break;
    }

    /* TODO: Check that base group is sane. */
}

/* Get information about a symbol.  */
/*
    i386omf_get_symbol_info

SYNOPSIS
    static void i386omf_get_symbol_info(bfd *ignore_abfd ATTRIBUTE_UNUSED, asymbol *symbol, symbol_info *ret);

DESCRIPTION
    Fills a symbol_info structure with information about the given symbol.

    @param ignore_abfd Ignored BFD file handle.
    @param symbol      Symbol to query.
    @param ret         Structure to fill with symbol information.
*/
static void
i386omf_get_symbol_info(bfd *ignore_abfd ATTRIBUTE_UNUSED,
                        asymbol *symbol,
                        symbol_info *ret) {
    bfd_symbol_info(symbol, ret);
}

#define i386omf_bfd_is_local_label_name     bfd_generic_is_local_label_name
#define i386omf_get_lineno                 _bfd_nosymbols_get_lineno
#define i386omf_find_nearest_line          _bfd_nosymbols_find_nearest_line
#define i386omf_find_inliner_info          _bfd_nosymbols_find_inliner_info
#define i386omf_bfd_make_debug_symbol      _bfd_nosymbols_bfd_make_debug_symbol
#define i386omf_read_minisymbols           _bfd_generic_read_minisymbols
#define i386omf_minisymbol_to_symbol       _bfd_generic_minisymbol_to_symbol
#define i386omf_bfd_is_target_special_symbol _bfd_bool_bfd_asymbol_false

/*
    i386omf_get_reloc_upper_bound

SYNOPSIS
    static long i386omf_get_reloc_upper_bound(bfd *abfd ATTRIBUTE_UNUSED, asection *sec);

DESCRIPTION
    Returns the amount of memory needed to read the relocation table for a section.

    @param abfd   The BFD file handle (unused).
    @param sec    Section to query.
    @return       Size in bytes required for the relocation table.
*/
static long
i386omf_get_reloc_upper_bound(bfd *abfd ATTRIBUTE_UNUSED, asection *sec) {
    struct i386omf_segment *seg = sec->used_by_bfd;
    long n = strtab_size(seg->relocs);

    return n * sizeof(arelent * );
}

/*
    i386omf_canonicalize_reloc

SYNOPSIS
    static long i386omf_canonicalize_reloc(bfd *abfd ATTRIBUTE_UNUSED, asection *sec, arelent **relptr, asymbol **symbols ATTRIBUTE_UNUSED);

DESCRIPTION
    Fills the provided array with pointers to all relocation entries for a section.
    Returns the number of relocations found.

    @param abfd     The BFD file handle (unused).
    @param sec      Section to query.
    @param relptr   Array to fill with relocation pointers.
    @param symbols  Array of symbols (unused).
    @return         Number of relocations.
*/
static long
i386omf_canonicalize_reloc(bfd *abfd ATTRIBUTE_UNUSED,
                           asection *sec,
                           arelent **relptr,
                           asymbol **symbols ATTRIBUTE_UNUSED) {
    struct i386omf_segment *seg = sec->used_by_bfd;
    struct i386omf_relent *relent;
    long n = 0;
    int i;

    for (i = 0; (relent = strtab_lookup(seg->relocs, i)) != NULL; i++)
        relptr[n++] = &relent->base;

    return n;
}

#define i386omf_bfd_reloc_type_lookup bfd_default_reloc_type_lookup
#define i386omf_bfd_reloc_name_lookup _bfd_norelocs_bfd_reloc_name_lookup

/* Set the architecture of a binary file.  */
#define binary_set_arch_mach _bfd_generic_set_arch_mach

static bool i386omf_write_object_contents(bfd *abfd) {
    if (omf_debug) fprintf(stderr, "i386omf_write_object_contents NOT IMPLEMENTED %s\n", abfd->filename);
    bfd_set_error(bfd_error_invalid_operation);
    return false;
}

/* Write section contents of a binary file.  */
static bool
binary_set_section_contents(bfd *abfd,
                            asection *sec,
                            const void *data,
                            file_ptr offset,
                            bfd_size_type size) {
    if (size == 0)
        return true;

    if (!abfd->output_has_begun) {
        bool found_low;
        bfd_vma low;
        asection *s;

        /* The lowest section LMA sets the virtual address of the start
           of the file.  We use this to set the file position of all the
           sections.  */
        found_low = false;
        low = 0;
        for (s = abfd->sections; s != NULL; s = s->next)
            if (((s->flags
                  & (SEC_HAS_CONTENTS | SEC_LOAD | SEC_ALLOC | SEC_NEVER_LOAD))
                 == (SEC_HAS_CONTENTS | SEC_LOAD | SEC_ALLOC))
                && (s->size > 0)
                && (!found_low || s->lma < low)) {
                low = s->lma;
                found_low = true;
            }

        for (s = abfd->sections; s != NULL; s = s->next) {
            s->filepos = s->lma - low;

            /* Skip following warning check for sections that will not
               occupy file space.  */
            if ((s->flags
                 & (SEC_HAS_CONTENTS | SEC_ALLOC | SEC_NEVER_LOAD))
                != (SEC_HAS_CONTENTS | SEC_ALLOC)
                || (s->size == 0))
                continue;

            /* If attempting to generate a binary file from a bfd with
               LMA's all over the place, huge (sparse?) binary files may
               result.  This condition attempts to detect this situation
               and print a warning.  Better heuristics would be nice to
               have.  */

            if (s->filepos < 0)
                if (omf_debug) fprintf(stderr,
                        "Warning: Writing section `%s' to huge (ie negative) file offset 0x%lx.\n",
                        bfd_section_name(sec),
                        (unsigned long) s->filepos);
        }

        abfd->output_has_begun = true;
    }

    /* We don't want to output anything for a section that is neither
       loaded nor allocated.  The contents of such a section are not
       meaningful in the binary format.  */
    if ((sec->flags & (SEC_LOAD | SEC_ALLOC)) == 0)
        return true;
    if ((sec->flags & SEC_NEVER_LOAD) != 0)
        return true;

    return _bfd_generic_set_section_contents(abfd, sec, data, offset, size);
}

/* No space is required for header information.  */

static int
binary_sizeof_headers(bfd* abfd ATTRIBUTE_UNUSED,
                      struct bfd_link_info* info ATTRIBUTE_UNUSED)
{
  return 0;
}

#define binary_bfd_get_relocated_section_contents  bfd_generic_get_relocated_section_contents
#define binary_bfd_relax_section                   bfd_generic_relax_section
#define binary_bfd_gc_sections                     bfd_generic_gc_sections
#define binary_bfd_merge_sections                  bfd_generic_merge_sections
#define binary_bfd_is_group_section                bfd_generic_is_group_section
#define binary_bfd_discard_group                   bfd_generic_discard_group
#define binary_section_already_linked             _bfd_generic_section_already_linked
#define binary_bfd_define_common_symbol            bfd_generic_define_common_symbol
#define binary_bfd_link_hash_table_create         _bfd_generic_link_hash_table_create
#define binary_bfd_link_hash_table_free           _bfd_generic_link_hash_table_free
#define binary_bfd_link_just_syms                 _bfd_generic_link_just_syms
#define binary_bfd_copy_link_hash_symbol_type \
  _bfd_generic_copy_link_hash_symbol_type
#define binary_bfd_link_add_symbols               _bfd_generic_link_add_symbols
#define binary_bfd_final_link                     _bfd_generic_final_link
#define binary_bfd_link_split_section             _bfd_generic_link_split_section
#define i386omf_get_section_contents_in_window    _bfd_generic_get_section_contents_in_window
#define binary_bfd_lookup_section_flags            bfd_generic_lookup_section_flags
#define binary_bfd_link_hide_symbol               _bfd_generic_link_hide_symbol
#define binary_bfd_group_name                      bfd_generic_group_name
#define i386omf_finalize_section_relocs            _bfd_norelocs_finalize_section_relocs
#define binary_bfd_define_start_stop               bfd_generic_define_start_stop
#define i386omf_find_line                         _bfd_nosymbols_find_line
#define i386omf_find_nearest_line_with_alt        _bfd_nosymbols_find_nearest_line_with_alt
#define i386omf_get_symbol_version_string \
  _bfd_nosymbols_get_symbol_version_string
#define binary_bfd_link_check_relocs              _bfd_generic_link_check_relocs

const bfd_target i386_omf_vec = {
    "i386omf",                           /* name (const char *name) */
    bfd_target_omf_flavour,              /* flavour (enum bfd_flavour flavour) */
    BFD_ENDIAN_LITTLE,                   /* byteorder (enum bfd_endian byteorder) */
    BFD_ENDIAN_LITTLE,                   /* header_byteorder (enum bfd_endian header_byteorder) */
    (HAS_RELOC | HAS_SYMS | HAS_LOCALS), /* object_flags (flagword object_flags) */
    (SEC_ALLOC | SEC_LOAD | SEC_RELOC | SEC_READONLY | SEC_CODE
     | SEC_DATA | SEC_ROM | SEC_HAS_CONTENTS | SEC_IN_MEMORY
     | SEC_GROUP),                      /* section_flags (flagword section_flags) */
    0,                                  /* symbol_leading_char (char symbol_leading_char) */
    ' ',                                /* ar_pad_char (char ar_pad_char) */
    16,                                 /* ar_max_namelen (unsigned char ar_max_namelen) */
    255,                                /* match_priority (unsigned char match_priority) */
    TARGET_KEEP_UNUSED_SECTION_SYMBOLS, /* keep_unused_section_symbols (bool keep_unused_section_symbols) */
    TARGET_MERGE_SECTIONS,             /* merge_sections (bool merge_sections) */

    /* Data byte swapping functions (for user section data) */
    bfd_getl64,         /* bfd_getx64 (uint64_t (*bfd_getx64)(const void *)) */
    bfd_getl_signed_64, /* bfd_getx_signed_64 (int64_t (*bfd_getx_signed_64)(const void *)) */
    bfd_putl64,         /* bfd_putx64 (void (*bfd_putx64)(uint64_t, void *)) */
    bfd_getl32,         /* bfd_getx32 (bfd_vma (*bfd_getx32)(const void *)) */
    bfd_getl_signed_32, /* bfd_getx_signed_32 (bfd_signed_vma (*bfd_getx_signed_32)(const void *)) */
    bfd_putl32,         /* bfd_putx32 (void (*bfd_putx32)(bfd_vma, void *)) */
    bfd_getl16,         /* bfd_getx16 (bfd_vma (*bfd_getx16)(const void *)) */
    bfd_getl_signed_16, /* bfd_getx_signed_16 (bfd_signed_vma (*bfd_getx_signed_16)(const void *)) */
    bfd_putl16,         /* bfd_putx16 (void (*bfd_putx16)(bfd_vma, void *)) */

    /* Header byte swapping functions (for file header data) */
    bfd_getl64,         /* bfd_h_getx64 (uint64_t (*bfd_h_getx64)(const void *)) */
    bfd_getl_signed_64, /* bfd_h_getx_signed_64 (int64_t (*bfd_h_getx_signed_64)(const void *)) */
    bfd_putl64,         /* bfd_h_putx64 (void (*bfd_h_putx64)(uint64_t, void *)) */
    bfd_getl32,         /* bfd_h_getx32 (bfd_vma (*bfd_h_getx32)(const void *)) */
    bfd_getl_signed_32, /* bfd_h_getx_signed_32 (bfd_signed_vma (*bfd_h_getx_signed_32)(const void *)) */
    bfd_putl32,         /* bfd_h_putx32 (void (*bfd_h_putx32)(bfd_vma, void *)) */
    bfd_getl16,         /* bfd_h_getx16 (bfd_vma (*bfd_h_getx16)(const void *)) */
    bfd_getl_signed_16, /* bfd_h_getx_signed_16 (bfd_signed_vma (*bfd_h_getx_signed_16)(const void *)) */
    bfd_putl16,         /* bfd_h_putx16 (void (*bfd_h_putx16)(bfd_vma, void *)) */

    /* Format checkers: _bfd_check_format (bool (*_bfd_check_format[4])(bfd *)) */
    {
        _bfd_dummy_target,      /* unknown format */
        i386omf_object_p,       /* object file format checker */
        _bfd_dummy_target,      /* archive format checker */
        _bfd_dummy_target,      /* core format checker */
    },

    /* Format setters: _bfd_set_format (bool (*_bfd_set_format[4])(bfd *)) */
    {
        _bfd_bool_bfd_false_error, /* unknown format */
        binary_mkobject,           /* object file format setter */
        _bfd_bool_bfd_false_error, /* archive format setter */
        _bfd_bool_bfd_false_error, /* core format setter */
    },

    /* Format writers: _bfd_write_contents (bool (*_bfd_write_contents[4])(bfd *)) */
    {
        _bfd_bool_bfd_false_error,     /* unknown format */
        i386omf_write_object_contents, /* object file writer */
        _bfd_bool_bfd_false_error,     /* archive writer */
        _bfd_bool_bfd_false_error,     /* core writer */
    },

    /* Jump tables for various BFD operations */
    BFD_JUMP_TABLE_GENERIC(i386omf),    /* _close_and_cleanup, _bfd_free_cached_info, _new_section_hook, _bfd_get_section_contents */
    BFD_JUMP_TABLE_COPY(_bfd_generic),  /* _bfd_copy_private_bfd_data, _bfd_merge_private_bfd_data, ... */
    BFD_JUMP_TABLE_CORE(_bfd_nocore),   /* _core_file_failing_command, ... */
    BFD_JUMP_TABLE_ARCHIVE(_bfd_noarchive), /* _bfd_slurp_armap, ... */
    BFD_JUMP_TABLE_SYMBOLS(i386omf),    /* _bfd_get_symtab_upper_bound, _bfd_canonicalize_symtab, ... */
    BFD_JUMP_TABLE_RELOCS(i386omf),     /* _get_reloc_upper_bound, _bfd_canonicalize_reloc, ... */
    BFD_JUMP_TABLE_WRITE(binary),       /* _bfd_set_arch_mach, _bfd_set_section_contents */
    BFD_JUMP_TABLE_LINK(binary),        /* _bfd_sizeof_headers, _bfd_get_relocated_section_contents, ... */
    BFD_JUMP_TABLE_DYNAMIC(_bfd_nodynamic), /* dynamic symbol/reloc routines */

    NULL, /* alternative_target (const struct bfd_target *alternative_target) */
    NULL  /* backend_data (const void *backend_data) */
};