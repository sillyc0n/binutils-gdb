/* BFD back-end for ix86 OMF objects.
   Copyright 2007 Free Software Foundation, Inc.
   Written by Bernd Jendrissek <bernd.jendrissek@gmail.com>
   Based on bfd/binary.c.

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

#define OMF_FIXUPP_FIXUP               0x80
#define OMF_FIXUPP_TARGET_SEGDEF       0        // segment index and target displacement
#define OMF_FIXUPP_TARGET_GRPDEF       1        // group index and target displacement
#define OMF_FIXUPP_TARGET_EXTDEF       2        // external index and target displacement
#define OMF_FIXUPP_TARGET_EXPLICIT     3        // explicit frame number
#define OMF_FIXUPP_TARGET_NODISP       4        // has displacement if bit set

#define OMF_FIXUPP_FRAME_SEGDEF        0
#define OMF_FIXUPP_FRAME_GRPDEF        1
#define OMF_FIXUPP_FRAME_EXTDEF        2
#define OMF_FIXUPP_FRAME_EXPLICIT      3
#define OMF_FIXUPP_FRAME_LEIDATA       4
#define OMF_FIXUPP_FRAME_TARGET        5

#define OMF_FIXUP_SEGREL               0x40     // 0100 0000
#define OMF_FIXUP_LOCATION_MASK        0x3c     // 0011 1100
#define OMF_FIXUP_LOCATION_SHIFT       2
#define OMF_FIX_DATA_FRAME_THREAD      0x80     // 1000 0000
#define OMF_FIX_DATA_FRAME_MASK        0x70     // 0111 0000
#define OMF_FIX_DATA_FRAME_SHIFT       4
#define OMF_FIX_DATA_TARGET_THREAD     0x08
#define OMF_FIX_DATA_P_MASK            0x04
#define OMF_FIX_DATA_TARGT_MASK        0x03
#define OMF_FIX_DATA_TARGET_METHOD_MASK \
  (OMF_FIX_DATA_TARGT_MASK | OMF_FIX_DATA_P_MASK)

#define OMF_FIXUP_THREAD_DATA_D_FIELD_MASK    0x40
#define OMF_FIXUP_THREAD_DATA_D_FIELD_SHIFT   6
#define OMF_FIXUP_THREAD_DATA_METHOD_MASK     0x1c
#define OMF_FIXUP_THREAD_DATA_METHOD_SHIFT    0x10
#define OMF_FIXUP_THREAD_DATA_THREAD_NUMBER   0x3

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

#define W2M(x) ((1 << (x)) - 1)

struct i386omf_symbol;

enum i386omf_offset_size {
    I386OMF_OFFSET_SIZE_16,
    I386OMF_OFFSET_SIZE_32,
};

struct counted_string {
    bfd_size_type len;
    char *data;
};

struct i386omf_segment {
    struct bfd_section *asect;
    struct strtab *relocs;
    struct strtab *pubdef;
    int combination;
    int name_index;
    int class_index;
    int overlay_index;
};

struct i386omf_group_entry {
    enum {
        GRPDEF_ENTRY_SEGDEF = 0xff,
    } type;
    union {
        int segdef;
    } u;
};

struct i386omf_group {
    int name_index;
    struct strtab *entries;
    struct strtab *pubdef;
    struct i386omf_symbol *symbol;
};

struct i386omf_symbol {
    asymbol base;
    struct counted_string name;
    int type_index;
    struct i386omf_segment *seg;
    struct i386omf_group *group;
};

struct i386omf_obj_data {
  bfd_byte *image;
  char *translator;
  struct counted_string module_name;
  bool is_main_module;
    bool has_start_address;
    struct strtab *lnames;
    struct strtab *segdef;
    struct strtab *grpdef;
    struct strtab *externs;
    struct strtab *abs_pubdef;
    struct strtab *dependencies;
    struct i386omf_segment *last_leidata;
    struct strtab *fixup_threads;
};

struct i386omf_relent {
    arelent base;
    asymbol *symbol;
};

struct i386_fixup_thread {
    int index;
    int thread_number;
    bool is_frame;
    int method;
};

enum reloc_type {
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

struct i386omf_borland_dependency {
    struct counted_string filename;
    int time;
    int date;
};

static const char *const thread_method[8] = {
        "Segment Index + Displacement (SEGDEF)",
        "Group Index + Displacement (GRPDEF)",
        "External Index + Displacement (EXTDEF)",
        "Frame number",
        "Segment Index (SEGDEF)",
        "Group Index (GRPDEF)",
        "External Index (EXTDEF)",
        "Frame number"
};

static bfd_reloc_status_type
i386omf_fix_wrt_frame(bfd *abfd,
                      arelent *reloc_entry,
                      asymbol *symbol,
                      void *data,
                      asection *input_section,
                      bfd *output_bfd,
                      char **error_message);

/**
 * Record type 9DH is new for LINK386; it has a Target Displacement field
 * of 32 bits rather than 16 bits, and the Location field of the Locat
 * word has been extended to 4 bits (using the previously unused higher
 * order S bit) to allow new LOCATION values of 9, 11, and 13.
 */
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
reloc_howto_type howto_wrt_segdef =
        HOWTO(R_I386OMF_WRT_FRAME, 0, 3, 16, false, 0, complain_overflow_bitfield, &i386omf_fix_wrt_frame,
              "WRTSEG", false, 0xffff, 0xffff, false);

static void
hexdump(bfd_byte const *p, bfd_size_type len) {
    bfd_size_type i;
    char *s;
    size_t amt;

    /* XXX - 1000 is the size of _bfd_default_error_handler()'s buffer. */
    if (len > 1000 / 3) {
        (*_bfd_error_handler)("(truncated hexdump)");
        len = 1000 / 3;
    }

    if (_bfd_mul_overflow (len + 1, 3, &amt)) {
        bfd_set_error(bfd_error_file_too_big);
    }

    s = bfd_malloc(amt); /* +1 for NUL. */
    if (s == NULL)
        return;
    for (i = 0; i < len; i++) {
        sprintf(s + i * 3, " %02llx", bfd_get_8(abfd, p + i));
    }
    (*_bfd_error_handler)(s);
    free(s);
}

static bool
i386omf_read_index(bfd *abfd,
                   int *idx,
                   bfd_byte const **p,
                   bfd_size_type *reclen) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;
    int v;

    if (*reclen < 1) {
        (*_bfd_error_handler)("Index truncated at 0x%lx.", *p - tdata->image);
        bfd_set_error(bfd_error_wrong_format);
        return false;
    }

    v = *(*p)++;
    (*reclen)--;
    if (v & OMF_INDEX_2BYTES) {
        if (*reclen < 1) {
            (*_bfd_error_handler)("Index truncated at 0x%lx.",
                                  *p - tdata->image);
            bfd_set_error(bfd_error_wrong_format);
            return false;
        }
        v = (v & OMF_INDEX_LOWMASK) * 256 + *(*p)++;
        (*reclen)--;
    }

    *idx = v;

    return true;
}

static bool
i386omf_read_offset(bfd *abfd,
                    bfd_vma *offset,
                    bfd_byte const **p,
                    bfd_size_type *reclen,
                    enum i386omf_offset_size sz) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;
    bfd_size_type offset_len = 0;

    switch (sz) {
        case I386OMF_OFFSET_SIZE_16:
            offset_len = 2;
            break;
        case I386OMF_OFFSET_SIZE_32:
            offset_len = 4;
            break;
    }

    /* TODO: Handle 32-bit OMF records. */
    if (*reclen < offset_len) {
        (*_bfd_error_handler)("Offset truncated at 0x%lx.", *p - tdata->image);
        bfd_set_error(bfd_error_wrong_format);
        return false;
    }

    if (offset) {
        switch (sz) {
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

static bfd_size_type
i386omf_read_string(bfd *abfd,
                    struct counted_string *s,
                    bfd_byte const *p,
                    bfd_size_type reclen) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;
    bfd_size_type slen = *p;

    if (slen + 1 > reclen) {
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

static char const *
i386omf_lookup_string(struct strtab *tab, int i, char const *def) {
    struct counted_string *s;

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

static bool
binary_mkobject(bfd *abfd ATTRIBUTE_UNUSED) {
    return true;
}

static bool
i386omf_read_coment(bfd *abfd, bfd_byte const *p, bfd_size_type reclen) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;
    int comment_type, comment_class;

    if (reclen < OMF_RECORD_HEADER_COMENT) {
        (*_bfd_error_handler)("Truncated COMENT record.");
        bfd_set_error(bfd_error_wrong_format);
        return false;
    }

    comment_type = bfd_get_8(abfd, p + 0);
    comment_class = bfd_get_8(abfd, p + 1);
    p += OMF_RECORD_HEADER_COMENT;
    reclen -= OMF_RECORD_HEADER_COMENT;

    switch (comment_class) {
        case OMF_COMENT_TRANSLATOR:
            if (tdata->translator)
                (*_bfd_error_handler)("Translator already set to %s",
                                      tdata->translator);
            if (reclen && !ISPRINT(bfd_get_8(abfd, p))
                && bfd_get_8(abfd, p) == reclen - 1) {
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
            strncpy(tdata->translator, (char const *) p, reclen);
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
            while (reclen) {
                struct i386omf_borland_dependency *dep;
                bfd_size_type slen;

                if (reclen < 5) {
                    (*_bfd_error_handler)("Truncated Borland dependency list at 0x%x",
                                          p - tdata->image);
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
            (*_bfd_error_handler)("Unknown record COMENT type: 0x%02x class: 0x%02x at 0x%04x",
                                  comment_type, comment_class, p - tdata->image - 1);
            bfd_set_error(bfd_error_wrong_format);
            return false;
    }

    return true;
}

static bool
i386omf_read_modend(bfd *abfd, bfd_byte const *p, bfd_size_type reclen) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;

    if (reclen < OMF_RECORD_HEADER_MODEND) {
        (*_bfd_error_handler)("Truncated MODEND record.");
        bfd_set_error(bfd_error_wrong_format);
        return false;
    }

    tdata->is_main_module = *p & OMF_MODEND_MAIN_MODULE ? true : false;
    tdata->has_start_address = *p & OMF_MODEND_START_ADDRESS ? true : false;

    if (*p & ~(OMF_MODEND_MAIN_MODULE | OMF_MODEND_START_ADDRESS)) {
        (*_bfd_error_handler)("Too much cleverness in MODEND record.");
        hexdump(p, reclen);
    }

    return true;
}

static bool
i386omf_read_comdef(bfd *abfd, bfd_byte const *p, bfd_size_type reclen) {
    fprintf(stderr, "i386omf_read_comdef: %p\n", p);

    struct i386omf_obj_data *tdata = abfd->tdata.any;

    while (reclen) {
        struct i386omf_symbol *extdef;
        bfd_size_type slen, dsize, dnum;
        int data_segment_type;

        extdef = bfd_alloc(abfd, sizeof(*extdef));
        if (extdef == NULL)
            return false;

        extdef = (struct i386omf_symbol *) bfd_make_empty_symbol(abfd);
        abfd->flags |= HAS_SYMS;

        slen = i386omf_read_string(abfd, &extdef->name, p, reclen);
        if (slen < 1) {
            _bfd_error_handler("COMDEF read error: 0x%llx", (unsigned long long)slen);
            bfd_set_error(bfd_error_wrong_format);
            return false;
        }
        fprintf(stderr, "COMDEF read: 0%s", extdef->name.data);
        p += slen;
        reclen -= slen;

        if (!i386omf_read_index(abfd, &extdef->type_index, &p, &reclen))
            return false;

        data_segment_type = bfd_get_8(abfd, p++);

        if (data_segment_type == OMF_COMDEF_DATA_SEG_TYPE_FAR) {
            dnum = bfd_get_8(abfd, p++);
            reclen -= 1;
        }

        dsize = bfd_get_8(abfd, p++);
        reclen -= 2;

        extdef->base.name = extdef->name.data;
        /* Maybe? extdef->base.flags |= BSF_WEAK; */
        /* extdef->base.flags |= SEC_ALLOC;*/
        extdef->base.value = 0;
        extdef->seg = NULL;
        extdef->base.section = bfd_und_section_ptr;
        // extdef->base.section = bfd_com_section_ptr;

        strtab_add(tdata->externs, extdef);
    }

    return true;

    /*External    Ordered by occurrence of EXTDEF, COMDEF,
    Symbols     LEXTDEF, and LCOMDEF records and symbols
               within each. Referenced as an external name
               index (in FIXUP subrecords).


     COMDEF, LCOMDEF, EXTDEF, LEXTDEF, and CEXTDEF records
    NOTE: This group of records is indexed together, so external name
    index fields in FIXUPP records may refer to any of the record
    types listed.
               */
}

static bool
i386omf_read_extdef(bfd *abfd, bfd_byte const *p, bfd_size_type reclen) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;

    while (reclen) {
        struct i386omf_symbol *extdef;
        bfd_size_type slen;

        extdef = bfd_alloc(abfd, sizeof(*extdef));
        if (extdef == NULL)
            return false;

        extdef = (struct i386omf_symbol *) bfd_make_empty_symbol(abfd);
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

static bool
i386omf_read_pubdef(bfd *abfd, bfd_byte const *p, bfd_size_type reclen, int is32) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;
    int base_group, base_segment;
    bfd_vma base_frame = 0;

    if (!i386omf_read_index(abfd, &base_group, &p, &reclen))
        return false;
    if (!i386omf_read_index(abfd, &base_segment, &p, &reclen))
        return false;
    if (base_segment == OMF_PUBDEF_SEGMENT_ABSOLUTE) {
        /* Rarely used absolute-address symbol. */
        if (reclen < 2) {
            (*_bfd_error_handler)("Truncated base frame in PUBDEF at 0x%X",
                                  p - tdata->image);
            bfd_set_error(bfd_error_wrong_format);
            return false;
        }
        base_frame = bfd_get_16(abfd, p);
        p += 2;
        reclen -= 2;
        (*_bfd_error_handler)("PUBDEF with base frame 0x%04x",
                              (unsigned int) base_frame);
    }

    while (reclen) {
        struct i386omf_symbol *pubdef;
        bfd_size_type slen;
        bfd_vma offset;

        pubdef = (struct i386omf_symbol *) bfd_make_empty_symbol(abfd);
        abfd->flags |= HAS_SYMS;

        slen = i386omf_read_string(abfd, &pubdef->name, p, reclen);
        if (slen < 1)
            return false;
        p += slen;
        reclen -= slen;
        if (!i386omf_read_offset(abfd, &offset, &p, &reclen,
                                 is32 ? I386OMF_OFFSET_SIZE_32 : I386OMF_OFFSET_SIZE_16))
            return false;
        if (!i386omf_read_index(abfd, &pubdef->type_index, &p, &reclen))
            return false;

        pubdef->base.name = pubdef->name.data;
        pubdef->base.flags |= BSF_GLOBAL;
        pubdef->base.value = base_frame * 16 + offset;
        pubdef->seg = strtab_lookup(tdata->segdef, base_segment);
        if (pubdef->seg)
            pubdef->base.section = pubdef->seg->asect;
        else
            pubdef->base.section = bfd_und_section_ptr;
        pubdef->group = strtab_lookup(tdata->grpdef, base_group);

        if (base_segment != OMF_PUBDEF_SEGMENT_ABSOLUTE) {
            /* Normal segment-relative exported symbol. */
            struct i386omf_segment *seg;

            seg = strtab_lookup(tdata->segdef, base_segment);
            if (seg == NULL) {
                (*_bfd_error_handler)("PUBDEF %s in unknown SEGDEF %d",
                                      pubdef->base.name, base_segment);
                bfd_set_error(bfd_error_wrong_format);
                return false;
            }
            strtab_add(seg->pubdef, pubdef);
        } else if (base_group != OMF_GRPDEF_NONE) {
            /* Rather more weird: relative to a group, but no segment? */
            struct i386omf_group *group;

            group = strtab_lookup(tdata->grpdef, base_group);
            if (group == NULL) {
                (*_bfd_error_handler)("PUBDEF %s in unknown GRPDEF %d",
                                      pubdef->base.name, base_group);
                bfd_set_error(bfd_error_wrong_format);
                return false;
            }
            strtab_add(group->pubdef, pubdef);

            if (base_frame) {
                (*_bfd_error_handler)("PUBDEF %s has nonzero base frame 0x%04x",
                                      pubdef->base.name, base_frame);
            }
        } else {
            /* Absolute exported symbol. */
            strtab_add(tdata->abs_pubdef, pubdef);
        }
    }

    return true;
}

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

static bool
i386omf_read_segdef(bfd *abfd, bfd_byte const *p, bfd_size_type reclen, int is32) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;
    int segdefs_seen = 0;

    while (reclen) {
        const unsigned int alignment_powers[] = {0, 0, 1, 4, 8, 2, 12, -1};
        int alignment, combination;
        int name_index, class_index, overlay_index;
        struct i386omf_segment *seg;
        struct i386omf_symbol *seg_sym;
        char const *segment_name;
        bfd_vma absolute_addr, seglen;
        bfd_byte attr;

        attr = *p;

        alignment = (attr & OMF_SEGDEF_ALIGNMENT_MASK) >> OMF_SEGDEF_ALIGNMENT_SHIFT;
        combination = (attr & OMF_SEGDEF_COMBINATION_MASK) >> OMF_SEGDEF_COMBINATION_SHIFT;

        if (alignment == OMF_SEGDEF_ALIGNMENT_ABSOLUTE) {
            /* Absolute segment; get frame number and offset. */
            absolute_addr = (bfd_vma) bfd_get_16(abfd, p + 1) * 16;
            absolute_addr += bfd_get_8(abfd, p + 3);

            if (reclen < 4) {
                (*_bfd_error_handler)("SEGDEF at 0x%lx is truncated, only %u bytes remain.",
                                      p - tdata->image, reclen);
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
        if (!i386omf_read_index(abfd, &name_index, &p, &reclen))
            return false;
        if (!i386omf_read_index(abfd, &class_index, &p, &reclen))
            return false;
        if (!i386omf_read_index(abfd, &overlay_index, &p, &reclen))
            return false;

        if (alignment == OMF_SEGDEF_ALIGNMENT_UNDEFINED) {
            (*_bfd_error_handler)("Segment %d (%s) wants alignment = 7",
                                  name_index,
                                  strtab_lookup(tdata->lnames, name_index));
            bfd_set_error(bfd_error_wrong_format);
            return false;
        }

        seg = bfd_alloc(abfd, sizeof(*seg));
        if (seg == NULL)
            return false;
        seg->combination = combination;
        seg->name_index = name_index;
        seg->class_index = class_index;
        seg->overlay_index = overlay_index;
        //(*_bfd_error_handler) ("SEGDEF name_index: %x, class_index: %x, overlay_index: %x\n", name_index, class_index, overlay_index);
        _bfd_error_handler(_("SEGDEF name_index:  %x, class_index: %x, overlay_index: %x"), name_index, class_index,
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
        (*_bfd_error_handler)("SEGDEF record doesn't contain exactly one segment definition");

    return true;
}

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

/**
 * @brief 
 * 
 * @param abfd - The binary file descriptor.
 * @param p - A pointer to the raw data of the fixup record.
 * @param reclen - The length of the raw data in the fixup record.
 * @return bool 
 */
static bool
i386omf_read_fixupp(bfd *abfd, bfd_byte const *p, bfd_size_type reclen) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;
    bfd_byte const *q;

    while (reclen) {
        int subrec;

        subrec = bfd_get_8(abfd,
                           p);                                         // Reads the next byte from p as the subrecord type (subrec). This byte indicates the type of fixup record being processed.
        if (subrec & OMF_FIXUPP_FIXUP) {    // this is a fixup field
            int location, fixdata;
            int frame_method, frame = 0, target_method, target = 0;
            bfd_size_type offset, displacement = 0;
            struct i386omf_relent *target_relent, *frame_relent;
            struct i386omf_symbol *sym, *frame_sym;
            reloc_howto_type *howto;

            if (tdata->last_leidata == NULL) {
                _bfd_error_handler(_("FIXUP record without LEIDATA"));
                bfd_set_error(bfd_error_wrong_format);
                return false;
            }

            if (reclen < 3) {
                (*_bfd_error_handler)("FIXUP subrecord truncated at 0x%lx.",
                                      p - tdata->image);
                bfd_set_error(bfd_error_wrong_format);
                return false;
            }

            /* Read then skip first 3 bytes that must always be present. */
            location = (subrec & OMF_FIXUP_LOCATION_MASK)                     //
                    >> OMF_FIXUP_LOCATION_SHIFT;                              // Extracts the LOC field
            offset = bfd_get_8(abfd, p + 1) + 256 * (subrec & 3);             // and offset bits 9 through 0 of the fixup field
            fixdata = bfd_get_8(abfd, p + 2);
            p += 3;                                                           // Advances the pointer p past the processed bytes
            reclen -= 3;                                                      // and decrements reclen accordingly.
            fprintf(stderr, " FIXUP subrec at [%p]: %02x, M: %02x, location: %02x, offset: %02llx, fixdata: %02llx\n",
                    p, subrec, (subrec & OMF_FIXUP_SEGREL) >> 6, location, (unsigned long long)offset, (unsigned long long)fixdata);

            if (fixdata & OMF_FIX_DATA_FRAME_THREAD) {
                /* FRAME for this fixup is specified by a reference to a previous thread field. */
                struct i386_fixup_thread *frame_thread;

                _bfd_error_handler(_("  F_bit: %x, frame_method: %0x, T_bit: %x, P_bit: %x, targt: %x"),
                        // (*_bfd_error_handler)("  F_bit: %x, frame_method: %0x, T_bit: %x, P_bit: %x, targt: %x\n",
                                   (fixdata & 0x80)
                                           >> 0x7,              /* if F_bit=1. There is no frame datum field in the subrecord.*/
                                   (fixdata & OMF_FIX_DATA_FRAME_MASK) >> OMF_FIX_DATA_FRAME_SHIFT,
                                   (fixdata & 0x8) >> 0x3,
                                   (fixdata & 0x4) >> 0x2,
                                   fixdata & 0x3);
                /* frame = 0;*/

                /* the frame field contains a number between 0 and 3 that indicates the thread field containing the FRAME method. */
                frame_thread = strtab_lookup(tdata->fixup_threads,
                                             (fixdata & OMF_FIX_DATA_FRAME_MASK) >> OMF_FIX_DATA_FRAME_SHIFT);
                _bfd_error_handler(_("  fixup FRAME thread_number: %x, method: %d, is_frame: %d, index: %x"),
                        //(*_bfd_error_handler)("  fixup FRAME thread_number: %x, method: %d, is_frame: %d, index: %x\n",
                                   frame_thread->thread_number,
                                   frame_thread->method,
                                   frame_thread->is_frame,
                                   frame_thread->index);
                frame_method = frame_thread->method;
                frame = frame_thread->index;
            } else {
                /* FRAME method is explicitly defined in this fixup field. */
                /* frame field contains 0,1, 2, 4, or 5, corresponding to one of the methods of specifying a FRAME listed in Table 19-2. */
                frame_method = (fixdata & OMF_FIX_DATA_FRAME_MASK) >> OMF_FIX_DATA_FRAME_SHIFT;
            }

            /*(*_bfd_error_handler)("  F_bit: %x, frame_method: %0x, T_bit: %x, P_bit: %x, targt: %x",
                                  (fixdata & 0x80) >> 0x7,
                                  frame_method,
                                  (fixdata&0x8) >> 0x3,
                                  (fixdata&0x4) >> 0x2,
                                  fixdata&0x3);*/
            switch (frame_method) {
                struct i386omf_segment *segdef;
                struct i386omf_group *grp;

                case OMF_FIXUPP_FRAME_SEGDEF:       // The FRAME is specified by a segment index.
                    if (!(fixdata & OMF_FIX_DATA_FRAME_THREAD)
                        && !i386omf_read_index(abfd, &frame, &p, &reclen))
                        return false;
                    segdef = strtab_lookup(tdata->segdef, frame);
                    frame_sym = (struct i386omf_symbol *) segdef->asect->symbol;
                    break;
                case OMF_FIXUPP_FRAME_GRPDEF:       // The FRAME is specified by a group index.
                    if (!(fixdata & OMF_FIX_DATA_FRAME_THREAD)
                        && !i386omf_read_index(abfd, &frame, &p, &reclen))
                        return false;
                    grp = strtab_lookup(tdata->grpdef, frame);
                    frame_sym = grp->symbol;
                    break;
                case OMF_FIXUPP_FRAME_EXTDEF:       // The FRAME is indicated by an external index. LINK determines the FRAME from the external name's corresponding PUBDEF record in another object module, which specifies either a logical segment or a group.
                    if (!(fixdata & OMF_FIX_DATA_FRAME_THREAD)
                        && !i386omf_read_index(abfd, &frame, &p, &reclen))
                        return false;
                    frame_sym = strtab_lookup(tdata->externs, frame);
                    break;
                case OMF_FIXUPP_FRAME_EXPLICIT:     // The FRAME is identified by an explicit frame number. (Not supported by LINK.)
                    frame = (int) bfd_get_16(abfd, p);
                    p += 2;
                    reclen -= 2;
                    frame_sym = NULL; /* TODO: Make an absolute symbol. */
                    break;
                case OMF_FIXUPP_FRAME_LEIDATA:  // The FRAME is determined by the segment in which the LOCATION is defined. In this case, the largest possible frame number is used.
                    frame_sym = (struct i386omf_symbol *) tdata->last_leidata->asect->symbol;
                    break;
                case OMF_FIXUPP_FRAME_TARGET:   // The FRAME is determined by the TARGET'S segment, group, or external index.
                    frame_sym = NULL;
                    break;
                default:
                    bfd_set_error(bfd_error_wrong_format);
                    return false;
            }

            if (fixdata & OMF_FIX_DATA_TARGET_THREAD) {     // if T bit set to one
                struct i386_fixup_thread *target_thread;
                // XXX Look it up from the TARGET thread.
                // target_method = 0;
                // target = 0;

                /*  If the T bit indicates that the TARGET is specified by a previous thread reference
                   (T = 1), the targt field contains a number between 0 and 3 that refers to a previous
                   thread field containing the TARGET method. In this case, the P bit, combined with the 2
                   low-order bits of the method field in the thread field, determines the TARGET thread. */
                target_thread = strtab_lookup(tdata->fixup_threads,
                                              fixdata & (OMF_FIX_DATA_P_MASK + OMF_FIX_DATA_TARGT_MASK));
                _bfd_error_handler(_("  fixup TARGET thread_number: %x, method: %d, is_frame: %d, index: %x"),
                        //(*_bfd_error_handler)("  fixup TARGET thread_number: %x, method: %d, is_frame: %d, index: %x\n",
                                   target_thread->thread_number,
                                   target_thread->method,
                                   target_thread->is_frame,
                                   target_thread->index);
                target_method = target_thread->method;
                target = target_thread->index;
            } else {
                target_method = fixdata & OMF_FIX_DATA_TARGET_METHOD_MASK;
            }
            target_relent = bfd_alloc(abfd, sizeof(*target_relent));
            if (target_relent == NULL)
                return false;

            q = p;
            switch (target_method) {
                struct i386omf_segment *segdef;
                struct i386omf_group *grpdef;

                case OMF_FIXUPP_TARGET_SEGDEF:
                case OMF_FIXUPP_TARGET_NODISP | OMF_FIXUPP_TARGET_SEGDEF:
                    if (!(fixdata & OMF_FIX_DATA_TARGET_THREAD) && !i386omf_read_index(abfd, &target, &p, &reclen))
                        return false;
                    segdef = strtab_lookup(tdata->segdef, target);
                    if (segdef == NULL) {
                        (*_bfd_error_handler)("FIXUP at 0x%x wants phantom segment [%d]",
                                              q - tdata->image, target);
                        bfd_set_error(bfd_error_wrong_format);
                        return false;
                    }
                    target_relent->symbol = segdef->asect->symbol;
                    break;
                case OMF_FIXUPP_TARGET_GRPDEF:
                case OMF_FIXUPP_TARGET_NODISP | OMF_FIXUPP_TARGET_GRPDEF:
                    if (!(fixdata & OMF_FIX_DATA_TARGET_THREAD) && !i386omf_read_index(abfd, &target, &p, &reclen))
                        return false;
                    grpdef = strtab_lookup(tdata->grpdef, target);
                    if (grpdef == NULL) {
                        (*_bfd_error_handler)("FIXUP at 0x%x wants phantom group [%d]",
                                              q - tdata->image, target);
                        bfd_set_error(bfd_error_wrong_format);
                        return false;
                    }
                    target_relent->symbol = NULL;
                    break;
                case OMF_FIXUPP_TARGET_EXTDEF:
                case OMF_FIXUPP_TARGET_NODISP | OMF_FIXUPP_TARGET_EXTDEF:
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
                case OMF_FIXUPP_TARGET_EXPLICIT:
                case OMF_FIXUPP_TARGET_NODISP | OMF_FIXUPP_TARGET_EXPLICIT:
                    target = (int) bfd_get_16(abfd, p);
                    p += 2;
                    reclen -= 2;
                    target_relent->symbol = NULL;
                    break;
            }
            // displacement is provided if P bit is set to 0
            if (!(fixdata & 0x80) >> 0x7) {
                if (!i386omf_read_offset(abfd, &displacement, &p, &reclen,
                                         I386OMF_OFFSET_SIZE_16)) {
                    (*_bfd_error_handler)("FIXUP at 0x%x wants displacement but none given [%d]\n",
                                          q - tdata->image, target);
                    bfd_set_error(bfd_error_wrong_format);
                    return false;
                }
            }

            // this logic was incorrect
            /*if (!(target_method & OMF_FIXUPP_TARGET_NODISP)) {
                if (!i386omf_read_offset(abfd, &displacement, &p, &reclen,
                                         I386OMF_OFFSET_SIZE_16)) {
                    fprintf(stderr, "FIXUP at 0x%zx wants displacement but none given [%d]\n",
                                          q - tdata->image, target);
                    return false;
                }
            }*/

            target_relent->base.sym_ptr_ptr = &target_relent->symbol;
            target_relent->base.address = offset;
            howto = &(subrec & OMF_FIXUP_SEGREL                     // M bit - 1 segment relative, 0 - self relative
                      ? howto_table_i386omf_segrel
                      : howto_table_i386omf_pcrel)[location];
            target_relent->base.addend
                    = displacement + (subrec & OMF_FIXUP_SEGREL
                                      ? 0
                                      : -bfd_get_reloc_size(howto));
            target_relent->base.howto = howto;
            strtab_add(tdata->last_leidata->relocs, target_relent);

            switch (frame_method) {
                case OMF_FIXUPP_FRAME_TARGET:       // the frame is determined by the target's segment, group or external index
                    // TODO fix this
                    break;
                default:
                    frame_relent = bfd_alloc(abfd, sizeof(*frame_relent));
                    if (frame_relent == NULL)
                        return false;
                    frame_relent->symbol = &frame_sym->base;
                    frame_relent->base.sym_ptr_ptr = &frame_relent->symbol;
                    frame_relent->base.address = offset;
                    frame_relent->base.addend = 0;
                    frame_relent->base.howto = &howto_wrt_segdef;
                    strtab_add(tdata->last_leidata->relocs, frame_relent);
                    break;
            }

            abfd->flags |= HAS_SYMS;
            tdata->last_leidata->asect->flags |= SEC_RELOC;

        } else {    // this is a thread field
            struct i386_fixup_thread *fixup_thread;

            int threaddata, index;

            threaddata = bfd_get_8(abfd, p++);
            reclen--;

            i386omf_read_index(abfd, &index, &p, &reclen);
            _bfd_error_handler(
                    _(" THREAD subrec: %02x, D(%x): %s, data bit5: %x, method: %d - %s, thread number: %d, index: %d"),
                    //(*_bfd_error_handler)(" THREAD subrec: %02x, D(%x): %s, data bit5: %x, method: %d - %s, thread number: %d, index: %d",
                    threaddata,
                    (threaddata & 0x40) >> 6,
                    ((threaddata & 0x40) >> 6) ? "FRAME" : "TARGET",         // D b6
                    (threaddata & 0x20),                                     // b5 (always 0)
                    ((threaddata & 0x1c) >> 2),                             // method b4,b3,b2
                    thread_method[((threaddata & 0x1c) >> 2)],               // method b4,b3,b2
                    threaddata & 3,                                          // thread number b1,b0
                    index                                            // index
            );

            fixup_thread = bfd_alloc(abfd, sizeof(*fixup_thread));
            fixup_thread->index = index;
            fixup_thread->thread_number = threaddata & OMF_FIXUP_THREAD_DATA_THREAD_NUMBER;
            fixup_thread->is_frame =
                    (threaddata & OMF_FIXUP_THREAD_DATA_D_FIELD_MASK) >> OMF_FIXUP_THREAD_DATA_D_FIELD_SHIFT;
            fixup_thread->method =
                    (threaddata & OMF_FIXUP_THREAD_DATA_METHOD_MASK) >> OMF_FIXUP_THREAD_DATA_METHOD_SHIFT;
            strtab_add(tdata->fixup_threads, fixup_thread);
        }
    }

    return true;
}

static bfd_size_type
i386omf_add_section_lidata(bfd *abfd, struct bfd_section *asect,
                           bfd_vma *offset, bfd_byte const *p,
                           bfd_size_type reclen) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;
    bfd_size_type block_count;
    bfd_size_type eaten = 0;

    if (reclen < 2 + 2) {
        (*_bfd_error_handler)("LIDATA data block truncated at 0x%lx.",
                              p - tdata->image);
        bfd_set_error(bfd_error_wrong_format);
        return false;
    }

    bfd_get_16(abfd, p);
    eaten += 2;
    block_count = bfd_get_16(abfd, p + eaten);
    eaten += 2;

    p += eaten;
    reclen -= eaten;

    if (block_count == 0) {
        /* Recursive exit condition. */
        int licount;

        licount = bfd_get_8(abfd, p);

        if ((asect->size < *offset)
            || (asect->size - *offset < (bfd_size_type) licount)) {
            (*_bfd_error_handler)("LIDATA at 0x%lx overflows section %A",
                                  asect, p - tdata->image);
            bfd_set_error(bfd_error_wrong_format);
            return 0;
        }

        memcpy(asect->contents + *offset, p + 1, licount);
        *offset += licount;
        eaten += licount + 1;
    } else {
        /* Recursive definition: data block contains other data blocks. */
        while (block_count--) {
            bfd_size_type subeaten;

            subeaten = i386omf_add_section_lidata(abfd, asect, offset,
                                                  p, reclen);
            if (!subeaten)
                return 0;

            if (subeaten > reclen) {
                (*_bfd_error_handler)("LIDATA at 0x%lx overflows section %A",
                                      asect, p - tdata->image);
                bfd_set_error(bfd_error_wrong_format);
                return 0;
            }

            p += subeaten;
            reclen -= subeaten;
        }
        if (reclen)
            (*_bfd_error_handler)("Leftover LIDATA at 0x%lx in section %A",
                                  asect, p - tdata->image);
    }

    return eaten;
}

static bool
i386omf_add_section_data(bfd *abfd,
                         struct bfd_section *asect,
                         bfd_vma offset,
                         bfd_byte const *p,
                         bfd_size_type reclen, int rectype) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;

    /* Lazily allocate memory for section data. */
    if ((asect->flags & SEC_IN_MEMORY) == 0) {
        asect->contents = bfd_zalloc(abfd, asect->size);
        if (asect->contents == NULL) {
            (*_bfd_error_handler)("Out of memory for %A section contents",
                                  asect);
            return false;
        }
        asect->flags |= SEC_IN_MEMORY;
    }

    if (rectype & (OMF_RECORD_LEDATA ^ OMF_RECORD_LIDATA)) {
        /* LIDATA, 0xa2 or 0xa3. */
        if (!i386omf_add_section_lidata(abfd, asect, &offset,
                                        p, reclen))
            return false;
    } else {
        /* LEDATA, 0xa2 or 0xa3. */
        if ((asect->size < offset) || (asect->size - offset < reclen)) {
            (*_bfd_error_handler)("LEDATA at 0x%lx overflows section %A",
                                  asect, p - tdata->image);
            bfd_set_error(bfd_error_wrong_format);
            return false;
        }

        memcpy(asect->contents + offset, p, reclen);
    }

    return true;
}

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

    segdef = strtab_lookup(tdata->segdef, seg_index);
    if (segdef == NULL) {
        (*_bfd_error_handler)("LEDATA at 0x%lx wants phantom segment [%d]",
                              p - tdata->image, seg_index);
        bfd_set_error(bfd_error_wrong_format);
        return false;
    }

    /* We'll need to know which section FIXUP records refer to. */
    tdata->last_leidata = segdef;

    if (!i386omf_read_offset(abfd, &offset, &p, &reclen,
                             rectype & 1 ? I386OMF_OFFSET_SIZE_32 : I386OMF_OFFSET_SIZE_16))
        return false;

    if (!i386omf_add_section_data(abfd, segdef->asect, offset,
                                  p, reclen, rectype))
        return false;

    segdef->asect->flags |= (SEC_HAS_CONTENTS |
                             SEC_LOAD |
                             SEC_ALLOC);

    return true;
}

static bool
process_record(bfd *abfd,
               int rectype,
               bfd_size_type reclen,
               bfd_byte const *p) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;
    bool record_ok;
    fprintf(stderr, "i386omf process_record rectype: 0x%2x, reclen: %llu\n", rectype, (unsigned long long)reclen);
    //_bfd_error_handler(_("i386omf process_record rectype: 0x%2x, reclen: %lu"), rectype, reclen);
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
            record_ok = i386omf_read_modend(abfd, p, reclen);
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
        case OMF_RECORD_FIXUPP:
        case OMF_RECORD_FIXUPP386:
            record_ok = i386omf_read_fixupp(abfd, p, reclen);
            break;
        case OMF_RECORD_LEDATA:
        case OMF_RECORD_LIDATA:
        case OMF_RECORD_LEDATA386:
        case OMF_RECORD_LIDATA386:
            record_ok = i386omf_read_leidata(abfd, p, reclen, rectype);
            break;
        case OMF_RECORD_COMDAT:
        case OMF_RECORD_COMDAT386:
            record_ok = true; /* TODO, need these for watcom. */
            break;
        case OMF_RECORD_LINSYM:
        case OMF_RECORD_LINSYM386:
            record_ok = true; /* TODO, need these for watcom. */
            break;
        case OMF_RECORD_COMDEF:
        case OMF_RECORD_LCOMDEF:
            record_ok = i386omf_read_comdef(abfd, p, reclen);
            break;
        default:
            (*_bfd_error_handler)("Unknown RECORD record 0x%02x at 0x%X",
                                  rectype, p - tdata->image);

            bfd_set_error(bfd_error_wrong_format);
            record_ok = false;
    }

    return record_ok;
}

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
            &tdata->fixup_threads,
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

    return true;
}

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
            &tdata->fixup_threads,
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
  strtab_add (tdata->fixup_threads, NULL);

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

static bool
i386omf_close_and_cleanup(bfd *abfd) {
    i386omf_teardown_tdata(abfd);
    return true;
}

#define i386omf_bfd_free_cached_info  _bfd_generic_bfd_free_cached_info
#define i386omf_new_section_hook      _bfd_generic_new_section_hook

static bool
i386omf_get_section_contents(bfd *abfd ATTRIBUTE_UNUSED,
                             asection *section,
                             void *location,
                             file_ptr offset,
                             bfd_size_type count) {
    if (section->flags & SEC_IN_MEMORY)
        memcpy(location, section->contents + offset, count);
    else
        memset(location, 0, count);

    return true;
}

/* Return the amount of memory needed to read the symbol table.  */

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
#define i386omf_bfd_set_private_flags \
    _bfd_generic_bfd_set_private_flags

static bool
i386omf_bfd_print_private_bfd_data(bfd *abfd, void *farg) {
    struct i386omf_obj_data *tdata = abfd->tdata.any;
    struct i386omf_segment *seg;
    struct i386omf_group *grp;
    FILE *f = farg;
    struct counted_string const *s;
    int i;

    fprintf(f, "\nModule name: %s\n", tdata->module_name.data);
    if (tdata->is_main_module) {
        fprintf(f, "Main module\n");
    }
    if (tdata->has_start_address) {
        fprintf(f, "Has start address\n");
    }
    if (tdata->translator)
        fprintf(f, "Translator: %s\n", tdata->translator);
    if (strtab_size(tdata->dependencies)) {
        struct i386omf_borland_dependency *dep;

        fprintf(f, "Dependencies:\n");
        for (i = 0; (dep = strtab_lookup(tdata->dependencies, i)); i++) {
            fprintf(f, "  %04d-%02d-%02d %02d:%02d:%02d %s\n",
                    ((dep->date >> OMF_MSDOS_DATE_YEAR_SHIFT)
                     & W2M(OMF_MSDOS_DATE_YEAR_WIDTH)) + 1980,
                    (dep->date >> OMF_MSDOS_DATE_MONTH_SHIFT)
                    & W2M(OMF_MSDOS_DATE_MONTH_WIDTH),
                    dep->date & W2M(OMF_MSDOS_DATE_DAY_WIDTH),
                    (dep->time >> OMF_MSDOS_TIME_HOUR_SHIFT)
                    & W2M(OMF_MSDOS_TIME_HOUR_WIDTH),
                    (dep->time >> OMF_MSDOS_TIME_MINUTE_SHIFT)
                    & W2M(OMF_MSDOS_TIME_MINUTE_WIDTH),
                    (dep->time & W2M(OMF_MSDOS_TIME_2SECOND_WIDTH)) * 2,
                    dep->filename.data);
        }
    }
    fprintf(f, "LNAMES:\n");
    for (i = OMF_LNAMES_NONE + 1; (s = strtab_lookup(tdata->lnames, i)); i++) {
        fprintf(f, "  %d %s\n", i, s->data);
    }
    fprintf(f, "SEGDEF:\n");
    for (i = OMF_SEGDEF_NONE + 1; (seg = strtab_lookup(tdata->segdef, i)); i++) {
        fprintf(f, "  %s (%d)\n",
                i386omf_lookup_string(tdata->lnames, seg->name_index,
                                      "UNNAMED"),
                seg->name_index);
    }
    fprintf(f, "GRPDEF:\n");
    for (i = OMF_GRPDEF_NONE + 1; (grp = strtab_lookup(tdata->grpdef, i)); i++) {
        struct i386omf_group_entry *entry;
        int j;

        fprintf(f, "  %s (%d)",
                i386omf_lookup_string(tdata->lnames, grp->name_index,
                                      "UNNAMED"),
                grp->name_index);

        for (j = 0; (entry = strtab_lookup(grp->entries, j)); j++) {
            switch (entry->type) {
                case GRPDEF_ENTRY_SEGDEF:
                    fprintf(f, " (%d)", entry->u.segdef);
                    break;
                default:
                    fprintf(f, " ??? (type=0x%02x)", entry->type);
                    break;
            }
        }

        fprintf(f, "\n");
    }

    return false;
}

static asymbol *
i386omf_make_empty_symbol(bfd *abfd) {
    bfd_size_type amt = sizeof(struct i386omf_symbol);
    asymbol *new = bfd_zalloc(abfd, amt);
    if (new)
        new->the_bfd = abfd;
    return new;
}

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

static long
i386omf_get_reloc_upper_bound(bfd *abfd ATTRIBUTE_UNUSED, asection *sec) {
    struct i386omf_segment *seg = sec->used_by_bfd;
    long n = strtab_size(seg->relocs);

    return n * sizeof(arelent * );
}

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
    _bfd_error_handler("i386omf_write_object_contents NOT IMPLEMENTED %s", abfd->filename);
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
                (*_bfd_error_handler)
                        (_("Warning: Writing section `%s' to huge (ie negative) file offset 0x%lx."),
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
binary_sizeof_headers(bfd *abfd ATTRIBUTE_UNUSED,
                      struct bfd_link_info *info ATTRIBUTE_UNUSED) {
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
#define i386omf_set_reloc                         _bfd_norelocs_set_reloc
#define binary_bfd_define_start_stop               bfd_generic_define_start_stop
#define i386omf_find_line                         _bfd_nosymbols_find_line
#define i386omf_find_nearest_line_with_alt        _bfd_nosymbols_find_nearest_line_with_alt
#define i386omf_get_symbol_version_string \
  _bfd_nosymbols_get_symbol_version_string
#define binary_bfd_link_check_relocs              _bfd_generic_link_check_relocs

const bfd_target i386_omf_vec =
{
  "i386omf",			/* name */
  bfd_target_omf_flavour,	/* flavour */
  BFD_ENDIAN_LITTLE,		/* byteorder */
  BFD_ENDIAN_LITTLE,		/* header_byteorder */
  (HAS_RELOC | HAS_SYMS | HAS_LOCALS), /* object_flags */
  (SEC_ALLOC | SEC_LOAD | SEC_LOAD | SEC_RELOC | SEC_READONLY
   | SEC_CODE | SEC_DATA | SEC_ROM | SEC_HAS_CONTENTS
   | SEC_IN_MEMORY | SEC_GROUP), /* section_flags */
  0,				/* symbol_leading_char */
  ' ',				/* ar_pad_char */
  16,				/* ar_max_namelen */
  255,				/* match priority.  */
  TARGET_KEEP_UNUSED_SECTION_SYMBOLS, /* keep unused section symbols.  */
  bfd_getl64, bfd_getl_signed_64, bfd_putl64,
  bfd_getl32, bfd_getl_signed_32, bfd_putl32,
  bfd_getl16, bfd_getl_signed_16, bfd_putl16,	/* data */
  bfd_getl64, bfd_getl_signed_64, bfd_putl64,
  bfd_getl32, bfd_getl_signed_32, bfd_putl32,
  bfd_getl16, bfd_getl_signed_16, bfd_putl16,	/* hdrs */
  {				/* bfd_check_format */
        _bfd_dummy_target,
        i386omf_object_p,
        _bfd_dummy_target,
        _bfd_dummy_target,
  },
  {				/* bfd_set_format */
          _bfd_bool_bfd_false_error,
          binary_mkobject,
          _bfd_bool_bfd_false_error,
          _bfd_bool_bfd_false_error,
  },
  {				/* bfd_write_contents */
          _bfd_bool_bfd_false_error,
          i386omf_write_object_contents,
          _bfd_bool_bfd_false_error,
          _bfd_bool_bfd_false_error,
  },

  BFD_JUMP_TABLE_GENERIC (i386omf),
  BFD_JUMP_TABLE_COPY (_bfd_generic),
  BFD_JUMP_TABLE_CORE (_bfd_nocore),
  BFD_JUMP_TABLE_ARCHIVE (_bfd_noarchive),
  BFD_JUMP_TABLE_SYMBOLS (i386omf),
  BFD_JUMP_TABLE_RELOCS (i386omf),
  BFD_JUMP_TABLE_WRITE (binary),
  BFD_JUMP_TABLE_LINK (binary),
  BFD_JUMP_TABLE_DYNAMIC (_bfd_nodynamic),

                NULL,

                NULL
        };
