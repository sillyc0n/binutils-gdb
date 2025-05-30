/* GNU/Linux/x86 specific low level interface, for the in-process
   agent library for GDB.

   Copyright (C) 2010-2025 Free Software Foundation, Inc.

   This file is part of GDB.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#include <sys/mman.h>
#include "tracepoint.h"
#include "gdbsupport/x86-xstate.h"
#include "arch/i386-linux-tdesc.h"
#include "arch/x86-linux-tdesc-features.h"

/* GDB register numbers.  */

enum i386_gdb_regnum
{
  I386_EAX_REGNUM,		/* %eax */
  I386_ECX_REGNUM,		/* %ecx */
  I386_EDX_REGNUM,		/* %edx */
  I386_EBX_REGNUM,		/* %ebx */
  I386_ESP_REGNUM,		/* %esp */
  I386_EBP_REGNUM,		/* %ebp */
  I386_ESI_REGNUM,		/* %esi */
  I386_EDI_REGNUM,		/* %edi */
  I386_EIP_REGNUM,		/* %eip */
  I386_EFLAGS_REGNUM,		/* %eflags */
  I386_CS_REGNUM,		/* %cs */
  I386_SS_REGNUM,		/* %ss */
  I386_DS_REGNUM,		/* %ds */
  I386_ES_REGNUM,		/* %es */
  I386_FS_REGNUM,		/* %fs */
  I386_GS_REGNUM,		/* %gs */
  I386_ST0_REGNUM		/* %st(0) */
};

#define i386_num_regs 16

#define FT_CR_EAX 15
#define FT_CR_ECX 14
#define FT_CR_EDX 13
#define FT_CR_EBX 12
#define FT_CR_UESP 11
#define FT_CR_EBP 10
#define FT_CR_ESI 9
#define FT_CR_EDI 8
#define FT_CR_EIP 7
#define FT_CR_EFL 6
#define FT_CR_DS 5
#define FT_CR_ES 4
#define FT_CR_FS 3
#define FT_CR_GS 2
#define FT_CR_SS 1
#define FT_CR_CS 0

/* Mapping between the general-purpose registers in jump tracepoint
   format and GDB's register array layout.  */

static const int i386_ft_collect_regmap[] =
{
  FT_CR_EAX * 4, FT_CR_ECX * 4, FT_CR_EDX * 4, FT_CR_EBX * 4,
  FT_CR_UESP * 4, FT_CR_EBP * 4, FT_CR_ESI * 4, FT_CR_EDI * 4,
  FT_CR_EIP * 4, FT_CR_EFL * 4, FT_CR_CS * 4, FT_CR_SS * 4,
  FT_CR_DS * 4, FT_CR_ES * 4, FT_CR_FS * 4, FT_CR_GS * 4
};

void
supply_fast_tracepoint_registers (struct regcache *regcache,
				  const unsigned char *buf)
{
  int i;

  for (i = 0; i < i386_num_regs; i++)
    {
      int regval;

      if (i >= I386_CS_REGNUM && i <= I386_GS_REGNUM)
	regval = *(short *) (((char *) buf) + i386_ft_collect_regmap[i]);
      else
	regval = *(int *) (((char *) buf) + i386_ft_collect_regmap[i]);

      supply_register (regcache, i, &regval);
    }
}

ULONGEST
get_raw_reg (const unsigned char *raw_regs, int regnum)
{
  /* This should maybe be allowed to return an error code, or perhaps
     better, have the emit_reg detect this, and emit a constant zero,
     or something.  */

  if (regnum > i386_num_regs)
    return 0;
  else if (regnum >= I386_CS_REGNUM && regnum <= I386_GS_REGNUM)
    return *(short *) (raw_regs + i386_ft_collect_regmap[regnum]);
  else
    return *(int *) (raw_regs + i386_ft_collect_regmap[regnum]);
}

/* This is only needed because reg-i386-linux-lib.o references it.  We
   may use it proper at some point.  */
const char *gdbserver_xmltarget;

/* Attempt to allocate memory for trampolines in the first 64 KiB of
   memory to enable smaller jump patches.  */

static void
initialize_fast_tracepoint_trampoline_buffer (void)
{
  const CORE_ADDR buffer_end = 64 * 1024;
  /* Ensure that the buffer will be at least 1 KiB in size, which is
     enough space for over 200 fast tracepoints.  */
  const int min_buffer_size = 1024;
  char buf[IPA_BUFSIZ];
  CORE_ADDR mmap_min_addr = buffer_end + 1;
  ULONGEST buffer_size;
  FILE *f = fopen ("/proc/sys/vm/mmap_min_addr", "r");

  if (!f)
    {    
      snprintf (buf, sizeof (buf), "mmap_min_addr open failed: %s",
		safe_strerror (errno));
      set_trampoline_buffer_space (0, 0, buf);
      return;
    }

  if (fgets (buf, IPA_BUFSIZ, f))
    sscanf (buf, "%llu", &mmap_min_addr);
      
  fclose (f);
      
  buffer_size = buffer_end - mmap_min_addr;

  if (buffer_size >= min_buffer_size)
    {
      if (mmap ((void *) (uintptr_t) mmap_min_addr, buffer_size,
		PROT_READ | PROT_EXEC | PROT_WRITE,
		MAP_FIXED | MAP_PRIVATE | MAP_ANONYMOUS,
		-1, 0)
	  != MAP_FAILED)
	set_trampoline_buffer_space (mmap_min_addr, buffer_end, NULL);
      else
	{
	  snprintf (buf, IPA_BUFSIZ, "low-64K-buffer mmap() failed: %s",
		    safe_strerror (errno));
	  set_trampoline_buffer_space (0, 0, buf);
	}
    }
  else
    {
      snprintf (buf, IPA_BUFSIZ, "mmap_min_addr is %d, must be %d or less",
		(int) mmap_min_addr, (int) buffer_end - min_buffer_size);
      set_trampoline_buffer_space (0, 0, buf);
    }
}

/* Return target_desc to use for IPA, given the tdesc index passed by
   gdbserver.  */

const struct target_desc *
get_ipa_tdesc (int idx)
{
  uint64_t xcr0 = x86_linux_tdesc_idx_to_xcr0 (idx);

  return i386_linux_read_description (xcr0);
}

/* Allocate buffer for the jump pads.  On i386, we can reach an arbitrary
   address with a jump instruction, so just allocate normally.  */

void *
alloc_jump_pad_buffer (size_t size)
{
  void *res = mmap (NULL, size, PROT_READ | PROT_WRITE | PROT_EXEC,
		    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

  if (res == MAP_FAILED)
    return NULL;

  return res;
}

void
initialize_low_tracepoint (void)
{
  initialize_fast_tracepoint_trampoline_buffer ();
  for (int i = 0; i < x86_linux_i386_tdesc_count (); i++)
    i386_linux_read_description (x86_linux_tdesc_idx_to_xcr0 (i));
}
