/* Pascal language support routines for GDB, the GNU debugger.

   Copyright (C) 2000-2025 Free Software Foundation, Inc.

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

/* This file is derived from c-lang.c */

#include "event-top.h"
#include "extract-store-integer.h"
#include "symtab.h"
#include "gdbtypes.h"
#include "expression.h"
#include "parser-defs.h"
#include "language.h"
#include "varobj.h"
#include "p-lang.h"
#include "valprint.h"
#include "value.h"
#include <ctype.h>
#include "c-lang.h"
#include "gdbarch.h"
#include "cli/cli-style.h"

/* All GPC versions until now (2007-09-27) also define a symbol called
   '_p_initialize'.  Check for the presence of this symbol first.  */
static const char GPC_P_INITIALIZE[] = "_p_initialize";

/* The name of the symbol that GPC uses as the name of the main
   procedure (since version 20050212).  */
static const char GPC_MAIN_PROGRAM_NAME_1[] = "_p__M0_main_program";

/* Older versions of GPC (versions older than 20050212) were using
   a different name for the main procedure.  */
static const char GPC_MAIN_PROGRAM_NAME_2[] = "pascal_main_program";

/* Function returning the special symbol name used
   by GPC for the main procedure in the main program
   if it is found in minimal symbol list.
   This function tries to find minimal symbols generated by GPC
   so that it finds the even if the program was compiled
   without debugging information.
   According to information supplied by Waldeck Hebisch,
   this should work for all versions posterior to June 2000.  */

const char *
pascal_main_name (void)
{
  bound_minimal_symbol msym
    = lookup_minimal_symbol (current_program_space, GPC_P_INITIALIZE);

  /*  If '_p_initialize' was not found, the main program is likely not
     written in Pascal.  */
  if (msym.minsym == NULL)
    return NULL;

  msym
    = lookup_minimal_symbol (current_program_space, GPC_MAIN_PROGRAM_NAME_1);
  if (msym.minsym != NULL)
    {
      return GPC_MAIN_PROGRAM_NAME_1;
    }

  msym
    = lookup_minimal_symbol (current_program_space, GPC_MAIN_PROGRAM_NAME_2);
  if (msym.minsym != NULL)
    {
      return GPC_MAIN_PROGRAM_NAME_2;
    }

  /*  No known entry procedure found, the main program is probably
      not compiled with GPC.  */
  return NULL;
}

/* See p-lang.h.  */

int
pascal_is_string_type (struct type *type,int *length_pos, int *length_size,
		       int *string_pos, struct type **char_type,
		       const char **arrayname)
{
  if (type != NULL && type->code () == TYPE_CODE_STRUCT)
    {
      /* Old Borland type pascal strings from Free Pascal Compiler.  */
      /* Two fields: length and st.  */
      if (type->num_fields () == 2
	  && type->field (0).name ()
	  && strcmp (type->field (0).name (), "length") == 0
	  && type->field (1).name ()
	  && strcmp (type->field (1).name (), "st") == 0)
	{
	  if (length_pos)
	    *length_pos = type->field (0).loc_bitpos () / TARGET_CHAR_BIT;
	  if (length_size)
	    *length_size = type->field (0).type ()->length ();
	  if (string_pos)
	    *string_pos = type->field (1).loc_bitpos () / TARGET_CHAR_BIT;
	  if (char_type)
	    *char_type = type->field (1).type ()->target_type ();
	  if (arrayname)
	    *arrayname = type->field (1).name ();
	 return 2;
	};
      /* GNU pascal strings.  */
      /* Three fields: Capacity, length and schema$ or _p_schema.  */
      if (type->num_fields () == 3
	  && type->field (0).name ()
	  && strcmp (type->field (0).name (), "Capacity") == 0
	  && type->field (1).name ()
	  && strcmp (type->field (1).name (), "length") == 0)
	{
	  if (length_pos)
	    *length_pos = type->field (1).loc_bitpos () / TARGET_CHAR_BIT;
	  if (length_size)
	    *length_size = type->field (1).type ()->length ();
	  if (string_pos)
	    *string_pos = type->field (2).loc_bitpos () / TARGET_CHAR_BIT;
	  /* FIXME: how can I detect wide chars in GPC ??  */
	  if (char_type)
	    {
	      *char_type = type->field (2).type ()->target_type ();

	      if ((*char_type)->code () == TYPE_CODE_ARRAY)
		*char_type = (*char_type)->target_type ();
	    }
	  if (arrayname)
	    *arrayname = type->field (2).name ();
	 return 3;
	};
    }
  return 0;
}

/* See p-lang.h.  */

void
pascal_language::print_one_char (int c, struct ui_file *stream,
				 int *in_quotes) const
{
  if (c == '\'' || ((unsigned int) c <= 0xff && (PRINT_LITERAL_FORM (c))))
    {
      if (!(*in_quotes))
	gdb_puts ("'", stream);
      *in_quotes = 1;
      if (c == '\'')
	{
	  gdb_puts ("''", stream);
	}
      else
	gdb_printf (stream, "%c", c);
    }
  else
    {
      if (*in_quotes)
	gdb_puts ("'", stream);
      *in_quotes = 0;
      gdb_printf (stream, "#%d", (unsigned int) c);
    }
}

/* See language.h.  */

void
pascal_language::printchar (int c, struct type *type,
			    struct ui_file *stream) const
{
  int in_quotes = 0;

  print_one_char (c, stream, &in_quotes);
  if (in_quotes)
    gdb_puts ("'", stream);
}



/* See language.h.  */

void pascal_language::language_arch_info
	(struct gdbarch *gdbarch, struct language_arch_info *lai) const
{
  const struct builtin_type *builtin = builtin_type (gdbarch);

  /* Helper function to allow shorter lines below.  */
  auto add  = [&] (struct type * t)
  {
    lai->add_primitive_type (t);
  };

  add (builtin->builtin_int);
  add (builtin->builtin_long);
  add (builtin->builtin_short);
  add (builtin->builtin_char);
  add (builtin->builtin_float);
  add (builtin->builtin_double);
  add (builtin->builtin_void);
  add (builtin->builtin_long_long);
  add (builtin->builtin_signed_char);
  add (builtin->builtin_unsigned_char);
  add (builtin->builtin_unsigned_short);
  add (builtin->builtin_unsigned_int);
  add (builtin->builtin_unsigned_long);
  add (builtin->builtin_unsigned_long_long);
  add (builtin->builtin_long_double);
  add (builtin->builtin_complex);
  add (builtin->builtin_double_complex);

  lai->set_string_char_type (builtin->builtin_char);
  lai->set_bool_type (builtin->builtin_bool, "boolean");
}

/* See language.h.  */

void
pascal_language::printstr (struct ui_file *stream, struct type *elttype,
			   const gdb_byte *string, unsigned int length,
			   const char *encoding, int force_ellipses,
			   const struct value_print_options *options) const
{
  enum bfd_endian byte_order = type_byte_order (elttype);
  unsigned int i;
  unsigned int things_printed = 0;
  int in_quotes = 0;
  int need_comma = 0;
  int width;

  /* Preserve ELTTYPE's original type, just set its LENGTH.  */
  check_typedef (elttype);
  width = elttype->length ();

  /* If the string was not truncated due to `set print elements', and
     the last byte of it is a null, we don't print that, in traditional C
     style.  */
  if ((!force_ellipses) && length > 0
      && extract_unsigned_integer (string + (length - 1) * width, width,
				   byte_order) == 0)
    length--;

  if (length == 0)
    {
      gdb_puts ("''", stream);
      return;
    }

  unsigned int print_max_chars = get_print_max_chars (options);
  for (i = 0; i < length && things_printed < print_max_chars; ++i)
    {
      /* Position of the character we are examining
	 to see whether it is repeated.  */
      unsigned int rep1;
      /* Number of repetitions we have detected so far.  */
      unsigned int reps;
      unsigned long int current_char;

      QUIT;

      if (need_comma)
	{
	  gdb_puts (", ", stream);
	  need_comma = 0;
	}

      current_char = extract_unsigned_integer (string + i * width, width,
					       byte_order);

      rep1 = i + 1;
      reps = 1;
      while (rep1 < length
	     && extract_unsigned_integer (string + rep1 * width, width,
					  byte_order) == current_char)
	{
	  ++rep1;
	  ++reps;
	}

      if (reps > options->repeat_count_threshold)
	{
	  if (in_quotes)
	    {
	      gdb_puts ("', ", stream);
	      in_quotes = 0;
	    }
	  printchar (current_char, elttype, stream);
	  gdb_printf (stream, " %p[<repeats %u times>%p]",
		      metadata_style.style ().ptr (),
		      reps, nullptr);
	  i = rep1 - 1;
	  things_printed += options->repeat_count_threshold;
	  need_comma = 1;
	}
      else
	{
	  if ((!in_quotes) && (PRINT_LITERAL_FORM (current_char)))
	    {
	      gdb_puts ("'", stream);
	      in_quotes = 1;
	    }
	  print_one_char (current_char, stream, &in_quotes);
	  ++things_printed;
	}
    }

  /* Terminate the quotes if necessary.  */
  if (in_quotes)
    gdb_puts ("'", stream);

  if (force_ellipses || i < length)
    gdb_puts ("...", stream);
}

/* Single instance of the Pascal language class.  */

static pascal_language pascal_language_defn;
