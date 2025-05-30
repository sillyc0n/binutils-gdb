/* Simulator support for m32r.

THIS FILE IS MACHINE GENERATED WITH CGEN.

Copyright (C) 1996-2025 Free Software Foundation, Inc.

This file is part of the GNU simulators.

   This file is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   It is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street - Fifth Floor, Boston, MA 02110-1301, USA.

*/

#include "sim-main.h"
#include "bfd.h"

const SIM_MACH * const m32r_sim_machs[] =
{
#ifdef HAVE_CPU_M32RBF
  & m32r_mach,
#endif
#ifdef HAVE_CPU_M32RXF
  & m32rx_mach,
#endif
#ifdef HAVE_CPU_M32R2F
  & m32r2_mach,
#endif
  0
};

