/* Copyright (C) 2024-2025 Free Software Foundation, Inc.

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

#ifndef GDBSUPPORT_UNORDERED_MAP_H
#define GDBSUPPORT_UNORDERED_MAP_H

#include "unordered_dense.h"

namespace gdb
{

template<typename Key,
	 typename T,
	 typename Hash = ankerl::unordered_dense::hash<Key>,
	 typename KeyEqual = std::equal_to<Key>>
using unordered_map
  = ankerl::unordered_dense::map
      <Key, T, Hash, KeyEqual, std::allocator<std::pair<Key, T>>,
       ankerl::unordered_dense::bucket_type::standard>;

} /* namespace gdb */

#endif /* GDBSUPPORT_UNORDERED_MAP_H */
