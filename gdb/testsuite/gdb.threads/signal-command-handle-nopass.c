/* This testcase is part of GDB, the GNU debugger.

   Copyright 2014-2025 Free Software Foundation, Inc.

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

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <pthread.h>
#include <signal.h>

static pthread_barrier_t barrier;

void
handler (int sig)
{
}

void *
thread_function (void *arg)
{
  volatile unsigned int i = 1;

  while (i != 0)
    usleep (1);
}

void *
thread_function_sync (void *arg)
{
  pthread_barrier_wait (&barrier);
  return thread_function (arg);
}

int
main (void)
{
  pthread_t child_thread;
  int i;

  signal (SIGUSR1, handler);

  pthread_barrier_init (&barrier, NULL, 2);

  pthread_create (&child_thread, NULL, thread_function_sync, NULL);

  /* Make sure that pthread_create is done once the breakpoint on
     thread_function triggers.  */
  pthread_barrier_wait (&barrier);

  pthread_join (child_thread, NULL);

  return 0;
}
