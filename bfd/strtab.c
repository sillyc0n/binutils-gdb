#include "sysdep.h"
#include "bfd.h"
#include "strtab.h"
#include "libbfd.h"
#include "strtab.h"

#define STRTAB_INIT_SIZE 256

struct strtab {
  bfd *owner;         // owning bfd structure.
  void **strings;     // A dynamically allocated array of pointers to strings.
  int strings_used;   // The number of strings currently stored in the table.
  int strings_size;   // The total capacity of the strings array.
};

/**
 * @brief Initialization - Allocates and initializes a new strtab instance. 
 * 
 * It allocates memory for the strtab structure itself and for the initial size of the strings array. 
 * If either allocation fails, it cleans up and returns
 */
struct strtab * strtab_new(bfd *abfd)
{
  struct strtab *st;
  size_t amt;

  st = bfd_malloc (sizeof (*st));
  if (st == NULL)
    return NULL;
  st->owner = abfd;

  if (_bfd_mul_overflow (STRTAB_INIT_SIZE, sizeof (*st->strings), &amt))
  {
    bfd_set_error (bfd_error_file_too_big);
    return NULL;
  }

  st->strings = bfd_malloc (amt);
  if (st->strings == NULL)
    {
      free (st);
      return NULL;
    }
  st->strings_used = 0;
  st->strings_size = STRTAB_INIT_SIZE;

  return st;
}

/**
 * @brief Cleanup - Frees the memory allocated for the strings array and the strtab structure itself.
 * 
 * @param ATTRIBUTE_UNUSED 
 */
void strtab_free (struct strtab *st ATTRIBUTE_UNUSED)
{
  free (st->strings);
  free (st);
}

/**
 * @brief Size Query - Returns the number of strings currently stored in the table.
 * 
 * @param st 
 * @return int 
 */
int strtab_size(struct strtab const *st)
{
  return (st->strings_used);
}

/**
 * @brief String Addition - Adds a new string to the table. 
 * If the current capacity is exceeded, it reallocates the strings array to double its size. 
 * Then, it adds the new string and increments the usage count.
 * 
 * @param st 
 * @param s 
 * @return int 
 */
int strtab_add(struct strtab *st, void *s)
{
  if (st->strings_used >= st->strings_size)
    {
      void **newstrings;
      int newsize = st->strings_size * 2;
      size_t amt;

     /*if (vec->max_el > -1u / 2)
     {
       bfd_set_error (bfd_error_file_too_big);
       return NULL;
     }*/

      if (_bfd_mul_overflow (newsize, sizeof (*st->strings), &amt))
      {
        bfd_set_error (bfd_error_file_too_big);
        abort();
      }
        // TODO - is this correct?
      //newstrings = bfd_realloc2 (st->strings, newsize, sizeof (*st->strings));
      newstrings = bfd_realloc_or_free (st->strings, amt);
      if (newstrings == NULL)
	  {
	    (*_bfd_error_handler) ("strings_used %d >= strings_size %d",
				 st->strings_used, st->strings_size);
	    abort();
	  }

      st->strings = newstrings;
      st->strings_size = newsize;
    }

  st->strings[st->strings_used++] = s;

  return (st->strings_used - 1);
}

/**
 * @brief Lookup - Retrieves the string at index i. If i is out of bounds, it returns NULL.
 * 
 * @param st 
 * @param i 
 * @return void* 
 */
void * strtab_lookup(struct strtab *st, int i)
{
  if (i >= st->strings_used)
    return NULL;

  return (st->strings[i]);
}
