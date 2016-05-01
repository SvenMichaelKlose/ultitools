#include<stdlib.h>
#include<string.h>
#include"readline.h"

/** Read a line
 * @param file	the input stream
 * @return	a line of text (NULL on end of file or out of memory)
 */
char*
readline (FILE* file)
{
  size_t length = 0;
  char* buf = malloc (BUFSIZ);
  if (!buf)
    return buf;
  while (fgets (buf + length, BUFSIZ, file)) {
    char* last = buf + length;
    while (*last) last++;
    if (last > buf + length && last[-1] == '\n') {
      last[-1] = 0;
      return buf;
    }
    else {
      char* buf2 = realloc (buf, (length += last - buf) + BUFSIZ);
      if (!buf2)
        return buf;
      buf = buf2;
    }
  }

  if (!length) {
    free (buf);
    buf = NULL;
  }
  return buf;
}
