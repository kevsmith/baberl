/*

Copyright (c) 2009 Electronic Arts, Inc.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

 */
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <iconv.h>

#include <erl_driver.h>


#include "drv_util.h"

inline int read_int32(char **data) {
  char *d = *data;
  int value = ((((int)(((unsigned char*) (d))[0]))  << 24) |
	       (((int)(((unsigned char*) (d))[1]))  << 16) |
	       (((int)(((unsigned char*) (d))[2]))  << 8)  |
	       (((int)(((unsigned char*) (d))[3]))));
  (*data) += 4;
  return value;
}

// Any string read using this function must be freed
// using driver_free later
char *read_string(char **data) {
  int length = read_int32(data);
  char *buf = NULL;
  if (length > 0) {
    buf = (char *) driver_alloc(length + 1);
    memset(buf, 0, length + 1);
    memcpy(buf, (const char *) *data, length);
    (*data) += length;
  }
  else {
    buf = (char *) driver_alloc(1);
    memset(buf, 0, 1);
  }
  return buf;
}

void convert_text(const char *from, const char *to, char *text, const size_t text_size, converted_text_t *results) {
  size_t remaining = text_size;
  size_t available = remaining * 3;
  size_t start_available = available;
  size_t iconv_result = 0;

  char *buf = (char *) driver_alloc(available);
  char *result = buf;
  memset(buf, 0, available);
  iconv_t cd = iconv_open(to, from);
  while (remaining > 0) {
    iconv_result = iconv(cd, &text, &remaining, &buf, &available);
    if (iconv_result == -1) {
      results->error = 1;
      break;
    }
  }
  iconv_close(cd);
  if (results->error == 0) {
    results->text_size = start_available - available;
    results->text = result;
  }
}
