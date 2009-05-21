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

#include <string.h>
#include <strings.h>
#include <iconv.h>

#include <erl_driver.h>


#include "drv_util.h"

inline int read_int32(const char* data, int offset) {
  return ((((int)(((unsigned char*) (data))[offset]))  << 24) |
	  (((int)(((unsigned char*) (data))[offset + 1]))  << 16) |
	  (((int)(((unsigned char*) (data))[offset + 2]))  << 8)  |
	  (((int)(((unsigned char*) (data))[offset + 3]))));
}

// Any string read using this function must be freed
// using driver_free later
char* read_string(const char* data, int offset, int length) {
  char* buf = (char*) driver_alloc(length + 1);
  bzero(buf, length + 1);
  const char* source = &data[offset];
  memcpy(buf, source, length);
  return buf;
}

void convert_text(const char *from, const char *to, char *text, const size_t text_size, converted_text_t *results) {
  size_t remaining = text_size;
  size_t available = remaining * 3;
  size_t start_available = available;
  char *buf = (char *) driver_alloc(available);
  bzero(buf, available);
  char *result = buf;
  iconv_t cd = iconv_open(to, from);
  while (remaining > 0) {
    iconv(cd, &text, &remaining, &buf, &available);
  }
  iconv_close(cd);
  results->text_size = start_available - available;
  results->text = result;
}
