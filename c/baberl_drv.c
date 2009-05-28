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

#include <erl_driver.h>
#include <ei.h>

#include "config.h"
#include "drv_util.h"

typedef struct _baberl_drv_t {
  ErlDrvPort port;
} baberl_drv_t;


static ErlDrvData start(ErlDrvPort port, char* cmd);
static void stop(ErlDrvData handle);
static void process(ErlDrvData handle, ErlIOVec *ev);

static ErlDrvEntry baberl_driver_entry = {
    NULL,                             /* init */
    start,                            /* startup */
    stop,                             /* shutdown */
    NULL,                             /* output */
    NULL,                             /* ready_input */
    NULL,                             /* ready_output */
    "baberl_drv",                     /* the name of the driver */
    NULL,                             /* finish */
    NULL,                             /* handle */
    NULL,                             /* control */
    NULL,                             /* timeout */
    process,                          /* process */
    NULL,                             /* ready_async */
    NULL,                             /* flush */
    NULL,                             /* call */
    NULL,                             /* event */
    ERL_DRV_EXTENDED_MARKER,          /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING     /* ERL_DRV_FLAGs */
};

DRIVER_INIT(baberl_driver) {
  return &baberl_driver_entry;
}

static ErlDrvData start(ErlDrvPort port, char* cmd) {
  baberl_drv_t* retval = (baberl_drv_t*) driver_alloc(sizeof(baberl_drv_t));
  retval->port = port;
  return (ErlDrvData) retval;
}

static void stop(ErlDrvData handle) {
  baberl_drv_t* driver_data = (baberl_drv_t*) handle;
  driver_free(driver_data);
}

static void process(ErlDrvData handle, ErlIOVec *ev) {
  baberl_drv_t* driver_data = (baberl_drv_t*) handle;
  ErlDrvBinary* args = ev->binv[1];
  char *data = args->orig_bytes;
  char *from_encoding, *to_encoding, *text;
  converted_text_t cv;

  from_encoding = read_string(&data);
  to_encoding = read_string(&data);
  text = read_string(&data);

  cv.error = 0;
  convert_text(from_encoding, to_encoding, text, strlen(text), &cv);

  if (cv.error == 0) {
    ErlDrvTermData spec[] = {ERL_DRV_ATOM, driver_mk_atom("ok"),
			     ERL_DRV_BUF2BINARY, cv.text, cv.text_size,
			     ERL_DRV_TUPLE, 2};
    driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
  }
  else {
    ErlDrvTermData spec[] = {ERL_DRV_ATOM, driver_mk_atom("error"),
			     ERL_DRV_ATOM, driver_mk_atom("iconv_coding"),
			     ERL_DRV_TUPLE, 2};
    driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
  }

  driver_free(from_encoding);
  driver_free(to_encoding);
  driver_free(text);
  driver_free(cv.text);
}
