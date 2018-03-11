#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <limits.h>

#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/misc.h"


CAMLprim value caml_string_of_float_fast_int(value arg)
{
  CAMLparam1(arg);
  CAMLlocal1(res);
  double d = Double_val(arg);
  long long int l;
  
  switch (fpclassify(d)) {
    case FP_NAN: 
      res = caml_copy_string("nan");
      break;
    case FP_INFINITE: 
      if (d > 0)
        res = caml_copy_string("inf");
      else
        res = caml_copy_string("-inf");
      break;
    default:
      l = (long long int)d;
      if (d == (double)l)
        res = caml_alloc_sprintf("%lld", l);
      else
        res = caml_alloc_sprintf("%.12g", d);
      break;
  }
  CAMLreturn(res);
}

