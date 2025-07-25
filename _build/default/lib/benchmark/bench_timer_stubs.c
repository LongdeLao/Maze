#define _POSIX_C_SOURCE 199309L
#include <time.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

CAMLprim value bench_now_ns_ocaml(value unit)
{
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  double ns = (double)ts.tv_sec * 1e9 + (double)ts.tv_nsec;
  return caml_copy_double(ns);
} 