/**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */

#include <string.h>

#include <caml/alloc.h>
#include <caml/memory.h>

#include <lz4.h>

CAMLprim value lz4_stub_compress_string(
  value ocaml_string,
  value ocaml_length
) {
  CAMLparam2(ocaml_string, ocaml_length);
  CAMLlocal1(result);

  char *str = String_val(ocaml_string);
  const uint64_t len = Long_val(ocaml_length);
  printf("Trying to compress string with len %d\n", len);

  size_t max_compression_size = LZ4_compressBound(len);
  printf("Max compression size is %d\n", max_compression_size);
  char* compressed_data = malloc(max_compression_size);
  printf("Data going to %p\n", compressed_data);
  size_t compressed_size = LZ4_compress_default(
    str,
    compressed_data,
    len,
    max_compression_size);
  printf("Actual compressed size: %d\n", compressed_size);

  result = caml_alloc_string(compressed_size);

  printf("Alloc'd a string\n");
  memcpy(String_val(result), compressed_data, compressed_size);
  printf("memcpy worked\n");
  free(compressed_data);
  printf("Freed");

  CAMLreturn(result);
 }

CAMLprim value lz4_stub_decompress_string(
  value ocaml_data,
  value ocaml_length
) {
  CAMLparam2(ocaml_data, ocaml_length);
  CAMLlocal1(result);

  char *data = String_val(ocaml_data);
  const uint64_t len = Long_val(ocaml_length);

  const uint64_t max_size = len * 100;

  char *decompressed_data = malloc(max_size);
  size_t decompressed_size = LZ4_decompress_safe(
    data,
    decompressed_data,
    len,
    max_size);

  result = caml_alloc_string(decompressed_size);
  memcpy(String_val(result), decompressed_data, decompressed_size);
  free(decompressed_data);

  CAMLreturn(result);
 }
