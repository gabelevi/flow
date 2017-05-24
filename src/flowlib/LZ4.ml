(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

 external lz4_compress_string: string -> int -> string = "lz4_stub_compress_string"
 external lz4_decompress_string: string -> int -> string = "lz4_stub_decompress_string"

 let compress_string str =
   lz4_compress_string str (String.length str)
   |> Bytes.of_string

 let decompress_string data =
  lz4_decompress_string (Bytes.to_string data) (Bytes.length data)
