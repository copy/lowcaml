let inc x = x + 1
let inc_int64 x = Int64.(x + 1L)
let test_bytes1 b o = Bytes.set_int16_le b o 42
let test_bytes2 b x = Bytes.set_uint8 b 0 x

let test_if x b = if 0 <> x then Bytes.set_int16_le b 0 42
let test_if_else1 x b = if 0 <> x then Bytes.set_int16_le b 0 42 else Bytes.set_int16_le b 0 43
let test_if_else2 x b =
  if x = 0 then
    Bytes.set_int16_le b 0 43
  else if x = 1 then
    Bytes.set_int16_le b 0 44
let test_if_else3 x b =
  if x = 0 then
    Bytes.set_int16_le b 0 43
  else if x = 1 then
    Bytes.set_int16_le b 0 44
  else
    Bytes.set_int16_le b 0 45
let test_if_else4 x y =
  3 + if x = 0 then y else x

let test_open () =
  let open Int64 in
  (3L + of_int Int.(3 + 4))

let test_callee x y =
  x + y * 3
let test_call x y z =
  test_callee x (test_callee y z)

let test_for1 b =
  for i = 0 to 10 do
    Bytes.set_int16_le b (2 * i) i
  done
let test_for2_helper b =
  Bytes.set_int16_le b 0 (1 + Bytes.get_uint16_le b 0);
  10
let test_for2 b =
  for _ = 0 to test_for2_helper b do
    ()
  done

let test_bool1 x =
  let b = x = 1 in
  if b then 42 else 54

let test_float1 x y =
  x +. y *. 3.0 -. 123. /. 7.0

let test_literal_int_1 () = 4611686018427387903
let test_literal_int_2 () = -4611686018427387904
let test_literal_int_3 () = 4611686018427387903 + -4611686018427387904

let test_literal_i64_1 () = 9223372036854775807L
let test_literal_i64_2 () = -9223372036854775808L
let test_literal_i64_3 () = -9223372036854775807L
let test_literal_i64_4 () = Int64.(9223372036854775807L + -9223372036854775808L)

let test_literal_i32_1 () = 2147483647l
let test_literal_i32_2 () = -2147483648l
let test_literal_i32_3 () = Int32.(2147483647l + -2147483648l)

let test_literal_float1 () = 1_2_3_._1_2_3_
let test_literal_float2 () = 1.
let test_literal_float3 () = 1_._
let test_literal_float4 () = -1_1_._4_3_e9_9
let test_literal_float5 () = -12.43e+99
let test_literal_float6 () = 12.43e-99
let test_literal_float7 () = 0x1.
let test_literal_float8 () = 0xa.b
let test_literal_float9 () = 0x1_2_._2_p4_2_

let test_shadow1 x =
  let x = x + 1 in
  x
let test_shadow2 x =
  let y = x + 1 in
  let y = y + 1 in
  let y_1 = x + 1 in
  y + y_1
let test_shadow3 () =
  let i = 42 in
  for i = i to i do
    let i = i + 1 in
    let _ = i in
    ()
  done
let test_shadow4 () =
  let i = 42 in
  while i < 10 do
    let i = i + 1 in
    let _ = i in
    ()
  done
let test_shadow5 () =
  let i = 42 in
  let _ = i in
  for _ = 42 to 54 do
    let i = 54 in
    let _ = i in
    ()
  done

let test_unit1 () () = ()
let test_unit2 () = test_unit1 () ()

(* a prime sieve *)
let test_sieve b =
  let len = Bytes.length b in
  for i = 2 to len - 1 do
    if Bytes.get_uint8 b i = 0 then (
      let j = Mut.int (2 * i) in
      while !j < len do
        Bytes.set_uint8 b !j 1;
        j := !j + i;
      done
    )
  done

external libc_memcpy : Void_ptr.t -> Const_void_ptr.t -> Uint64_t.t -> Void_ptr.t = "memcpy"
external libc_memset : Void_ptr.t -> int32 -> Uint64_t.t -> Void_ptr.t = "memset"
external libc_memchr : Const_void_ptr.t -> int32 -> Uint64_t.t -> Void_ptr.t  = "memchr"
external libc_memrchr : Const_void_ptr.t -> int32 -> Uint64_t.t -> Void_ptr.t  = "memrchr"
external libc_memcmp : Const_void_ptr.t -> Const_void_ptr.t -> Uint64_t.t -> int32  = "memcmp"

let test_external b =
  let p = Void_ptr.bytes b in
  let _ = libc_memcpy p (Void_ptr.to_const (Void_ptr.offset p 1)) (Uint64_t.of_int 1) in
  ()

external libc_getpid : unit -> int32 = "getpid"
let test_external_void () =
  libc_getpid ()

let test_simd_fill b x =
  let i = Mut.int 0 in
  let len = Bytes.length b in
  let x = SIMD._mm256_set1_epi32 x in
  while !i < len do
    SIMD._mm256_storeu_si256 (Ptr.offset (Ptr.bytes b) !i) x;
    i := !i + 32;
  done
let test_simd2 b y =
  SIMD._mm256_storeu_si256 (Ptr.bytes b) (SIMD._mm256_set_epi32 y y y y y y y y)

let test_something (i : int) (i32 : int32) (i64 : int64) =
  let _ = SIMD._mm256_extract_epi64 (SIMD._mm256_set1_epi32 42l) i32 in
  let _ = SIMD._mm_set1_epi32 i32 in
  ()

let test_mut1 x =
  let x = Mut.int x in
  let y = x in
  for _ = 0 to 10 do
    x := !y + 1;
    y := !x + 1;
  done;
  !x

let test_something2 x b =
  let x = Mut.int x in
  let y = x in
  let z = Ptr.of_mut y in
  let w = Ptr.offset z 42 in
  let p = Ptr.bytes b in
  let cp = Const_ptr.bytes b in
  let vp = Void_ptr.bytes b in
  let cvp = Const_void_ptr.bytes b in
  let p2 = Ptr.of_void_ptr vp in
  let p3 = Ptr.of_mut x in
  let cp2 = Const_ptr.of_void_ptr vp in
  let cp3 = Const_ptr.of_const_void_ptr cvp in
  let i1 = Ptr.to_int p in
  ()

(* let test_mut2 () = let x = Mut.int 0 in x *) (* Must not be able to return a Mut.t *)
