(******** OCaml stdlib ********)

module String : sig
  type t = string
  external length : t -> int = "lowcaml_bytes_length"

  (* Note: No bounds checks *)
  external get_uint8 : t -> int -> int = "lowcaml_bytes_get_uint8"
  external get_uint16_le : t -> int -> int = "lowcaml_bytes_get_uint16"
  external get_uint32_le : t -> int -> int = "lowcaml_bytes_get_uint32"
  external get_int32_le : t -> int -> int32 = "lowcaml_bytes_get_int32"
end

module Bytes : sig
  type t = bytes
  external length : t -> int = "lowcaml_bytes_length"

  (* Note: No bounds checks *)
  external get_uint8 : t -> int -> int = "lowcaml_bytes_get_uint8"
  external set_uint8 : t -> int -> int -> unit = "lowcaml_bytes_set_int8"
  external get_uint16_le : t -> int -> int = "lowcaml_bytes_get_uint16"
  external get_uint32_le : t -> int -> int = "lowcaml_bytes_get_uint32"
  external get_int32_le : t -> int -> int32 = "lowcaml_bytes_get_int32"

  external set_int16_le : t -> int -> int -> unit = "lowcaml_bytes_set_int16"
end

module Int : sig
  type t = int
  external ( + ) : t -> t -> t = "%addint"
end

module Int64 : sig
  type t = int64
  external of_int : int -> t = "%identity"
  external to_int : t -> int = "%identity"
  external of_int32 : int32 -> t = "lowcaml_int32_to_int"
  external to_int32 : t -> int32 = "lowcaml_int32_of_int"

  external ( + ) : t -> t -> t = "%addint"
  external ( asr ) : t -> int -> t = "%asrint"
  external ( lsr ) : t -> int -> t = "%lsrint"
end

module Int32 : sig
  type t = int32
  external of_int : int -> t = "lowcaml_int32_of_int"
  external to_int : t -> int = "lowcaml_int32_to_int"

  external ( + ) : t -> t -> t = "%addint"
  external ( * ) : t -> t -> t = "%mulint"
  external ( lsl ) : t -> int -> t = "%lslint"
  external ( asr ) : t -> int -> t = "%asrint"
  external ( lsr ) : t -> int -> t = "%int32_lsr"
end

module Float : sig
  type t = float
end


external (=) : int -> int -> bool = "%equal"
external (<>) : int -> int -> bool = "%notequal"
external (>) : int -> int -> bool = "%greaterthan"
external (>=) : int -> int -> bool = "%lessequal"
external (<) : int -> int -> bool = "%lessthan"
external (<=) : int -> int -> bool = "%greaterequal"

external ( ~- ) : int -> int = "%negint"
external ( ~+ ) : int -> int = "%identity"
(* external succ : int -> int = "%succint" *)
(* external pred : int -> int = "%predint" *)
external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
(* external ( / ) : int -> int -> int = "%divint" *)
(* external ( mod ) : int -> int -> int = "%modint" *)

external ( land ) : int -> int -> int = "%andint"
external ( lor ) : int -> int -> int = "%orint"
external ( lxor ) : int -> int -> int = "%xorint"
(* val lnot : int -> int *)
external ( lsl ) : int -> int -> int = "%lslint"
external ( lsr ) : int -> int -> int = "%lsrint"
external ( asr ) : int -> int -> int = "%asrint"

external not : bool -> bool = "%boolnot"
external ( && ) : bool -> bool -> bool = "%sequand"
external ( || ) : bool -> bool -> bool = "%sequor"

external ( ~-. ) : float -> float = "%negfloat"
external ( ~+. ) : float -> float = "%identity"
external ( +. ) : float -> float -> float = "%addfloat"
external ( -. ) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external ( /. ) : float -> float -> float = "%divfloat"


(******** C types & libc bindings ********)

module Mut : sig
  type 'a t
  (** the type of stack-allocated values *)

  external get : 'a t -> 'a = "lowcaml_deref"

  external int : int -> int t = "lowcaml_mut_create"
  (** Allocate a new int. Can only appear in let bindings *)
end

external (:=) : 'a Mut.t -> 'a -> unit = "lowcaml_mut_set"
external (!) : 'a Mut.t -> 'a = "lowcaml_deref"

module Const_ptr : sig
  type t
  (** The [const void*] type *)

  external string : string -> t = "lowcaml_string_to_constptr"
  external bytes : bytes -> t = "lowcaml_string_to_constptr"
  external bigarray : (_, _, Bigarray.c_layout) Bigarray.Array1.t -> t = "lowcaml_bigarray_to_ptr"

  external to_int : t -> int = "lowcaml_ptr_to_int"
  external is_null : t -> bool = "%boolnot"

  external offset : t -> int -> t = "lowcaml_ptr_offset"

  external of_mut : 'a Mut.t -> t = "%identity"
end

module Ptr : sig
  type t
  (** The [void*] type *)

  external bytes : bytes -> t = "lowcaml_bytes_to_ptr"
  external bigarray : (_, _, Bigarray.c_layout) Bigarray.Array1.t -> t = "lowcaml_bigarray_to_ptr"

  external to_int : t -> int = "lowcaml_ptr_to_int"
  external to_const : t -> Const_ptr.t = "lowcaml_ptr_to_const_ptr"
  external is_null : t -> bool = "%boolnot"

  external offset : t -> int -> t = "lowcaml_ptr_offset"

  (* external write64 : t -> int -> unit = "lowcaml_ptr_write64" *)

  external of_mut : 'a Mut.t -> t = "%identity"
end

module Uint8_t : sig
  type t
  (** The [uint8_t] type *)

  external of_int : int -> t = "lowcaml_int_to_uint8_t"
end
module Uint64_t : sig
  type t
  (** The [uint64_t] type *)

  external of_int : int -> t = "lowcaml_int_to_uint64_t"
end

external assert_ : bool -> unit = "assert"


(******** x86 AVX SIMD instructions ********)

module SIMD : sig
  type __m128i
  external _mm_set1_epi32 : int32 -> __m128i = "_mm_set1_epi32"
  external _mm_and_si128 : __m128i -> __m128i -> __m128i = "_mm_and_si128"
  external _mm_cmpeq_epi32 : __m128i -> __m128i -> __m128i = "_mm_cmpeq_epi32"
  external _mm_blendv_epi8 : __m128i -> __m128i -> __m128i -> __m128i = "_mm_blendv_epi8"
  external _mm_storeu_si64 : Ptr.t -> __m128i -> unit = "_mm_storeu_si64"
  external _mm_storeu_si128 : Ptr.t -> __m128i -> unit = "_mm_storeu_si128"

  type __m256i
  external _mm256_set1_epi32 : int32 -> __m256i = "_mm256_set1_epi32"
  external _mm256_set_epi32 : int32 -> int32 -> int32 -> int32 -> int32 -> int32 -> int32 -> int32 -> __m256i = "_mm256_set_epi32"
  external _mm256_cmpeq_epi32 : __m256i -> __m256i -> __m256i = "_mm256_cmpeq_epi32"
  external _mm256_and_si256 : __m256i -> __m256i -> __m256i = "_mm256_and_si256"
  external _mm256_blendv_epi8 : __m256i -> __m256i -> __m256i -> __m256i = "_mm256_blendv_epi8"
  external _mm256_storeu_si256 : Ptr.t -> __m256i -> unit = "_mm256_storeu_si256"
  external _mm256_store_si256 : Ptr.t -> __m256i -> unit = "_mm256_store_si256"
  external _mm256_stream_si256 : Ptr.t -> __m256i -> unit = "_mm256_stream_si256"

  external _mm256_extract_epi64 :  __m256i -> int32 -> int = "_mm256_extract_epi64"
  external _mm256_extracti128_si256 :  __m256i -> int32 -> __m128i = "_mm256_extracti128_si256"
  external _mm256_castsi256_si128 : __m256i -> __m128i = "_mm256_castsi256_si128"

  (* external _mm256_storeu_epi8 : Ptr.t -> __m256i -> unit = "_mm256_storeu_epi8" *) (* AVX512 *)
end
