Lowcaml is an experimental OCaml-to-C compiler. It generates C files as well as
the corresponding OCaml bindings. Its primary goal is writing typesafe SIMD
code, but it can also be used to accelerate simple OCaml code and create
bindings to C libraries.

The following features are supported:
- built-in OCaml types (`int`, `int32`, `int64`, `unit`, `bool`, `char`, `string`, `bytes` and `Bigarray.Array1.t`)
- a subset of the OCaml standard library, mostly functions on the above types that don't allocate
- top-level functions
- for and while loops
- if-else expressions
- let bindings (currently only directly below functions)
- externals, which call external C functions directly
- calling other lowcaml functions
- some libc types
- stack-allocated `int` (like OCaml's `ref`), currently called `int Mut.t`
- generating `#include` using `[@@@include "header"]`

The following features are *not* supported, but *may* be supported in the future:
- C array, struct, enum, union, typedef or bindings to types from external libraries
- `static` (non-exported) C functions
- stack-allocated values
- OCaml variants, records, tuples
- `ref` and `array` values
- bytecode stubs
- bounds checks
- sub-modules
- named and optional parameters
- match expressions
- top-level constants
- string literals
- a pure OCaml implementation of `Lowcaml_stdlib` (for jsoo support)

The following features are *not* supported, and are out of scope for the project:
- allocating from lowcaml or calling into the OCaml runtime: All functions generated by lowcaml are marked `[@@noalloc]`
- closures, partial application, exceptions or effects
- cross-platform SIMD bindings (but could be implemented as a third-party library)
- complete libc bindings
- 32-bit platforms
- any particular support for shared memory parallelism

A subset of the OCaml stdlib, as well as some libc and SIMD methods are
exposed. You can browse the interface (wip):
[`lowcaml_stdlib.mli`](lowcaml_stdlib.mli).

Currently, only OCaml 5.0 is supported.


Usage
-----

Dune users can use this library by vendoring it in their project. opam users
can run `dune install` which will install `lowcaml.exe` in their current opam
switch.

You will need a custom rule that invokes `lowcaml.exe` and a library with C
stubs. In the following, `my_stubs_lowcaml.ml` is the input while `lstubs.ml` and
`cstubs.c` are generated files. Dune users can use something similar to this:

```
(rule
 (targets lstubs.ml cstubs.c)
 (deps my_stubs_lowcaml.ml)
 (action (run lowcaml.exe -source my_stubs_lowcaml.ml -o-ml lstubs.ml -o-c cstubs.c)))

(library
 (name lstubs)
 (modules lstubs)
 (foreign_stubs
  (language c)
  (names cstubs)
  (flags
   :standard
   -Wall -Wpedantic -Wconversion -Werror
   -mavx2 ; Note: for SIMD instructions, requires x86_64 with AVX2
   )))
```

Merlin is supported by defining a dummy library. Dune users can use the
following library stanza and run `dune build lowcaml_test_dummy.cma`.

```
(library
 (name lowcaml_merlin_dummy)
 (modules my_stubs_lowcaml)
 (libraries lowcaml.stdlib)
 (flags :standard -nopervasives -open Lowcaml_stdlib))
```


Examples
--------

An implementation of [Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes):

```ocaml
let sieve b =
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
```

```c
// generated by lowcaml
void sieve(const value b)
{
    const int64_t len = (int64_t)caml_string_length(b);
    const int64_t upto = (len-1);
    for(int64_t i = 2; (i<=upto); (i+=1))
    {
        if((*(uint8_t*)&Byte(b, i)==0))
        {
            int64_t j = (2*i);
            while((j<len))
            {
                (*(uint8_t*)&Byte(b, j)=(uint8_t)1);
                (j=(j+i));
            }
        }
    }
}
```

A SIMD-based fill32 for `Bytes.t`:

```ocaml
let simd_fill32 b x =
  let i = Mut.int 0 in
  let len = Bytes.length b in
  let x = SIMD._mm256_set1_epi32 x in
  while !i < len do
    SIMD._mm256_storeu_si256 (Ptr.offset (Ptr.bytes b) !i) x;
    i := !i + 32;
  done
```

```c
// generated by lowcaml
void simd_fill32(const value b, const int32_t x)
{
    int64_t i = 0;
    const int64_t len = (int64_t)caml_string_length(b);
    const __m256i x_1 = _mm256_set1_epi32(x);
    while((i<len))
    {
        _mm256_storeu_si256((void*)((uint8_t*)(void*)Bytes_val(b)+i), x_1);
        (i=(i+32));
    }
}
```

See also [`tests/test_lowcaml.ml`](tests/test_lowcaml.ml). This section will be
expanded.


Technical details
-----------------

Lowcaml generates C code from the typedtree representation in the OCaml
compiler. Since typedtree is not a public API and can change significantly
between OCaml versions, we suggest checking the output into your code
repository. Lowcaml tries to generate readable C code, preserving variable
names and proper indentation.

Lowcaml is somewhat more type-safe than writing C code directly:
- all casts are explicit, thanks to OCaml's stricter type system
- bindings to the generated C code are generated
- externals generate a C declaration, which is checked by the C compiler to match the header

Lowcaml relies on integer overflow and aliasing being defined behaviour, as
enabled by the `-fno-strict-aliasing -fwrapv` flags. These flags are
automatically included by the OCaml compiler for all C stubs.

OCaml types are mapped to C types in the following places:

- let bindings (as the type that appears in the variable C declaration)
- parameters and return types of OCaml functions (which become C functions)
- parameters and return types of OCaml externals (which become C function declarations)

The mapping is as follows:

| OCaml type                    | C type            | let | arg | ret | ext | Notes                                                                  |
|-------------------------------|-------------------|-----|-----|-----|-----|------------------------------------------------------------------------|
| `unit`                        | `value` or `void` |  X  |  X  |  X  |  X  | `value` in arguments of lowcaml functions, `void` otherwise            |
| `int`                         | `int64_t`         |  X  |  X  |  X  |  X  | note: larger than OCaml's built-in `int`                               |
| `int64`                       | `int64_t`         |  X  |  X  |  X  |  X  |                                                                        |
| `int32`                       | `int32_t`         |  X  |  X  |  X  |  X  |                                                                        |
| `bool`                        | `bool`            |  X  |  X  |  X  |  X  | generates conversion stub if used in param or return                   |
| `char`                        | `char`            |  X  |  X  |  X  |  X  | generates conversion stub if used in param or return                   |
| `bytes`                       | `value`           |  X  |  X  |  X  |     |                                                                        |
| `string`                      | `value`           |  X  |  X  |  X  |     |                                                                        |
| `Bigarray.Array1.t`           | `value`           |  X  |  X  |  X  |     | only `Bigarray.C_layout`                                               |
| `int Lowcaml_stdlib.Mut.t`    | `int` or `int*`   |  X  |     |     |     | stack-allocated, memory-safe, not lifetime-safe, similar to `int ref`  |
| `Lowcaml_stdlib.Ptr.t`        | `void*`           |  X  |     |     |  X  | unsafe, primarily for calling external C functions                     |
| `Lowcaml_stdlib.Const_ptr.t`  | `const void*`     |  X  |     |     |  X  | unsafe, primarily for calling external C functions                     |
| `Lowcaml_stdlib.Uint8_t.t`    | `uint8_t`         |  X  |     |     |  X  | primarily for calling external C functions                             |
| `Lowcaml_stdlib.Uint64_t.t`   | `uint64_t`        |  X  |     |     |  X  | primarily for calling external C functions                             |
| `Lowcaml_stdlib.SIMD.__m128i` | `__m128i`         |  X  |     |     |  X  | x86 only, will likely be moved out of stdlib                           |
| `Lowcaml_stdlib.SIMD.__m256i` | `__m256i`         |  X  |     |     |  X  | x86 only, will likely be moved out of stdlib                           |

All C types, except for `Lowcaml_stdlib.Mut.t`, are marked as `const`.


Related projects
----------------

- ["Generating low-level code from a higher-level
  language"](https://okmij.org/ftp/meta-programming/tutorial/genc.html),
  ["Mutable Variables and Reference Types: L-values demystified and
  deprecated"](https://okmij.org/ftp/meta-programming/mutable-var.html), a
  major inspiration for this project. Lowcaml's `Mut.t` is uses this
  compilation scheme.
- [ocaml-ctypes](https://github.com/yallop/ocaml-ctypes), while lowcaml can be
  used to write some bindings to C libraries, its primary goal is writing SIMD
  code. ocaml-ctypes is a more complete and stable project for writing
  bindings.
- [KaRaMeL](https://github.com/FStarLang/karamel), an F*-to-C compiler. More
  low-level than lowcaml, with a focus on correctness proofs.
- [OCaml inline assembly](https://github.com/ocaml/ocaml/pull/162), a
  (rejected) pull request to the OCaml with support for inline assembly.
