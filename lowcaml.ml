(* 0.1 release checklist
   - more examples/tests
*)
(* TODO
   - top-level constants
   - match/switch
   - lift let-bindings in expressions
   - lift mut creation
   - built-in C types (uint8_t, etc.)
   - custom C types (struct)
     - clock_gettime64 needs [struct timespec t; clock_gettime(&t)]
       - maybe via arrays?
   - tests
     - failing (expect) tests
   - generated code beauty
     - remove parenthesis from expressions (operator precedence)
     - remove shadowing suffix if allowed due to blocks
     - un-nest if-else chains
     - preserve comments
     - preserve number literals
     - group includes together
   - bounds checks
   - check whether int fits into OCaml int before returning it (likely requires stubs)
   - complete stdlib
   - use fsanitize=undefined
   - generate (void)x to handle unused warning (enable -Wextra)
   - sub-modules
   - inner functions
   - named arguments
   - optional arguments
   - 'a -> void*
   - [@inline] -> __attribute__((always_inline))
   - (local) try -> goto
   - "export"/static
   - let (bar[@c_name "foo"]) x y =
   - type foo [@c_name "foo"]
   - nicer cmdline interface
   - lowcaml libraries (install mli+h?)
     - convert lowcaml_stdlib to library
   - order of side effects (e.g. in parameters)?
   - track dependencies of headers, remove unused header includes
   - include ocaml-style error in Not_supported
   - portable vector instructions? (http://gcc.gnu.org/onlinedocs/gcc/Vector-Extensions.html)

   - things to rework
     - the pointer types

   - examples
     - sieve
     - memchr (simd + bindings)
     - bigarray ops

   - porting
     - Array.sort (from OCaml stdlib)
     - bigstringaf (or other) stubs
     - add two float bigarrays (like owl)
     - bitops (popcount, bsr, bsf)
     - hardware aes (for rng/hash)
*)

let logging = ref false
let log fmt =
  if !logging
  then Format.kfprintf (fun f -> Format.pp_print_newline f ()) Format.err_formatter fmt
  else Format.ikfprintf (fun _ -> ()) Format.err_formatter fmt
let print fmt = Format.eprintf (fmt ^^ "@.")
let failwithf fmt = Format.kasprintf failwith fmt
let sprintf = Printf.sprintf

exception Not_supported of string
let not_supported fmt = Format.kasprintf (fun msg -> raise (Not_supported msg)) fmt

module C_ast = struct
  type identifier = I of string [@unboxed]

  let create_identifer i =
    if i = "" || i.[0] <= '9' || not @@ String.for_all (function '0'..'9' | 'a'..'z' | 'A'..'Z' | '_' -> true | _ -> false) i then
      not_supported "Bad C identifier: %S" i;
    I i

  type ty =
    | I64 | I32 | I16 | I8
    | U64 | U32 | U16 | U8
    | Bool | Char
    | Float | Double
    | M128i | M256i | M512i
    | Mmask16
    | Value
    | Void_ptr | Const_void_ptr
    | Ptr of ty
    | Const of ty

  type expression =
    | Constant of int
    | Constant_i64 of int64
    | Constant_float of float
    | Constant_char of char
    | Constant_bool of bool
    | Call of identifier * expression list
    | Op1 of string * expression
    | Op2 of string * expression * expression
    | Ternary of expression * expression * expression
    | Variable of identifier
    | Deref of expression
    | Addr_of of expression
    | Cast of ty * expression

  type declaration = ty * identifier * expression

  type statement =
    | Declaration of declaration
    | Expression of expression
    | For of declaration * expression * expression * statement list
    | While of expression * statement list
    | If_then of expression * statement list
    | If_then_else of expression * statement list * statement list
    | Return of expression

  type element =
    | Function of {
        name: identifier;
        args: (identifier * ty) list;
        return_type: ty option;
        body: statement list;
      }
    | Prototype of {
        name: identifier;
        args: ty list;
        return_type: ty option;
      }
    | Include of string

  type t = {
    elements: element list;
  }

  module Print = struct
    let pr = Buffer.add_string
    let pr_indent buf len = Buffer.add_string buf (String.make (4 * len) ' ')
    let rec sep buf seperator f = function
      | [] -> ()
      | [x] -> f buf x
      | x :: xs -> f buf x; pr buf seperator; sep buf seperator f xs

    let rec ty buf = function
      | I64 -> pr buf "int64_t"
      | I32 -> pr buf "int32_t"
      | I16 -> pr buf "int16_t"
      | I8 -> pr buf "int8_t"
      | U64 -> pr buf "uint64_t"
      | U32 -> pr buf "uint32_t"
      | U16 -> pr buf "uint16_t"
      | U8 -> pr buf "uint8_t"
      | Bool -> pr buf "bool"
      | Char -> pr buf "char"
      | Float -> pr buf "float"
      | Double -> pr buf "double"
      | M128i -> pr buf "__m128i"
      | M256i -> pr buf "__m256i"
      | M512i -> pr buf "__m512i"
      | Mmask16 -> pr buf "__mmask16"
      | Value -> pr buf "value"
      | Void_ptr -> pr buf "void*"
      | Const_void_ptr -> pr buf "const void*"
      | Ptr t -> ty buf t; pr buf "*"
      | Const Const_void_ptr -> pr buf "const void*const"
      | Const Void_ptr -> pr buf "void*const"
      | Const Ptr t -> ty buf t; pr buf "*const"
      | Const Const _ -> failwith "Bad C type: nested const"
      | Const t -> pr buf "const "; ty buf t

    let arg buf (I var, t) =
      ty buf t;
      pr buf " ";
      pr buf var

    let rec expression buf = function
      | Constant i -> pr buf (string_of_int i);
      | Constant_i64 i when i = Int64.min_int -> pr buf "(-9223372036854775807-1)"; (* "integer constant is so large that it is unsigned" *)
      | Constant_i64 i -> pr buf (Int64.to_string i);
      | Constant_float f -> pr buf (Float.to_string f);
      | Constant_char '\\' -> pr buf "'\\\\'"
      | Constant_char c when c >= ' ' && c <= '~' -> pr buf (sprintf "'%c'" c);
      | Constant_char c -> pr buf (sprintf "'\\x%02x'" (Char.code c));
      | Constant_bool b -> pr buf (string_of_bool b);
      | Variable I v -> pr buf v;
      | Op1 (op, arg) -> pr buf "("; pr buf op; expression buf arg; pr buf ")"
      | Op2 (op, lhs, rhs) ->
         pr buf "("; expression buf lhs; pr buf op; expression buf rhs; pr buf ")"
      | Ternary (cond, then_, else_) ->
        pr buf "(("; expression buf cond; pr buf ") ? (";
        expression buf then_; pr buf ") : (";
        expression buf else_; pr buf "))";
      | Call (I f, args) -> pr buf f; pr buf "("; sep buf ", " expression args; pr buf ")"
      | Deref e -> pr buf "*"; expression buf e
      | Addr_of e -> pr buf "&"; expression buf e
      | Cast (t, e) -> pr buf "("; ty buf t; pr buf ")"; expression buf e

    let declaration buf indent (t, I var, expr) =
      pr_indent buf indent;
      ty buf t;
      pr buf " ";
      pr buf var;
      pr buf " = ";
      expression buf expr;
      pr buf ";"

    let rec statement indent buf = function
      | Declaration d ->
        declaration buf indent d
      | Expression expr ->
        pr_indent buf indent;
        expression buf expr;
        pr buf ";"
      | For (d, cond, after, body) ->
        pr_indent buf indent;
        pr buf "for(";
        declaration buf 0 d;
        pr buf " ";
        expression buf cond;
        pr buf "; ";
        expression buf after;
        pr buf ")\n";
        statements buf indent body;
      | While (cond, body) ->
        pr_indent buf indent;
        pr buf "while(";
        expression buf cond;
        pr buf ")\n";
        statements buf indent body
      | If_then (cond, then_) ->
        pr_indent buf indent;
        pr buf "if(";
        expression buf cond;
        pr buf ")\n";
        statements buf indent then_;
      | If_then_else (cond, then_, else_) ->
        pr_indent buf indent;
        pr buf "if(";
        expression buf cond;
        pr buf ")\n";
        statements buf indent then_;
        pr buf "\n";
        pr_indent buf indent;
        pr buf "else\n";
        statements buf indent else_
      | Return expr ->
        pr_indent buf indent;
        pr buf "return ";
        expression buf expr;
        pr buf ";"

    and statements buf indent statements =
      pr_indent buf indent;
      pr buf "{\n";
      sep buf "\n" (statement (1 + indent)) statements;
      pr buf "\n"; (* Maybe merge into [sep]? *)
      pr_indent buf indent;
      pr buf "}"

    let element buf = function
      | Function { return_type; name = I name; args; body } ->
        (match return_type with None -> pr buf "void" | Some t -> ty buf t);
        pr buf " ";
        pr buf name;
        pr buf "(";
        (match args with
         | [] -> pr buf "void"
         | args -> sep buf ", " arg args);
        pr buf ")\n";
        statements buf 0 body
      | Prototype { name = I name; args; return_type } ->
        (match return_type with None -> pr buf "void" | Some t -> ty buf t);
        pr buf " ";
        pr buf name;
        pr buf "(";
        (match args with
         | [] -> pr buf "void"
         | args -> sep buf ", " ty args);
        pr buf ");"
      | Include name ->
        pr buf "#include ";
        pr buf name

    let print { elements } =
      let buf = Buffer.create 1000 in
      Buffer.add_string buf "// generated by lowcaml\n";
      sep buf "\n\n" element elements;
      pr buf "\n";
      Buffer.contents buf
  end

  module Simplify = struct
    let rec simplify_expression = function
      | Deref (Addr_of x) -> x (* appears naturally due to Mut.t *)

      | Call (i, args) -> Call (i, List.map simplify_expression args)
      | Op1 (op, rhs) -> Op1 (op, simplify_expression rhs)
      | Op2 (op, lhs, rhs) -> Op2 (op, simplify_expression lhs, simplify_expression rhs)
      | Ternary (i, t, e) -> Ternary (simplify_expression i, simplify_expression t, simplify_expression e)
      | Constant _ | Constant_bool _ | Constant_float _ | Constant_i64 _ | Constant_char _ as x -> x
      | Variable _ as v -> v
      | Deref e -> Deref (simplify_expression e)
      | Addr_of e -> Addr_of (simplify_expression e)
      | Cast (ty, e) -> Cast (ty, simplify_expression e)

    let rec simplify_statement = function
      | Declaration _ as d -> d
      | Expression e -> Expression (simplify_expression e)
      | For (d, cond, after, body) -> For (d, simplify_expression cond, simplify_expression after, simplify_body body)
      | While (cond, body) -> While (simplify_expression cond, simplify_body body)
      | If_then (cond, body) -> If_then (simplify_expression cond, simplify_body body)
      | If_then_else (cond, then_, else_) -> If_then_else (simplify_expression cond, simplify_body then_, simplify_body else_)
      | Return e -> Return (simplify_expression e)
    and simplify_body s = List.map simplify_statement s

    let simplify_element = function
      | Function f -> Function { f with body = simplify_body f.body }
      | Prototype _ | Include _ as el -> el

    let go t = { elements = List.map simplify_element t.elements }
  end
end

module Names = struct
  module M = Map.Make(String)

  type t = {
    scope: int;
    idents: (C_ast.identifier * [`Mut | `Const]) Ident.Map.t;
    map: string M.t;
  }

  let empty = { scope = 0; map = M.empty; idents = Ident.Map.empty }

  let enter_scope t = { t with scope = t.scope + 1 }

  let get t id = Ident.Map.find id t.idents

  let new_var t ?(mut=false) ?ident var =
    let rec find_free_var i =
      let var = if i = 0 then var else sprintf "%s_%d" var i in
      if M.find_opt var t.map = None then var
      else find_free_var (i + 1)
    in
    let var = find_free_var 0 in
    { t with
      idents = (match ident with Some id -> Ident.Map.add id (C_ast.create_identifer var, if mut then `Mut else `Const) t.idents | None -> t.idents);
      map = M.add var var t.map },
    C_ast.create_identifer var
end

module OCaml_type = struct
  type t =
    | Type_variable of { name: string }
    | Int
    | Int32
    | Int64
    | Float
    | Char
    | Bool
    | Unit
    | Bytes
    | String
    | Bigarray
    (* Lowcaml_stdlib types below *)
    | F32
    | U8
    | U16
    | U32
    | U64
    | I8
    | I16
    | M128i
    | M256i
    | M512i
    | Mmask16
    | Void_ptr
    | Const_void_ptr
    | Ptr of t
    | Const_ptr of t
    | Mut of t

  let rec map ~where env ty =
    match Types.get_desc (Ctype.expand_head env ty) with
    | Tvar (Some name) -> Type_variable { name = "'" ^ name }
    | Tvar None -> Type_variable { name = "_" }
    | Tconstr (path, [], _) when Path.same path Predef.path_unit -> Unit
    | Tconstr (path, [], _) when Path.same path Predef.path_bool -> Bool
    | Tconstr (path, [], _) when Path.same path Predef.path_int -> Int
    | Tconstr (path, [], _) when Path.same path Predef.path_int32 -> Int32
    | Tconstr (path, [], _) when Path.same path Predef.path_int64 -> Int64
    | Tconstr (path, [], _) when Path.same path Predef.path_float -> Float
    | Tconstr (path, [], _) when Path.same path Predef.path_char -> Char
    | Tconstr (path, [], _) when Path.same path Predef.path_bytes -> Bytes
    | Tconstr (path, [], _) when Path.same path Predef.path_string -> String
    | Tconstr (path, [], _) ->
      (match Path.name path with
       | "Lowcaml_stdlib.F32.t" -> F32
       | "Lowcaml_stdlib.SIMD.__m128i" -> M128i
       | "Lowcaml_stdlib.SIMD.__m256i" -> M256i
       | "Lowcaml_stdlib.SIMD.__m512i" -> M512i
       | "Lowcaml_stdlib.SIMD.__mmask16" -> Mmask16
       | "Lowcaml_stdlib.Void_ptr.t" -> Void_ptr
       | "Lowcaml_stdlib.Const_void_ptr.t" -> Const_void_ptr
       | "Lowcaml_stdlib.Uint8_t.t" -> U8
       | "Lowcaml_stdlib.Uint16_t.t" -> U16
       | "Lowcaml_stdlib.Uint32_t.t" -> U32
       | "Lowcaml_stdlib.Uint64_t.t" -> U64
       | "Lowcaml_stdlib.Int8_t.t" -> I8
       | "Lowcaml_stdlib.Int16_t.t" -> I16
       | _ -> not_supported "unknown type: %a in %s" Printtyp.type_expr ty where)
    | Tconstr (path, [tyarg], _) ->
      (match Path.name path with
       | "Lowcaml_stdlib.Ptr.t" -> Ptr (map ~where env tyarg)
       | "Lowcaml_stdlib.Const_ptr.t" -> Const_ptr (map ~where env tyarg)
       | "Lowcaml_stdlib.Mut.t" -> Mut (map ~where env tyarg)
       | _ -> not_supported "unknown type: %a in %s" Printtyp.type_expr ty where)
    | Tconstr (path, [_ty; _kind; _layout], _) when Path.name path = "Stdlib__Bigarray.Array1.t" ->
      Bigarray (* TODO: check 3rd type parameter is c_layout? *)
    | _ ->
      not_supported "type expression: %a in %s" Printtyp.type_expr ty where

  let rec show = function
    | Type_variable { name } -> name
    | Int -> "int"
    | Int32 -> "int32"
    | Int64 -> "int64"
    | Float -> "float"
    | Char -> "char"
    | Bool -> "bool"
    | Unit -> "unit"
    | Bytes -> "bytes"
    | String -> "string"
    | Bigarray -> "(_, _, _) Bigarray.Array1.t"
    | F32 -> "Lowcaml_stdlib.F32.t"
    | U8 -> "Lowcaml_stdlib.Uint8_t.t"
    | U16 -> "Lowcaml_stdlib.Uint16_t.t"
    | U32 -> "Lowcaml_stdlib.Uint32_t.t"
    | U64 -> "Lowcaml_stdlib.Uint64_t.t"
    | I8 -> "Lowcaml_stdlib.Int8_t.t"
    | I16 -> "Lowcaml_stdlib.Int16_t.t"
    | M128i -> "Lowcaml_stdlib.SIMD.__m128i"
    | M256i -> "Lowcaml_stdlib.SIMD.__m256i"
    | M512i -> "Lowcaml_stdlib.SIMD.__m512i"
    | Mmask16 -> "Lowcaml_stdlib.SIMD.__mmask16"
    | Void_ptr -> "Lowcaml_stdlib.Void_ptr.t"
    | Const_void_ptr -> "Lowcaml_stdlib.Const_void_ptr.t"
    | Ptr t -> show t ^ " Lowcaml_stdlib.Ptr.t"
    | Const_ptr t -> show t ^ " Lowcaml_stdlib.Const_ptr.t"
    | Mut t -> show t ^ " Lowcaml_stdlib.Mut.t"

  (* TODO: random test: x = map (show x) *)
end

module Lowcaml = struct
  open Typedtree
  open C_ast

  let print_structure_item fmt str = Pprintast.structure_item fmt (Untypeast.default_mapper.structure_item Untypeast.default_mapper str)
  let print_expr fmt e = Pprintast.expression fmt (Untypeast.untype_expression e)
  let print_pat fmt p = Pprintast.pattern fmt (Untypeast.untype_pattern p)

  let rec ocaml_type_to_ctype ~where = function
    | OCaml_type.Int | Int64 -> I64
    | Int32 -> I32
    | Float -> Double
    | Bool -> Bool
    | Char -> Char
    | Bytes | String | Bigarray | Unit -> Value
    | F32 -> Float
    | U8 -> U8
    | U16 -> U16
    | U32 -> U32
    | U64 -> U64
    | I8 -> I8
    | I16 -> I16
    | M128i -> M128i
    | M256i -> M256i
    | M512i -> M512i
    | Mmask16 -> Mmask16
    | Void_ptr -> Void_ptr
    | Const_void_ptr -> Const_void_ptr
    | Ptr Type_variable _ | Mut Type_variable _ -> Void_ptr
    | Const_ptr Type_variable _ -> Const_void_ptr
    | Ptr t -> Ptr (ocaml_type_to_ctype ~where t)
    | Const_ptr t -> Ptr (Const (ocaml_type_to_ctype ~where t))
    | Mut t -> Ptr (ocaml_type_to_ctype ~where t)
    | Type_variable _ as t -> not_supported "type variable: %s" (OCaml_type.show t)

  let ocaml_type_to_external_type ~where = function
    | OCaml_type.Int -> "(int[@untagged])"
    | Int64 -> "(int64[@unboxed])"
    | Int32 -> "(int32[@unboxed])"
    | Float -> "(float[@unboxed])"
    | Bool | Char -> "(int[@untagged])" (* converted to int in OCaml stub *)
    | Bytes -> "bytes"
    | String -> "string"
    | Unit -> "unit"
    | Bigarray -> "(_, _, Bigarray.c_layout) Bigarray.Array1.t" (* TODO: also map type parameters *)
    | F32 | U8 | U16
    | U32 | U64 | I8
    | I16
    | M128i | M256i | M512i
    | Mmask16
    | Void_ptr | Const_void_ptr
    | Ptr _ | Const_ptr _
    | Mut _ as t ->
      not_supported "type: %s in %s" (OCaml_type.show t) where
    | Type_variable _ as t ->
      not_supported "type variable: %s in %s" (OCaml_type.show t) where

  let is_unit ty =
    match Types.get_desc ty with
    | Tconstr (path, [], _) when Path.same path Predef.path_unit -> true
    | _ -> false

  let is_bool ty =
    match Types.get_desc ty with
    | Tconstr (path, [], _) when Path.same path Predef.path_bool -> true
    | _ -> false

  let rec get_var_from_pat pat =
    match pat.pat_desc with
    | Tpat_var (ident, var) -> Some ident, var.txt
    | Tpat_any -> None, "unused"
    | Tpat_construct (_, constr, _, _) when is_unit constr.cstr_res -> None, "unit"
    | Tpat_alias (p, ident, var) -> let _ = get_var_from_pat p in Some ident, var.txt
    | Tpat_variant _ -> not_supported "variant pattern: %a" print_pat pat
    | Tpat_construct _ -> not_supported "construct pattern: %a" print_pat pat
    | _ -> not_supported "pattern: %a" print_pat pat

  let rec get_args expr =
    (* TODO: not_supported "mixing unit and non-unit arguments: %a" print_pat pat *)
    match expr.exp_desc with
    | Texp_function {
        arg_label = Nolabel;
        param;
        cases = [{ c_lhs; c_guard = None; c_rhs }];
        partial = Total;
      } ->
      let rest, body = get_args c_rhs in
      let _ident, name = get_var_from_pat c_lhs in
      (param, name, c_lhs.pat_type) :: rest, body
    | Texp_function _ ->
      not_supported "partial/label/pattern in function: %a" print_expr expr
    | _ ->
      [], expr

  let generate_primitive name args =
    let bad_arity expected f = failwithf "Bad arity: %s expected %d got %d" f expected (List.length args) in
    let args1 f = function [x] -> x | _ -> bad_arity 1 f in
    let args2 f = function [x; y] -> x, y | _ -> bad_arity 2 f in
    let args3 f = function [x; y; z] -> x, y, z | _ -> bad_arity 3 f in
    let bytes_get size args =
      let buf, offset = args2 name args in
      (* NOTE: only ub-safe because all C code is compiled with -fno-strict-aliasing *)
      (* Deref (Cast (Ptr size, Op2 ("+", Call (create_identifer "Bytes_val", [buf]), offset))) *)
      Deref (Cast (Ptr size, Addr_of (Call (create_identifer "Byte", [buf; offset]))))
    in
    let bytes_set size args =
      let buf, offset, value = args3 name args in
      (* Op2 ("=", Deref (Cast (Ptr size, Op2 ("+", Call (create_identifer "Bytes_val", [buf]), offset))), value) *)
      Op2 ("=", Deref (Cast (Ptr size, Addr_of (Call (create_identifer "Byte", [buf; offset])))), Cast (size, value))
    in
    match name with
    | "lowcaml_bytes_length" ->
      let buf = args1 name args in
      Cast (I64, Call (create_identifer "caml_string_length", [buf]))
    | "lowcaml_bytes_get_uint8" -> bytes_get U8 args
    | "lowcaml_bytes_get_uint16" -> bytes_get U16 args
    | "lowcaml_bytes_get_uint32" -> bytes_get U32 args
    | "lowcaml_bytes_get_int32" -> bytes_get I32 args
    | "lowcaml_bytes_set_int8" -> bytes_set U8 args
    | "lowcaml_bytes_set_int16" -> bytes_set U16 args
    | "lowcaml_ptr_offset" ->
      (* (void* )((uint8_t* )ptr+offset) *)
      let ptr, offset = args2 name args in
      Cast (Void_ptr, Op2 ("+", Cast (Ptr U8, ptr), offset))
(*
    | "lowcaml_ptr_offset" ->
      (* (ptr+offset) *)
      let ptr, offset = args2 name args in
      Op2 ("+", ptr, offset)
*)
    | "lowcaml_ptr_to_int" ->
      let ptr = args1 name args in
      Cast (I64, ptr)
    | "lowcaml_ptr_get_uint8" ->
      let ptr = args1 name args in
      Deref (Cast (Ptr U8, ptr))
    | "lowcaml_ptr_write64" ->
      let ptr, value = args2 name args in
      Op2 ("=", Deref (Cast (Ptr I64, ptr)), value)
    | "lowcaml_bigarray_to_ptr" ->
      let bigarray = args1 name args in
      Cast (Void_ptr, Call (create_identifer "Caml_ba_data_val", [bigarray]))
    | "lowcaml_bytes_to_ptr" ->
      let bytes = args1 name args in
      Cast (Void_ptr, Call (create_identifer "Bytes_val", [bytes]))
    | "lowcaml_string_to_constptr" ->
      let bytes = args1 name args in
      Cast (Const_void_ptr, Call (create_identifer "String_val", [bytes]))
    | "lowcaml_ptr_to_const_ptr" ->
      Cast (Const_void_ptr, args1 name args)
    | "lowcaml_int32_to_int" ->
      Cast (I64, args1 name args)
    | "lowcaml_char_to_int" ->
      Op2 ("&", Constant 0xFF, Cast (I64, args1 name args))
    | "lowcaml_int32_of_int" ->
      Cast (I32, args1 name args)
    | "lowcaml_int32_of_char" ->
      Op2 ("&", Constant 0xFF, Cast (I32, args1 name args))
    | "lowcaml_int_to_uint64_t" ->
      Cast (U64, args1 name args)
    | "lowcaml_deref" ->
      Deref (args1 name args)
    | "lowcaml_mut_set" ->
      let ptr, value = args2 name args in
      Op2 ("=", Deref ptr, value)
    | "lowcaml_mut_create" ->
      not_supported "Mut.t is not allowed here"
    | "%identity" ->
      args1 name args
    | "%equal" -> let lhs, rhs = args2 name args in Op2 ("==", lhs, rhs)
    | "%notequal" -> let lhs, rhs = args2 name args in Op2 ("!=", lhs, rhs)
    | "%greaterthan" -> let lhs, rhs = args2 name args in Op2 (">", lhs, rhs)
    | "%lessthan" -> let lhs, rhs = args2 name args in Op2 ("<", lhs, rhs)
    | "%greaterequal" -> let lhs, rhs = args2 name args in Op2 (">=", lhs, rhs)
    | "%lessequal" -> let lhs, rhs = args2 name args in Op2 ("<=", lhs, rhs)
    | "%negint" -> Op1 ("-", args1 name args)
    | "%addint" | "%addfloat" -> let lhs, rhs = args2 name args in Op2 ("+", lhs, rhs)
    | "%subint" | "%subfloat" -> let lhs, rhs = args2 name args in Op2 ("-", lhs, rhs)
    | "%mulint" | "%mulfloat" -> let lhs, rhs = args2 name args in Op2 ("*", lhs, rhs)
    | "%divfloat" -> let lhs, rhs = args2 name args in Op2 ("/", lhs, rhs)
    | "%andint" -> let lhs, rhs = args2 name args in Op2 ("&", lhs, rhs)
    | "%orint" -> let lhs, rhs = args2 name args in Op2 ("|", lhs, rhs)
    | "%xorint" -> let lhs, rhs = args2 name args in Op2 ("^", lhs, rhs)
    | "%lslint" -> let lhs, rhs = args2 name args in Op2 ("<<", lhs, rhs)
    | "%lsrint" -> let lhs, rhs = args2 name args in Cast (I64, Op2 (">>", Cast (U64, lhs), rhs))
    | "%int32_lsr" -> let lhs, rhs = args2 name args in Cast (I32, Op2 (">>", Cast (U32, lhs), rhs))
    | "%asrint" -> let lhs, rhs = args2 name args in Op2 (">>", lhs, rhs)
    | "%boolnot" -> Op1 ("!", args1 name args)
    | "%sequand" -> let lhs, rhs = args2 name args in Op2 ("&&", lhs, rhs)
    | "%sequor" -> let lhs, rhs = args2 name args in Op2 ("||", lhs, rhs)
    | name when
        String.starts_with ~prefix:"%" name ||
        String.starts_with ~prefix:"lowcaml" name ->
      failwithf "Unimplemented primitive: %s" name;
    | name ->
      Call (create_identifer name, args)

  let rec generate_simple_expression names expr =
    match expr.exp_desc with
    | Texp_apply ({ exp_desc = Texp_ident (_path, _, { val_kind = Val_prim prim; _ }); _ }, args) ->
      let args =
        match args with
        | [Asttypes.Nolabel, Some t] when is_unit t.exp_type -> []
        | args ->
          List.map (function
              | Asttypes.Nolabel, Some arg -> generate_simple_expression names arg
              | _ -> not_supported "labelled argument: %a" print_expr expr
            ) args
      in
      let name = prim.prim_name in
      generate_primitive name args
    | Texp_apply ({ exp_desc = Texp_ident (path, _, { val_kind = Val_reg; _ }); _ }, args) ->
      let args = List.map (function
          | (Asttypes.Nolabel, Some arg) -> generate_simple_expression names arg
          | _ -> not_supported "labelled argument: %a" print_expr expr
        ) args
      in
      let fname = Path.last path in
      Call (create_identifer fname, args)
    | Texp_apply _ ->
      not_supported "apply: %a" print_expr expr
    | Texp_ident (path, _lident, { val_kind = Val_reg; _ }) ->
      let id = Path.head path in
      let name, constness = Names.get names id in
      (match constness with
      | `Mut -> Addr_of (Variable name)
      | `Const -> Variable name)
    | Texp_ifthenelse (cond, then_, Some else_) ->
      Ternary (
        generate_simple_expression names cond,
        generate_simple_expression names then_,
        generate_simple_expression names else_)
    | Texp_ifthenelse (_cond, _then, None) ->
      not_supported "if-then is currently not allowed in expression: %a" print_expr expr
    | Texp_open (_, expr) ->
      generate_simple_expression names expr
    | Texp_ident (_path, _ident, _) ->
      failwith "TODO: ident with kind <> Val_reg"
    | Texp_let _ ->
      not_supported "let in expression: %a" print_expr expr
    | Texp_sequence _ ->
      (* could be supported using ',', maybe? *)
      not_supported "semicolon in expression: %a" print_expr expr
    | Texp_constant Const_int i -> Constant i
    | Texp_constant Const_int32 i -> Constant (Int32.to_int i)
    | Texp_constant Const_int64 i -> Constant_i64 i
    | Texp_constant Const_char c -> Constant_char c
    | Texp_constant Const_float f -> Constant_float (Float.of_string f) (* C doesn't support the all of OCaml's float literals *)
    | Texp_construct (_, { cstr_res; cstr_tag = Cstr_constant i; _ }, []) when is_bool cstr_res ->
      Constant_bool (i <> 0)
    | Texp_construct (_, { cstr_res; cstr_tag = Cstr_constant 0; _ }, []) when is_unit cstr_res ->
      Variable (create_identifer "Val_unit")
    | Texp_construct (_, _, _) ->
      not_supported "constructor: %a" print_expr expr
    | _ ->
      not_supported "in expression: %a" print_expr expr

  let rec generate_body ~return names body =
    match body.exp_desc with
    | Texp_let (Nonrecursive, [
        { vb_pat;
          vb_expr = {
            exp_desc = Texp_apply ({ exp_desc = Texp_ident (_path, _, { val_kind = Val_prim { prim_name = "lowcaml_mut_create"; _ }; _ }); _ }, [Nolabel, Some rhs]);
            _ };
          _ }], expr) ->
      let ident, var = get_var_from_pat vb_pat in
      let ty = vb_pat.pat_type in
      let names', varname = Names.new_var names ~mut:true ?ident var in
      let ty =
        match (OCaml_type.map ~where:var body.exp_env ty) with
        | Mut t -> ocaml_type_to_ctype ~where:var t
        | _ -> failwithf "bad type assigned to Mut.t: %a" Printtyp.type_expr ty
      in
      Declaration (
        ty,
        varname,
        generate_simple_expression names rhs
      )
      ::
      generate_body ~return names' expr
    | Texp_let (Nonrecursive, [binding], expr) ->
      let ident, var = get_var_from_pat binding.vb_pat in
      let ty = binding.vb_pat.pat_type in
      let names', varname = Names.new_var names ?ident var in
      Declaration (
        Const (ocaml_type_to_ctype ~where:var (OCaml_type.map ~where:var body.exp_env ty)),
        varname,
        generate_simple_expression names binding.vb_expr
      )
      ::
      generate_body ~return names' expr
    | Texp_sequence (e1, e2) ->
      generate_body ~return:false names e1 @ generate_body ~return names e2
    | Texp_ifthenelse (cond, then_, None) ->
      [If_then (
          generate_simple_expression names cond,
          generate_body ~return:false (Names.enter_scope names) then_)]
    | Texp_ifthenelse (cond, then_, Some else_) ->
      [If_then_else (
          generate_simple_expression names cond,
          generate_body ~return (Names.enter_scope names) then_,
          generate_body ~return (Names.enter_scope names) else_)]
    | Texp_while (cond, body) ->
      [While (
          generate_simple_expression names cond,
          generate_body ~return:false (Names.enter_scope names) body)]
    | Texp_for (ident, pat, expr_from, { exp_desc = Texp_constant Const_int upto; _ }, direction, body) ->
      let op_condition, op_next = match direction with Upto -> "<=", "+=" | Downto -> ">=", "-=" in
      let var = match pat.ppat_desc with Ppat_var v -> v.txt | Ppat_any -> "i" | _ -> not_supported "pattern in for-loop" in
      let names, var = Names.new_var names ~ident var in
      let decl = I64, var, generate_simple_expression names expr_from in
      let while_ = Op2 (op_condition, Variable var, Constant upto) in
      let after = Op2 (op_next, Variable var, Constant 1) in
      [For (decl, while_, after, generate_body ~return:false (Names.enter_scope names) body)]
    | Texp_for (ident, pat, expr_from, expr_to, direction, body) ->
      let op_condition, op_next = match direction with Upto -> "<=", "+=" | Downto -> ">=", "-=" in
      let var = match pat.ppat_desc with Ppat_var v -> v.txt | Ppat_any -> "i" | _ -> not_supported "pattern in for-loop" in
      let names, var = Names.new_var names ~ident var in
      let names, upto = Names.new_var names "upto" in
      let decl = I64, var, generate_simple_expression names expr_from in
      let while_ = Op2 (op_condition, Variable var, Variable upto) in
      let after = Op2 (op_next, Variable var, Constant 1) in
      [
        Declaration (Const I64, upto, generate_simple_expression names expr_to);
        For (decl, while_, after, generate_body ~return:false (Names.enter_scope names) body);
      ]
    | Texp_construct (_, _, _) when is_unit body.exp_type ->
      []
    | Texp_construct (_, _, _) ->
      [Return (generate_simple_expression names body)]
    | Texp_assert _ ->
      not_supported "TODO: assert"
    | Texp_apply _ | Texp_constant _ | Texp_ident _ when return ->
      [Return (generate_simple_expression names body)]
    | Texp_apply _ ->
      [Expression (generate_simple_expression names body)]
    | Texp_constant _ | Texp_ident _ ->
      print "Warning: meaningless value in function body: %a" print_expr body;
      [Expression (generate_simple_expression names body)]
    | Texp_match ({ exp_type; _ } as e1, [{ c_lhs = { pat_desc = Tpat_value p; _ }; c_guard = None; c_rhs = e2}], Total) when is_unit exp_type ->
      (* let () = ... in ... *)
      (match (p :> pattern) with
       | { pat_desc = Tpat_construct _; _ } ->
         generate_body ~return:false names e1 @ generate_body ~return names e2
       | _ ->
         not_supported "in function body: %a" print_expr body)
    | Texp_open (_, body) ->
      generate_body ~return names body
    | Texp_let (Recursive, _, _) ->
      not_supported "recursive let binding: %a" print_expr body
    | Texp_tuple _ ->
      not_supported "tuple: %a" print_expr body
    | _ ->
      not_supported "in function body: %a" print_expr body

  let go typed =
    let elements, ml_funcs = List.split @@ List.map (fun item ->
        match item.str_desc with
        | Tstr_value (Nonrecursive, [value]) ->
          let _ident, func_name = get_var_from_pat value.vb_pat in
          log "Got function: %s" func_name;
          let args, body = get_args value.vb_expr in
          List.iter (fun (id, name, ty) -> log "Arg: %a %s %a" Ident.print id name Printtyp.type_expr ty) args;
          let return_type = body.exp_type in
          log "return type: %a" Printtyp.type_expr return_type;
          if args = [] then not_supported "TODO: Constant";
          let args = List.map (fun (id, name, ty) -> id, name, OCaml_type.map ~where:name item.str_env ty) args in
          let return_type = OCaml_type.map ~where:func_name item.str_env return_type in
          let c_fun =
            let return_type = match return_type with Unit -> None | t -> Some (ocaml_type_to_ctype ~where:func_name t) in
            assert (match return_type with Some Const _ -> false | _ -> true);
            let names, args = List.fold_left (fun (names, args) (ident, var, ty) ->
                let cty = Const (ocaml_type_to_ctype ~where:var ty) in
                let names, var = Names.new_var names ~ident var in
                (names, (var, cty) :: args)
              ) (Names.empty, []) args
            in
            let args = List.rev args in
            Function {
              name = create_identifer func_name;
              args;
              return_type;
              body = generate_body ~return:(return_type <> None) names body;
            }
          in
          let return_ty = ocaml_type_to_external_type ~where:func_name return_type in
          let arg_types = String.concat " -> " (List.map (fun (_id, name, ty) -> ocaml_type_to_external_type ~where:name ty) args) in
          let external_decl =
            sprintf {|external %s : %s -> %s = "bytecode_not_supported_by_lowcaml" "%s" [@@noalloc]|} func_name arg_types return_ty func_name
          in
          let conversion_stub =
            if List.exists (fun (_, _, ty) -> ty = OCaml_type.Char || ty = Bool) args ||
               return_type = Char || return_type = Bool
            then
              let arg_names = String.concat " " (List.map (fun (_, name, _) -> name) args) in
              let arg_names_mapped = String.concat " " (List.map (fun (_, name, ty) ->
                  match ty with
                  | OCaml_type.Bool -> sprintf "(Bool.to_int %s)" name
                  | OCaml_type.Char -> sprintf "(Char.code %s)" name
                  | _ -> name
                ) args) in
              let call =
                match return_type with
                | Char -> sprintf "Char.chr (0xFF land %s %s)" func_name arg_names_mapped
                | Bool -> sprintf "0 <> %s %s" func_name arg_names_mapped
                | _ -> sprintf "%s %s" func_name arg_names_mapped
              in
              sprintf "\nlet %s %s = %s" func_name arg_names call
            else
              ""
          in
          c_fun, Some (external_decl ^ conversion_stub)
        | Tstr_primitive { val_id; val_name; val_desc; val_prim = func_name :: _; _ } ->
          log "external: %a %s %s" Ident.print val_id val_name.txt func_name;
          let rec get_args_from_ty ty =
            match Types.get_desc ty with
            | Tarrow (Nolabel, arg, ret, _commutable) ->
              let rest, ret = get_args_from_ty ret in
              arg :: rest, ret
            | Tarrow _ ->
              not_supported "labelled argument in external: %a" print_structure_item item
            | _ ->
              [], ty
          in
          let args, return_type = get_args_from_ty val_desc.ctyp_type in
          List.iter (fun arg -> log "external arg: %a" Printtyp.type_expr arg) args;
          log "external return_type: %a" Printtyp.type_expr return_type;
          let args = List.map (OCaml_type.map ~where:func_name item.str_env) args in
          let return_type = OCaml_type.map ~where:func_name item.str_env return_type in
          Prototype {
            name = create_identifer func_name;
            args =
              (match args with
               | [Unit] -> []
               | _ -> List.map (ocaml_type_to_ctype ~where:func_name) args);
            return_type = (match return_type with Unit -> None | t -> Some (ocaml_type_to_ctype ~where:func_name t));
          },
          None
        | Tstr_primitive { val_name; _ } ->
          not_supported "primitive with more than two names: %s" val_name.txt
        | Tstr_attribute { attr_name = { txt = "include" | "lowcaml.include"; _ };
                           attr_payload = PStr [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (header, _, None)); _ }, _); _ }]; _ } ->
          (match String.get header 0, String.get header (String.length header - 1) with
           | '<', '>' -> Include header
           | '<', _ | _, '>' | exception Invalid_argument _ -> not_supported "Invalid header: %S" header;
           | _ -> Include (sprintf "%S" header)),
          None
        | _ ->
          not_supported "%a" print_structure_item item
      ) typed.str_items
    in
    let builtin_includes = [
      "<assert.h>";
      "<immintrin.h>";
      "<stdbool.h>";
      "<stdint.h>";
      "<string.h>";
      "<caml/mlvalues.h>";
      "<caml/bigarray.h>";
    ]
    in
    let elements = List.map (fun f -> C_ast.Include f) builtin_includes @ elements in
    { elements }, String.concat "\n" (
      "(* generated by lowcaml *)\n"
      ::
      List.filter_map Fun.id ml_funcs
      @
      [""]
    )
end

module Util = struct
  let rand_dir () =
    let rec go i =
      let temp_dir = Filename.concat (Filename.get_temp_dir_name ()) (sprintf "lowcaml_stdlib_%d" (Random.bits ())) in
      try
        Sys.mkdir temp_dir 0o755;
        temp_dir
      with
        Sys_error e ->
        if i = 0 then failwithf "rand_dir: failed to create temporary directory: %s" e;
        go (i - 1)
    in
    go 1000

  let write_file filename out =
    match filename with
    | "-" -> print_string out
    | _ -> Out_channel.with_open_bin filename (fun c -> output_string c out)
end

let type_program ~cmi ~source_file program =
  let output_prefix = try Filename.chop_extension source_file with Invalid_argument _ -> "input" in
  let module_name = Compenv.module_of_filename source_file output_prefix in
  Clflags.strict_sequence := true;
  (* ignore @@ Warnings.parse_options false "-32-34-37-38-60"; *) (* TODO *)
  Clflags.nopervasives := true;
  Clflags.open_modules := ["Lowcaml_stdlib"];
  begin match cmi with
    | Some cmi -> Clflags.include_dirs := [Filename.dirname cmi];
    | None ->
      let temp_dir = Util.rand_dir () in
      at_exit (fun () -> Sys.rmdir temp_dir);
      let cmi = Filename.concat temp_dir "lowcaml_stdlib.cmi" in
      Out_channel.with_open_bin cmi (fun c -> output_string c Lowcaml_stdlib_cmi.data);
      at_exit (fun () -> Sys.remove cmi);
      Clflags.include_dirs := [temp_dir];
  end;
  Location.input_name := source_file;
  Compmisc.init_path ();
  Env.set_unit_name module_name;
  Compilenv.reset ?packname:None module_name;
  Typecore.reset_delayed_checks ();
  Env.reset_required_globals ();
  let initial_env = Compmisc.initial_env () in
  let parsed = Parse.implementation (Lexing.from_string program) in
  let typed, _signature, _names, _shape, _final_env = Typemod.type_structure initial_env parsed in
  typed

let () =
  Printexc.record_backtrace true;
  Random.self_init ();
  let i = ref "-" in
  let c_file = ref "-" in
  let ml_file = ref None in
  let cmi = ref None in
  let skip_simplify = ref false in
  let opt_arg res = Arg.String (fun arg -> res := Some arg) in
  let args = [
    "-i", Arg.Set_string i, "input .ml file (default stdin)";
    "-o-c", Arg.Set_string c_file, "output .c file (default stdout)";
    "-o-ml", opt_arg ml_file, "output .ml file";
    "-cmi", opt_arg cmi, "lowcaml stdlib cmi (default uses built-in)";
    "-skip-simplify", Arg.Set skip_simplify, "skip simplify pass (default false)";
    "-v", Arg.Set logging, "verbose logging";
  ]
  in
  let usage_message = "Usage: lowcaml.exe [-i file] [-o-ml file] [-o-c file]" in
  let usage _ = Arg.usage args usage_message; exit 1 in
  Arg.parse args usage usage_message;
  let input = if !i = "-"
    then In_channel.input_all stdin
    else In_channel.with_open_bin !i In_channel.input_all
  in
  try
    let typed = type_program ~cmi:!cmi ~source_file:!i input in
    let c, ml = Lowcaml.go typed in
    let c = if !skip_simplify then c else C_ast.Simplify.go c in
    let out = C_ast.Print.print c in
    Util.write_file !c_file out;
    Option.iter (fun f -> Util.write_file f ml) !ml_file
  with
    | Typecore.Error _ | Env.Error _ | Syntaxerr.Error _ as e ->
      print "%a" Location.report_exception e;
      exit 1
    | Not_supported msg ->
      print "--- not supported ---@\n%s" msg;
      exit 1
