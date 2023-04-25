let () =
  Printexc.record_backtrace true;

  assert (Lstubs.inc 42 = 43);
  assert (Lstubs.inc_int64 42L = 43L);

  let b = Bytes.make 2 '\x00' in
  Lstubs.test_bytes1 b 0; assert (Bytes.get_uint16_le b 0 = 42);
  let b = Bytes.make 2 '\x00' in
  Lstubs.test_if 0 b; assert (Bytes.get_uint16_le b 0 = 0);
  let b = Bytes.make 2 '\x00' in
  Lstubs.test_if 1 b; assert (Bytes.get_uint16_le b 0 = 42);

  let b = Bytes.make 2 '\x00' in
  Lstubs.test_if_else1 0 b; assert (Bytes.get_uint16_le b 0 = 43);
  let b = Bytes.make 2 '\x00' in
  Lstubs.test_if_else1 1 b; assert (Bytes.get_uint16_le b 0 = 42);

  let b = Bytes.make 2 '\x00' in
  Lstubs.test_if_else2 0 b; assert (Bytes.get_uint16_le b 0 = 43);
  let b = Bytes.make 2 '\x00' in
  Lstubs.test_if_else2 1 b; assert (Bytes.get_uint16_le b 0 = 44);
  let b = Bytes.make 2 '\x00' in
  Lstubs.test_if_else2 42 b; assert (Bytes.get_uint16_le b 0 = 0);

  let b = Bytes.make 2 '\x00' in
  Lstubs.test_if_else3 0 b; assert (Bytes.get_uint16_le b 0 = 43);
  let b = Bytes.make 2 '\x00' in
  Lstubs.test_if_else3 1 b; assert (Bytes.get_uint16_le b 0 = 44);
  let b = Bytes.make 2 '\x00' in
  Lstubs.test_if_else3 42 b; assert (Bytes.get_uint16_le b 0 = 45);

  assert (4 = Lstubs.test_if_else4 0 1);
  assert (6 = Lstubs.test_if_else4 3 1);

  assert (Lstubs.test_call 1 2 3 = 34);
  (* Lstubs.test_unit () () (); *)

  let b = Bytes.make 22 '\x00' in
  Lstubs.test_for1 b;
  for i = 0 to 10 do
    assert (Bytes.get_uint16_le b (2 * i) = i)
  done;

  let b = Bytes.make 3 '\x00' in
  Lstubs.test_for3 b;
  assert (Bytes.get_uint8 b 0 = 42);
  assert (Bytes.get_uint8 b 1 = 0);
  assert (Bytes.get_uint8 b 2 = 0);

  let b = Bytes.make 2 '\x00' in
  Lstubs.test_for2 b;
  assert (Bytes.get_uint16_le b 0 = 1);
  let b = Bytes.make 2 '\x00' in
  Lstubs.test_for4 b;
  assert (Bytes.get_uint16_le b 0 = 1);

  assert (Lstubs.test_bool1 0 = 54);
  assert (Lstubs.test_bool1 1 = 42);
  assert (Lstubs.test_bool_param false = 54);
  assert (Lstubs.test_bool_param true = 42);
  assert (Lstubs.test_bool_ret 42 = false);
  assert (Lstubs.test_bool_ret 43 = true);

  assert (Lstubs.test_bool_id true = true);
  assert (Lstubs.test_char_id 'a' = 'a');
  assert (Lstubs.test_int_id 37 = 37);
  assert (Lstubs.test_int32_id 37l = 37l);
  assert (Lstubs.test_int64_id 37L = 37L);

  let x = 43.0 and y = 12.5 in
  assert (Lstubs.test_float1 x y = x +. y *. 3.0 -. 123. /. 7.0);

  assert (Lstubs.test_literal_int_1 () = 4611686018427387903);
  assert (Lstubs.test_literal_int_2 () = -4611686018427387904);
  assert (Lstubs.test_literal_int_3 () = 4611686018427387903 + -4611686018427387904);
  assert (Lstubs.test_literal_i64_1 () = 9223372036854775807L);
  assert (Lstubs.test_literal_i64_2 () = -9223372036854775808L);
  assert (Lstubs.test_literal_i64_3 () = -9223372036854775807L);
  assert (Lstubs.test_literal_i64_4 () = Int64.add 9223372036854775807L (-9223372036854775808L));
  assert (Lstubs.test_literal_i32_1 () = 2147483647l);
  assert (Lstubs.test_literal_i32_2 () = -2147483648l);
  assert (Lstubs.test_literal_i32_3 () = Int32.add 2147483647l (-2147483648l));

  assert (Lstubs.test_literal_bool1 () = false);
  assert (Lstubs.test_literal_bool2 () = true);

  assert (Lstubs.test_literal_char1 () = 'b');
  assert (Lstubs.test_literal_char2 () = '\\');
  assert (Lstubs.test_literal_char3 () = '\x00');
  assert (Lstubs.test_literal_char4 () = '\xff');

  let b = Bytes.make 50 '\x00' in
  Lstubs.test_sieve b;
  assert (Bytes.get_uint8 b 41 = 0);
  assert (Bytes.get_uint8 b 42 = 1);
  assert (Bytes.get_uint8 b 43 = 0);
  assert (Bytes.get_uint8 b 45 = 1);
  assert (Bytes.get_uint8 b 46 = 1);
  assert (Bytes.get_uint8 b 47 = 0);
  assert (Bytes.get_uint8 b 48 = 1);

  let b = Bytes.init 2 Char.chr in
  Lstubs.test_external b;
  assert (Bytes.get_uint8 b 0 = 1);

  let b = Bytes.make 128 '\x00' in
  Lstubs.test_simd_fill b 0x11223344l;
  for i = 0 to 128 / 4 - 1 do
    assert (Bytes.get_int32_le b (4 * i) = 0x11223344l);
  done;
  Lstubs.test_simd2 b 42l;
  for i = 0 to 7 do assert (Bytes.get_int32_le b (4 * i) = 42l) done;

  Gc.full_major ();
