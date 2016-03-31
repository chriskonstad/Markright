open Markright
open OUnit2

(* Test basic functionality *)
let test_basic test_ctxt = assert_equal
    "I like OCaml!"
    (markright
       false
       "{{ {\"mappings\": { \"lang\": \"OCaml\"} } }}I like {{lang}}!"
       "./test")

let test_basic_escaped test_ctxt = assert_equal
    "I like {{lang}}!"
    (markright
       false
       "{{ {\"mappings\": { \"lang\": \"OCaml\"} } }}I like \{{lang}}!"
       "./test")

let test_basic_nested text_ctxt = assert_equal
    (load_file "test/test1.md")
    (markright false (load_file "test/test1.mr") "./test/test1.mr")

(* Test imports *)
let test_import_nested text_ctxt = assert_equal
    (load_file "test/test2.md")
    (markright false (load_file "test/test2.mr") "./test/test2.mr")

(* Test error on multiple defs *)
let test_mult_error text_ctxt = assert_raises
    (Mapper.Multiple_def("fruit"))
    (fun () -> (markright false (load_file "test/test3.mr") "./test/test3.mr"))

(* Test ignoring error on multiple defs *)
let test_mult_ignore text_ctxt = assert_equal
    (load_file "test/test3.md")
    (markright true (load_file "test/test3.mr") "./test/test3.mr")

let suite =
  "suite">:::
  [
    "test_basic">:: test_basic;
    "test_basic_escaped">:: test_basic_escaped;
    "test_basic_nested">:: test_basic_nested;
    "test_import_nested">:: test_import_nested;
    "test_mult_error">:: test_mult_error;
    "test_mult_ignore">:: test_mult_ignore;
  ]

let () =
  run_test_tt_main suite
