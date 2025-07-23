(* A simple test case *)
let test_addition () =
  Alcotest.(check int) "same value" 2 (1 + 1)

(* A list of all tests to run *)
let all_tests = [
  "Addition", `Quick, test_addition;
]

(* Run the tests *)
let () = Alcotest.run "Maze tests" [ "suite", all_tests ]
