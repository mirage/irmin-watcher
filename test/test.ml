let basic_polling () = ()

let polling_tests = [
  "basic", `Quick, basic_polling;
]

let tests = [
  "polling", polling_tests;
]

let () = Alcotest.run "irmin-watch" tests
