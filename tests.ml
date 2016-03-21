#use "markright.ml"


let test_flatten_replace = ("goodbye hello" = flatten (replace [Variable("hello"); Text(" "); Text("hello")] (fun e -> match e with x -> if x = "hello" then "goodbye" else x)))
