#use "markright.ml"

let test_flatten_replace = ("goodbye hello" = flatten (replace [Variable("hello"); Text(" "); Text("hello")] (fun e -> match e with x -> if x = "hello" then "goodbye" else x)))

let test_filter_parse = ([Text "Hello \\{{world "; Variable "goodbye"; Text "!"] = filter_empty_text (parse "Hello \\{{world {{ goodbye }}!"))
