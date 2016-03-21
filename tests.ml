#use "markright.ml"

let test_flatten_replace = ("goodbye hello" = flatten (replace (fun e -> match e with x -> if x = "hello" then "goodbye" else x )[Variable("hello"); Text(" "); Text("hello")] ))

let test_filter_parse = ([Text "Hello \\{{world "; Variable "goodbye"; Text "!"] = filter_empty_text (parse "Hello \\{{world {{ goodbye }}!"))

let test_basic = "My girlfriend is kasey and my parents are rolf and andrea." = (markright "test.json" "My girlfriend is {{girlfriend}} and my parents are {{parents.dad}} and {{parents.mom}}.")

