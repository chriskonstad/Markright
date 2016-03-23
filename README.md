# Markright
This is a Markdown preprocessor that can replace variables in the Markdown with
specificed values.
The values are specified in a JSON file that is loaded by the program.

# Why are you doing this?
I wanted to practice my Ocaml and what better way than write and ship a program.
Plus I've wanted this feature with Markdown for a while.
I also wanted to avoid just using regex for everything because I wanted to try
my hand at parsing strings with Ocaml.

# Precedence rules
* Inline mappings hide imported mappings
* The last mappings added hide mappings from before then, even if the mappings
come after their use.  Mappings at the end of a file can override mappings at
the beginning of a file.
