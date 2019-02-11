Ceng444 - Assignment 2
1902824
Ahmet Sadık Gülden

<b>Lexer Part</b>

Lexer is written in swift programming language. Prebuilt lexer file can be found in directory. It is named  `lexer`.
To lexify tokens in given xpln source code you can run below command.

`lexer sourceFile > tokensFile`

MacOS:
Code can be inspected, new lexer can be built in MacOS using Assignment2Lexer/Assignment2Lexer.xcodeprof file.

Linux:
You can download and install swift from here: <link>https://swift.org/getting-started/#installing-swift</link>

Navigate to Lexer/ folder
Run below script. this will create a new lexer for given source code in main.swift

`swiftc main.swift -o lexer`

<b>Parser and code generation</b>
<ol>
<li> yalalr
<li> `(load "Assignment2")`
<li> `(make-lalrparser)`
<li> `(target-code-mips tokensFile t)`
</ol>
<b>Some notes</b>
<ol>
<li> Leaf/nonleaf procedures are detected and local variables and formal arguments are not stored.
<li> If a function does not have return value 0.0 is returned.
<li> Argument values can be changed inside function.
<li> Functions can have multiple arguments.
<li> Variables defined in if / else / while blocks are considered member of enclosing scope of those statements (main / functions).
</ol>
