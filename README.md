# compiler using c0 grammar in haskell

## Introduction

### C0 grammar

```
>>>> [_Not Yet] <<<<<
>>>> Implemented  Grammar <<<<<
<id>         ::= <alpha> {<alpha> | <number>}
<str>        ::= "[32,33,35-126]"

<const_desc> ::= const <const_def> ; { const <const_def> ; }
<const_def>  ::= int <id> = <integer> {, <id> = <integer>}
               | char <id> = <char> {, <id> = <char>}
<var_desc>   ::= <var_def>; { <var_def>;  }
<var_def>    ::= <type_id> (<id> | <id> \[<u_integer>\]}) {,(<id> | <id> \[<u_integer>\]})}

<program>    ::= [<const_desc>] [<var_desc>] {<func_def>}
<declare_h>  ::= int <id> | char <id>
<func>       ::= <declare_h> \(<param_list>\) \{ <comd_stmt> \}
<void_func>  ::= void <id> \(<param_list>\) \{ <comd_stmt \}
<func_call>  ::= <id> \( <param_val> \)
<v_func_call>::= <id> \( <param_val> \)
<param_val>  ::= <expr> {, <expr> } | <empty>

<stmt>       ::= <if_stmt> | <loop_stmt> | \{ <stmt_list> \} | <func_call> ; | <v_func_call> ; | <assig_stmt> ;
               | <read_stmt> ; | <write_stmt> ; | <empty> ; | <ret_stmt> ;
<ret_stmt>   ::= return [\(<expr>\)]
<comd_stmt>  ::= [<const_desc>] [<var_desc>] <stmt_list>
<param_list> ::= <type_id> <id> {, <type_id> <id>} | <empty>

<assig_stmt> ::= <id> = <expr> | id \[<expr>\] = <expr>
<stmt_list>  ::= {<stmt>}
<if_stmt>    ::= if \( <cond> \) <stmt> [else <stmt>]
<loop_stmt>  ::= do <stmt> while \( <cond \)
               | for \( <assig_stmt> ; <cond> ; <assig_stmt> \) <stmt>
<read_stmt>  ::= scanf\( <id> {, <id> } \)
<write_stmt> ::= printf\(<str>, <expr>\) | printf\(<str>\) | printf\(<expr>\)


<cond>       ::= <bool_expr>
<expr>       ::= <bool_expr>
<bool_expr>  ::= <cmp_expr> { <bool_op> <cmp_expr> }
<cmp_expr>   ::= <arith_expr> <cmp_op> <arith_expr>
<arith_expr> ::= [ + | - ] <factor> {<add_op> <factor>}
<factor>     ::= <unary_expr> {<mul_op> <unary_expr>}
<unary_expr> ::= {!} <term>
<term>       ::= <id> | <idnetifier> \[ <expr> \] | <integer> | <char> | <func_call> | \(<expr>\)

>>>> In Lexical <<<<<
<type_id>    ::= int | char
<add_op>     ::= + | -
<mul_op>     ::= * | /
<cmp_op>     ::= > | < | >= | <= | != | ==
<bool_op>    ::= || | &&

<alpha>      ::= [_a-zA-Z]
<number>     ::= 0 | <number_n0>
<number_n0>  ::= [1-9]

<u_integer>  ::= <number_n0> { <number> } | 0
<integer>    ::= [ + | - ] <u_integer>
```

## Build & Run

### Build

```shell
make
```

### Run

> currently we can generate x86-64 asm

```
./cpr path-to-file
```

## TODO LIST

- [x] Lexical
- [x] Grammar
  - [x] expr
    - [x] bool expr (|| && !)
    - [x] cmp expr (> < >= <= == !=)
    - [x] expr (+ - * /)
    - [x] id, array
    - [x] char
    - [x] func call
  - [x] stmt
    - [x] if stmt
    - [x] loop stmt
    - [x] comd stmt
    - [x] stmt list
    - [x] assig stmt
    - [x] empty stmt
    - [x] func call
    - [x] read stmt
    - [x] write stmt
    - [x] ret stmt
  - [x] program
    - [x] const desc
    - [x] var desc
    - [x] func def
  - [x] AST
- [x] Semantic Analysis
  - [x] const desc
  - [x] var desc
  - [x] func def
  - [x] comd stmt
  - [x] stmt list
  - [x] for
  - [x] do
  - [x] read
  - [x] write
  - [x] return
  - [x] func call
  - [x] assign
  - [x] expr
    - [x] bin node
    - [x] unary node
    - [x] leaf node
- [x] code generation
  - [x] global variables
  - [x] function definations
  - [x] local variables
  - [x] stmt list
  - [x] stmt
    - [x] stmt-list
    - [x] empty-stmt
    - [x] if-stmt
    - [x] do-stmt
    - [x] for-stmt
    - [x] assign-stmt
    - [x] func-call
    - [x] read-stmt
    - [x] write
    - [x] return
  - [x] expr
    - [x] bin-node
    - [x] unary-node
    - [x] number
    - [x] array
    - [x] func-call
    - [x] id
    - [x] char (char is reduced in semantic analysis)
- [ ] extra
  - [ ] error reporter
    - [x] Some crude error messages in semantic analysis
    - [ ] ...
  - [ ] optimization
    - [x] reduce const expr
    - [ ] ...
  - [ ] ...

