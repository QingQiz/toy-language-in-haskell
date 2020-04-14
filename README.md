# compiler using c0 grammar in haskell

## Introduction

### C0 grammar

```lex
<add_op>     ::= + | -
<mul_op>     ::= * | /
<cmp_op>     ::= > | < | >= | <= | != | ==
<bool_op>    ::= || | &&

<alpha>      ::= [_a-zA-Z]
<number>     ::= 0 | <number_n0>
<number_n0>  ::= [1-9]
<str>        ::= "[32,33,35-126]"

<program>    ::= <const_stmt> | <var_stmt> | <func_def>
<const_stmt> ::= const <const_def> ;
<const_def>  ::= int <id> = <integer> {, <id> = <integer>}
               | char <id> = <char> {, <id> = <char>}
<id>         ::= <alpha> {<alpha> | <number>}
<declare_h>  ::= int <id> | char <id>
<var_stmt>   ::= <v_declare>; { <v_declare>;  }
<var_def>    ::= <type_id> (<id>\[<u_integer>\]}) {,(<id>\[<u_integer>\]})}
<type_id>    ::= int | char
<func>       ::= <declare_h> \(<param_list>\) \{ <stmt_sq> \}
<void_func>  ::= void <id> \(<param_list>\) \{ <stmt_sq \}
<stmt_sq>    ::= [<const_stmt>] [<var_stmt>] <stmt_list>
<param_list> ::= <type_id> <id> {, <type_id> <id>} | <empty>

<stmt>       ::= <if_stmt> | <loop_stmt> | \{ <stmt_list> \} | <func_call> ; | <v_func_call> ; | <assig_stmt> ;
               | <read_stmt> ; | <write_stmt> ; | <empty> ; | <ret_stmt> ;
<assig_stmt> ::= <id> = <expr> | id \[<expr>\] = <expr>
<if_stmt>    ::= if \( <cond> \) <stmt> [else <stmt>]
<cond>       ::= <bool_expr>
<loop_stmt>  ::= do <stmt> while \( <cond \)
               | for \( <id = <expr>; <cond> ; <assig_stmt> \) <stmt>
<func_call>  ::= <id> \( <param_val> \)
<v_func_call>::= <id> \( <param_val> \)
<param_val>  ::= <expr> {, <expr> } | <empty>
<stmt_list>  ::= <stmt>
<read_stmt>  ::= scanf\( <id> {, <id> } \)
<write_stmt> ::= printf\(<str>, <expr>\) | printf\(<str>\) | printf\(<expr>\)
<ret_stmt>   ::= return [\(<expr>\)]

<u_integer>  ::= <number_n0> { <number> } | 0
<integer>    ::= [ + | - ] <u_integer>

<expr>       ::= <bool_expr>
<bool_expr>  ::= <cmp_expr> { <bool_op> <cmp_expr> }
<cmp_expr>   ::= <arith_expr> <cmp_op> <arith_expr>
<arith_expr> ::= [ + | - ] <factor> {<add_op> <factor>}
<factor>     ::= <term> {<mul_op> <term>}
<term>       ::= <id> | <idnetifier> \[ <expr> \] | <integer> | <char> | <func_call> | \(<expr>\)
```

## Build & Run

> not availalbe


## TODO LIST

- [x] Lexical
- [ ] grammar
  - [x] bool expr (|| &&)
  - [x] cmp expr (> < >= <= == !=)
  - [x] expr (+ - * /)
- [ ] code generation

