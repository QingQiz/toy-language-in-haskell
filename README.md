# compiler using c0 grammar in haskell

## Introduction

### C0 grammar

```
>>>> [_] <<<<<
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
               | <read_stmt> ; | <write_stmt> ; | <empty> ; | <ret_stmt> ; | break ; | continue ;
<ret_stmt>   ::= return [<expr>]
<comd_stmt>  ::= [<const_desc>] [<var_desc>] <stmt_list>
<param_list> ::= <type_id> <id> {, <type_id> <id>} | <empty>

<assig_stmt> ::= <id> = <expr> | id \[<expr>\] = <expr>
<stmt_list>  ::= {<stmt>}
<if_stmt>    ::= if \( <cond> \) <stmt> [else <stmt>]
<loop_stmt>  ::= do <stmt> while \( <cond \)
               | for \( [<assig_stmt>] ; [<cond>] ; [<assig_stmt>] \) <stmt>
<read_stmt>  ::= scanf\( <id> {, <id> } \)
<write_stmt> ::= printf\(<str>, <expr> {, <expr>}\)

<cond>       ::= <bool_expr>
<expr>       ::= <bool_expr>
<bool_expr>  ::= <cmp_expr> { <bool_op> <cmp_expr> }
<cmp_expr>   ::= <arith_expr> <cmp_op> <arith_expr>
<arith_expr> ::= [ + | - ] <factor> {<add_op> <factor>}
<factor>     ::= <unary_expr> {<mul_op> <unary_expr>}
<unary_expr> ::= {!} <term>
<term>       ::= <id> | <idnetifier> \[ <expr> \] | <integer> | <char> | <func_call> | \(<expr>\)

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
./cpr x.c0 > x.s
as --64 x.s -o x.o
ld -dynamic-linker /lib64/ld-linux-x86-64.so.2 /usr/lib/x86_64-linux-gnu/crt1.o /usr/lib/x86_64-linux-gnu/crti.o -lc x.o /usr/lib/x86_64-linux-gnu/crtn.o -o x.out
```

## TODO LIST

- [ ] optimization
  - [x] reduce const expr
  - [ ] ...

