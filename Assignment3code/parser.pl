/*** 
block = ‘{’ , stmts , ‘}’ ;
stmts = [ assign , stmts ] ;
assign = id , ‘=’ , expr , ‘;’ ;
expr = term , [ ( ‘+’ | ‘−’ ) , expr ] ;
term = factor , [ ( ‘*’ | ‘/’ ) , term ] ;
factor = int | id | ‘(’ , expr , ‘)’ ;
where id is defined as (a..z)+ and int is defined as
 ***/

block(X):-left_curly(X), stmts(X), right_curly(X).

stms(X):-assign(X), stmts(X).
stmts(X):-NULL?!

assign(X):-id(X), equals(X), expr(X), semicol(X).

expr(X):-term(X), plus(X), expr(X).
expr(X):-term(X), minus(X), expr(X).
expr(X):-term(X).

term(X):-factor(X), divide(X), term(X).
term(X):-factor(X), multiply(X), term(X). 
term(X):-factor(X).

factor(X):-int(X).
factor(X):-id(X).
factor(X):-left_paren(X), expr(X), right_paren(X).

int(X):-digit_code(X).
id(X):-letter_code(X).

plus(X):-symbol_code(X, 43).
minus(X):-symbol_code(X, 45).

divide(X):-symbol_code(X, 47).
multiply(X):-symbol_code(X, 42).

left_paren(X):-symbol_code(X, 40).
right_paren(X):-symbol_code(X, 41).

equals(X):-symbol_code(X, 61).
semicol(X):-symbol_code(X, 59).

left_curly(X):-symbol_code(X, 123).
right_curly(X):-symbol_code(X, 125).

