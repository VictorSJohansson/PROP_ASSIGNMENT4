block = ‘{’ , stmts , ‘}’ ;
stmts = [ assign , stmts ] ;
assign = id , ‘=’ , expr , ‘;’ ;
expr = term , [ ( ‘+’ | ‘−’ ) , expr ] ;
term = factor , [ ( ‘*’ | ‘/’ ) , term ] ;
factor = int | id | ‘(’ , expr , ‘)’ ;
where id is defined as (a..z)+ and int is defined as 