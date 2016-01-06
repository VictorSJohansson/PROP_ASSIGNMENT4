/***
A skeleton for Assignment 3 on PROP HT2015 at DSV/SU.
Peter Idestam-Almquist, 2015-12-09.
***/

:- [tokenizer].

/***
Example call of the top level predicate run/2:
?- run('program1.txt','myparsetree1.txt').
***/

run(InputFile,OutputFile):-
	tokenize(InputFile,Program),
	parse(ParseTree,Program,[]),
	evaluate(ParseTree,[],VariablesOut), 
	output_result(OutputFile,ParseTree,VariablesOut).

output_result(OutputFile,ParseTree,Variables):- 
	open(OutputFile,write,OutputStream),
	write(OutputStream,'PARSE TREE:'), 
	nl(OutputStream), 
	writeln_term(OutputStream,0,ParseTree),
	nl(OutputStream), 
	write(OutputStream,'EVALUATION:'), 
	nl(OutputStream), 
	write_list(OutputStream,Variables), 
	close(OutputStream).
	
writeln_term(Stream,Tabs,int(X)):-
	write_tabs(Stream,Tabs), 
	writeln(Stream,int(X)).
writeln_term(Stream,Tabs,ident(X)):-
	write_tabs(Stream,Tabs), 
	writeln(Stream,ident(X)).
writeln_term(Stream,Tabs,Term):-
	functor(Term,_Functor,0), !,
	write_tabs(Stream,Tabs),
	writeln(Stream,Term).
writeln_term(Stream,Tabs1,Term):-
	functor(Term,Functor,Arity),
	write_tabs(Stream,Tabs1),
	writeln(Stream,Functor),
	Tabs2 is Tabs1 + 1,
	writeln_args(Stream,Tabs2,Term,1,Arity).
	
writeln_args(Stream,Tabs,Term,N,N):-
	arg(N,Term,Arg),
	writeln_term(Stream,Tabs,Arg).
writeln_args(Stream,Tabs,Term,N1,M):-
	arg(N1,Term,Arg),
	writeln_term(Stream,Tabs,Arg), 
	N2 is N1 + 1,
	writeln_args(Stream,Tabs,Term,N2,M).
	
write_tabs(_,0).
write_tabs(Stream,Num1):-
	write(Stream,'\t'),
	Num2 is Num1 - 1,
	write_tabs(Stream,Num2).

writeln(Stream,Term):-
	write(Stream,Term), 
	nl(Stream).
	
write_list(_Stream,[]). 
write_list(Stream,[Ident = Value|Vars]):-
	write(Stream,Ident),
	write(Stream,' = '),
	format(Stream,'~1f',Value), 
	nl(Stream), 
	write_list(Stream,Vars).
	
/***
parse(-ParseTree)-->
	TODO: Implement a definite clause grammar defining our programming language,
	and returning a parse tree.


parse([])-->[].***/
parse(ParseTree, [X], [])--> assign([X|X2],[X2|X3], [X3]), assign(ParseTree).
assign(assignment(ident(X), '=', Exp))--> identifier(X), [=], expr(Exp), [';'].
expr(expression(Term))--> term(Term).
expr(expression(Term, Operator, Expr))-->term(Term), operator(Operator), expr(Expr).
term(term([X, Y | W]))-->factor(X), operator(Y), term(W).
term(term(Factor))-->factor(Factor).
factor(factor(X))-->int(X).
factor(factor(X))-->id(X).
factor(factor(Expr))-->['('], expr(Expr), [')'].

removehead([_|Tail], Tail).

operator(plus)-->['+'].
operator(minus)-->['-'].
operator(divide)-->['/'].
operator(multiply)-->['*'].
leftparen(paren)-->['('].
rightparen(paren)-->[')'].

identifier(X)--> [X], {atom(X)}.
int(X)--> [X], {integer(X)}.


/*** evaluator ***/
evaluate([])-->[].
evaluate([X|Xs], [], VariablesOut):-evaluate([X|Xs], Y), evaluate(Xs, Z), VariablesOut is Y+Z.
evaluate(X, X):- number(X).
evaluate(X + Y, Z):- evaluate(X, Xs), evaluate(Y, Ys), Z is Xs + Ys.
evaluate(X - Y, Z):- evaluate(X, Xs), evaluate(Y, Ys), Z is Xs - Ys.
evaluate(X * Y, Z):- evaluate(X, Xs), evaluate(Y, Ys), Z is Xs * Ys.
evaluate(X / Y, Z):- evaluate(X, Xs), evaluate(Y, Ys), Z is Xs / Ys.

/*** 
block = ‘{’ , stmts , ‘}’ ;
stmts = [ assign , stmts ] ;
assign = id , ‘=’ , expr , ‘;’ ;
expr = term , [ ( ‘+’ | ‘−’ ) , expr ] ;
term = factor , [ ( ‘*’ | ‘/’ ) , term ] ;
factor = int | id | ‘(’ , expr , ‘)’ ;
where id is defined as (a..z)+ and int is defined as
 ***/
	
/***
evaluate(+ParseTree,+VariablesIn,-VariablesOut):-
	TODO: Implement an evaluate predicate that evaluates a parse-tree and 
	returns the state of the program after evaluation as a list of variables and 
	their values.

evaluator i slutet tar in [a = 3, b = 4]


[_, var=val]
_ = inte första elementet i listan


lecture 13, example 3, page 21
non-tail recrsice

sum([],0)


package control
sublimerpl
sicstus
***/