# List-Prolog-to-Prolog-Converter

Converts List Prolog code to Prolog code.

List Prolog (LP) Interpreter (available <a href="https://github.com/luciangreen/listprologinterpreter">here</a>) is an interpreter for a different version of Prolog that is in list format, making it easier to generate List Prolog programs. The LP interpreter is an algorithm that parses and runs List Prolog code. The converter helps convert List Prolog programs to Prolog programs.  The interpreter and converter are written in SWI-Prolog.


# Prerequisites

Install List Prolog Interpreter Repository (https://github.com/luciangreen/listprologinterpreter).

Install Text to Breasonings Repository (https://github.com/luciangreen/Text-to-Breasonings).


# Installation from List Prolog Package Manager (LPPM)

* Optionally, you can install from LPPM by installing <a href="https://www.swi-prolog.org/build/">SWI-Prolog</a> for your machine, downloading the <a href="https://github.com/luciangreen/List-Prolog-Package-Manager">LPPM Repository</a>,
```
git clone https://github.com/luciangreen/List-Prolog-Package-Manager.git
cd List-Prolog-Package-Manager
```
loading LPPM with `['lppm'].` then installing the package by running `lppm_install("luciangreen","List-Prolog-to-Prolog-Converter").`.

# Installing

* Please download and install <a href="https://www.swi-prolog.org/build/">SWI-Prolog</a> for your machine.

* Load List Prolog by downloading the <a href="https://github.com/luciangreen/listprologinterpreter">repository from GitHub</a>.

* Download this repository to your machine.
In the SWI-Prolog environment, enter:
`['../listprologinterpreter/listprolog'].`    

* Load the List Prolog to Prolog Converter by typing:
`['lp2pconverter'].`

* The converter is called in the form:
`test(Number,_,Algorithm1,_),lp2p1(Algorithm1,Algorithm2),write(Algorithm2).`

Where:
Number - Test number of algorithm to convert (taken from <a href="https://github.com/luciangreen/listprologinterpreter/blob/master/lpiverify4.pl">"lpiverify4.pl"</a>).
Algorithm1 - is the List Prolog algorithm to convert.
Algorithm2 - is the Prolog algorithm produced.

* For example:
```
test(1,_,A,_),lp2p1(A,B),write(B).
function(A,B,C):-+(A,B,C).
```

```
test(2,_,A,_),lp2p1(A,B),write(B).
function(A,B,C):-+(A,B,D),+(D,1,C).
```

```
test(3,_,A,_),lp2p1(A,B),write(B).
function(A,B,C):-function2(D,F),+(A,B,E),+(E,F,G),+(G,D,C).
function2(A,F):-is(A,2),is(F,1).
```

```
test(4,_,A,_),lp2p1(A,B),write(B).
append1(A):-b(B),c(C),append(B,C,A).
b(["b"]).
c(["c"]).
```

```
test(15,_,A,_),lp2p1(A,B),write(B).
grammar1(U,T):-compound(U,"",[],T).
compound213("","",T,T).
compound213(U,U,T,T).
compound(T,U)->"[",compound21(T,V),"]",compound213(V,U).
compound212("","",T,T).
compound212(U,U,T,T).
compound21(T,U)->item(I),{wrap(I,Itemname1),append(T,Itemname1,V)},compound212(V,U).
compound21(T,U)->item(I),",",compound21([],Compound1name),{wrap(I,Itemname1),append(T,Itemname1,V),append(V,Compound1name,U)}.
item(T)->number21("",U),{stringtonumber(U,T)}.
item(T)->word21("",T).
item(T)->compound([],T).
number212("","",T,T).
number212(U,U,T,T).
number21(T,U)->A,commaorrightbracketnext,{stringtonumber(A,A1),number(A1),stringconcat(T,A,V)},number212(V,U).
number21(T,U)->A,{stringtonumber(A,A1),number(A1),stringconcat(T,A,V)},number21("",Numberstring),{stringconcat(V,Numberstring,U)}.
word212("","",T,T).
word212(U,U,T,T).
word21(T,U)->A,commaorrightbracketnext,{letters(A),stringconcat(T,A,V)},word212(V,U).
word21(T,U)->A,{letters(A),stringconcat(T,A,V)},word21("",Wordstring),{stringconcat(V,Wordstring,U)}.
commaorrightbracketnext->lookahead(",").
commaorrightbracketnext->lookahead("]").
lookahead(A,A,B):-stringconcat(B,D,A).
```

# Authors

Lucian Green - Initial programmer - <a href="https://www.lucianacademy.com/">Lucian Academy</a>

# License

I licensed this project under the BSD3 License - see the LICENSE.md file for details
