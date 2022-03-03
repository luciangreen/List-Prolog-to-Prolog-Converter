%% lp2p

lp2p1(Algorithm1,Algorithm2) :-
	%% note: without type, mode statements
	memberlp2p10(Algorithm1,"",Algorithm2).
		%%string_concat(Algorithm3,"]",Algorithm2).

memberlp2p10([],Algorithm1,Algorithm1) :- !.
memberlp2p10(Functions2,Algorithm1,Algorithm2) :-
	Functions2=[Functions3|Functions4],
	memberlp2p1(Functions3,Algorithm1,Algorithm3),
	memberlp2p10(Functions4,Algorithm3,Algorithm2).

memberlp2p1(Functions2,Algorithm1,Algorithm2) :-
        Functions2=[Function,Arguments2,Symbol,Body],
        interpretstatementlp2p2a(Function,Algorithm1,Algorithm3a),
        	string_concat(Algorithm3a,"(",Algorithm3d),
        interpretstatementlp2p2(Arguments2,Algorithm3d,Algorithm3e),
        	string_concat(Algorithm3e,")",Algorithm3f),
                	concat_list([Algorithm3f,Symbol],Algorithm3),
	interpretbodylp2p(Body,Algorithm3,Algorithm2a),
	      write_full_stop_if_last_item([],Algorithm2a,Algorithm2),
!.
	%%;
%%		memberlp2p11(Functions2,Algorithm1,Algorithm2)).
		
memberlp2p1(Functions2,Algorithm1,Algorithm2) :-
        Functions2=[Function,Symbol,Body],
        interpretstatementlp2p2a(Function,Algorithm1,Algorithm3b),
                	concat_list([Algorithm3b,Symbol],Algorithm3a),
		%%string_concat(Algorithm3a,"(",Algorithm3d),
        interpretbodylp2p(Body,Algorithm3a,Algorithm2a),
        write_full_stop_if_last_item([],Algorithm2a,Algorithm2),!.
      %%string_concat(Algorithm3e,")",Algorithm2))).
/**
	(
	memberlp2p12(Functions2,Algorithm1,Algorithm2))
	).
**/
	
memberlp2p1(Functions2,Algorithm1,Algorithm2) :-
        Functions2=[Function,Arguments2],
                interpretstatementlp2p2a(Function,Algorithm1,Algorithm3a),
                		string_concat(Algorithm3a,"(",Algorithm3d),
        interpretstatementlp2p2b(Arguments2,Algorithm3d,Algorithm2a),
        write_full_stop_if_last_item([],Algorithm2a,Algorithm2),!.
/**
        ;
	(memberlp2p13(Functions2,Algorithm1,Algorithm2)
	)).
**/

memberlp2p1(Functions2,Algorithm1,Algorithm2) :-
        Functions2=[Function],
        interpretstatementlp2p2b(Function,Algorithm1,Algorithm2a),
        write_full_stop_if_last_item([],Algorithm2a,Algorithm2),!.
/**
	,
	(Functions2=[_Function|Functions3],
				write_comma_if_not_empty_list(Functions3,Algorithm2b,Algorithm2a),
	memberlp2p1(Functions3,Algorithm2a,Algorithm2c),
					write_full_stop_if_last_item(Functions3,Algorithm2c,Algorithm2))
	).
**/

interpretbodylp2p([],Algorithm1,Algorithm1) :- !.
interpretbodylp2p(Body,Algorithm1,Algorithm2) :-
	Body=[Statement|Statements],
	Statement=[v,_],
	interpretstatementlp2p2a(Statement,Algorithm1,Algorithm3),		write_comma_if_not_empty_list(Statements,Algorithm3,Algorithm4),
interpretbodylp2p(Statements,Algorithm4,Algorithm2),
%%write_full_stop_if_last_item(Statements,Algorithm5,Algorithm2),
!.
interpretbodylp2p(Body,Algorithm1,Algorithm2) :-
        Body=[[Statements1|Statements1a]|Statements2],
		not(predicate_or_rule_name(Statements1)),
	string_concat(Algorithm1,"(",Algorithm3),
	interpretbodylp2p([Statements1],Algorithm3,Algorithm4),
	write_comma_if_not_empty_list(Statements1a,Algorithm4,Algorithm5),
	interpretbodylp2p(Statements1a,Algorithm5,Algorithm6),
		string_concat(Algorithm6,")",Algorithm6a),
			write_comma_and_newline_if_not_empty_list(Statements2,Algorithm6a,Algorithm7),
	interpretbodylp2p(Statements2,Algorithm7,Algorithm2),
				%%write_full_stop_if_last_item(Statements2,Algorithm8,Algorithm2),
	!.
        
interpretbodylp2p(Body,Algorithm1,Algorithm2) :-
        Body=[[[n,not],[Statement]]|Statements2],
	string_concat(Algorithm1,"not((",Algorithm3),
        	interpretbodylp2p([Statement],Algorithm3,Algorithm4),
		string_concat(Algorithm4,"))",Algorithm5),
	write_comma_if_not_empty_list(Statements2,Algorithm5,Algorithm6),
        interpretbodylp2p(Statements2,Algorithm6,Algorithm2),
        				%%write_full_stop_if_last_item(Statements2,Algorithm7,Algorithm2),
	!.

interpretbodylp2p(Body,Algorithm1,Algorithm2) :-
        Body=[[[n,or],[Statements1,Statements2]]|Statements3],
        	string_concat(Algorithm1,"((",Algorithm3),

        interpretbodylp2p([Statements1],Algorithm3,Algorithm4),
                	string_concat(Algorithm4,");(",Algorithm5),

        interpretbodylp2p([Statements2],Algorithm5,Algorithm6),
		string_concat(Algorithm6,"))",Algorithm7),
	write_comma_if_not_empty_list(Statements3,Algorithm7,Algorithm8),
        interpretbodylp2p(Statements3,Algorithm8,Algorithm2),
        				%%write_full_stop_if_last_item(Statements3,Algorithm9,Algorithm2),
        !.

interpretbodylp2p(Body,Algorithm1,Algorithm2) :-
        Body=[[[n,"->"],[Statements1,Statements2]]|Statements3],
        	string_concat(Algorithm1,"(",Algorithm3),
        interpretbodylp2p([Statements1],Algorithm3,Algorithm4),
        	string_concat(Algorithm4,"]->(",Algorithm5),
                interpretbodylp2p([Statements2],Algorithm5,Algorithm6),
        	string_concat(Algorithm6,"))",Algorithm7),
	write_comma_if_not_empty_list(Statements3,Algorithm7,Algorithm8),
        interpretbodylp2p(Statements3,Algorithm8,Algorithm2),
        				%%write_full_stop_if_last_item(Statements3,Algorithm9,Algorithm2),
        !.

interpretbodylp2p(Body,Algorithm1,Algorithm2) :-
        Body=[[[n,"->"],[Statements1,Statements2,Statements2a]]|Statements3],
        	string_concat(Algorithm1,"(",Algorithm3),
        interpretbodylp2p([Statements1],Algorithm3,Algorithm4),
        	string_concat(Algorithm4,"->(",Algorithm5),
                interpretbodylp2p([Statements2],Algorithm5,Algorithm6),
        	string_concat(Algorithm6,");(",Algorithm7),
                interpretbodylp2p([Statements2a],Algorithm7,Algorithm8),
        	string_concat(Algorithm8,"))",Algorithm9),
	write_comma_if_not_empty_list(Statements3,Algorithm9,Algorithm10),
        interpretbodylp2p(Statements3,Algorithm10,Algorithm2),
        				%%write_full_stop_if_last_item(Statements3,Algorithm11,Algorithm2),
        !.

interpretbodylp2p(Body,Algorithm1,Algorithm2) :-
%trace,
        Body=[[[n,code]|Statements1]|Statements3],
        	string_concat(Algorithm1,"{",Algorithm3),
        	%trace,
        interpretbodylp2p(Statements1,Algorithm3,Algorithm4),
                	string_concat(Algorithm4,"}",Algorithm7),

	write_comma_if_not_empty_list(Statements3,Algorithm7,Algorithm8),
        interpretbodylp2p(Statements3,Algorithm8,Algorithm2),
        				%%write_full_stop_if_last_item(Statements3,Algorithm9,Algorithm2),
        !.

interpretbodylp2p(Body,Algorithm1,Algorithm2) :-
	Body=[Statement|Statements],
	not(predicate_or_rule_name(Statement)),
	interpretstatementlp2p1(Statement,Algorithm1,Algorithm3),		write_comma_if_not_empty_list(Statements,Algorithm3,Algorithm4),
interpretbodylp2p(Statements,Algorithm4,Algorithm2),
%%write_full_stop_if_last_item(Statements,Algorithm5,Algorithm2),
!.

interpretbodylp2p(Arguments1,Algorithm1,Algorithm2) :-
	Arguments1=[Arguments2|Arguments3],
	interpretstatementlp2p2([Arguments2],Algorithm1,Algorithm3a),
	write_comma_if_not_empty_list(Arguments3,Algorithm3a,Algorithm4),
	interpretbodylp2p(Arguments3,Algorithm4,Algorithm2),!.

write_comma_if_not_empty_list(Statements2,Algorithm6,Algorithm7) :-
	(not(Statements2=[])->string_concat(Algorithm6,",",Algorithm7);
	Algorithm6=Algorithm7),!.

write_comma_and_newline_if_not_empty_list(Statements2,Algorithm6,Algorithm7) :-
	(Statements2=[]->string_concat(Algorithm6,",\n",Algorithm7);
	Algorithm6=Algorithm7),!.

write_full_stop_if_last_item(Statements2,Algorithm8,Algorithm2) :-
	((length(Statements2,A),(A=0%%->true;A=1
	)
	)->string_concat(Algorithm8,".\n",Algorithm2);
	Algorithm8=Algorithm2),!.

write_close_bracket_and_full_stop_if_last_item(Statements2,Algorithm8,Algorithm2) :-
	((length(Statements2,A),(A=0%%->true;A=1
	))->string_concat(Algorithm8,").\n",Algorithm2);
	Algorithm8=Algorithm2),!.

write_close_bracket_if_last_item(Statements2,Algorithm8,Algorithm2) :-
	((length(Statements2,A),(A=0%%->true;A=1
	))->string_concat(Algorithm8,")",Algorithm2);
	Algorithm8=Algorithm2),!.

write_close_bracket_and_comma_if_not_empty_list(Statements2,Algorithm6,Algorithm7) :-
	(not(Statements2=[])->string_concat(Algorithm6,"),",Algorithm7);
	Algorithm6=Algorithm7),!.
	
interpretstatementlp2p1(Statement,Algorithm1,Algorithm2) :-
	Statement=[[N_or_v,Name]],(N_or_v=n;N_or_v=v),
	interpretstatementlp2p2a([N_or_v,Name],Algorithm1,Algorithm2),!.
        	
interpretstatementlp2p1(Statement,Algorithm1,Algorithm2) :-
	Statement=[[N_or_v,Name],Arguments],(N_or_v=n;N_or_v=v),
	interpretstatementlp2p2a([N_or_v,Name],Algorithm1,Algorithm3a),
        	string_concat(Algorithm3a,"(",Algorithm3),
	interpretstatementlp2p2(Arguments,Algorithm3,Algorithm4),
        	string_concat(Algorithm4,")",Algorithm2),!.

interpretstatementlp2p2([],Algorithm1,Algorithm1) :- !.
interpretstatementlp2p2(Arguments1,Algorithm1,Algorithm2) :-
	Arguments1=[Arguments1a|Arguments2],
	interpretstatementlp2p2a(Arguments1a,Algorithm1,Algorithm3),
		write_comma_if_not_empty_list(Arguments2,Algorithm3,Algorithm4),
	interpretstatementlp2p2(Arguments2,Algorithm4,Algorithm2),!.
	   %%write_close_bracket_and_full_stop_if_last_item(Arguments2,Algorithm5,Algorithm2).

interpretstatementlp2p2b([],Algorithm1,Algorithm1) :- !.
interpretstatementlp2p2b(Arguments1,Algorithm1,Algorithm2) :-
	Arguments1=[Arguments1a|Arguments2],
	interpretstatementlp2p2a(Arguments1a,Algorithm1,Algorithm3),
		write_comma_if_not_empty_list(Arguments2,Algorithm3,Algorithm4),
	interpretstatementlp2p2b(Arguments2,Algorithm4,Algorithm5),
	   write_close_bracket_if_last_item(Arguments2,Algorithm5,Algorithm2),!.

interpretstatementlp2p2a(Arguments1,Algorithm1,Algorithm2) :-
	interpretstatementlp2p3(Arguments1,Name),
	string_concat(Algorithm1,Name,Algorithm2),!.

interpretstatementlp2p3([n,Name],Name) :- !.
interpretstatementlp2p3([v,Name1],Name2) :- string_concat(A,B,Name1),atom_length(A,1),upcase_atom(A,A1),string_concat(A1,B,Name2),!.
%%interpretstatementlp2p3([],"[]") :- !.
%%interpretstatementlp2p3("","\"\"") :- !.
interpretstatementlp2p3([Term1],Term2) :- interpretstatementlp2p3(Term1,Term3),	
	string_concat("[",Term3,Term4),	string_concat(Term4,"]",Term2),!.
interpretstatementlp2p3(Value1,Value2):-term_to_atom(Value1,Value2),!.



