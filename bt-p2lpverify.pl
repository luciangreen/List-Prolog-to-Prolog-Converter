% back-translates Prolog to List Prolog then List Prolog to Prolog.

% bt-p2lpverify(A,B).
% A - output: total tests
% B - output: total correct results

% bt-p2lpverify1(N,B).
% A - input: number to test
% B - output: result

bt-p2lpverify(BL,RL) :-
findall(A,(p2lp_test(N,_I,O),((lp2p1(O,I1),p2lpconverter([string,I1],O2),O=O2)->(writeln([bt-p2lpverify,N,passed]),A=passed);(writeln([bt-p2lpverify,N,failed],A=failed)))),B),
length(B,BL),
findall(_,member(passed,B),R),length(R,RL),!.

bt-p2lpverify1(N,A) :-
p2lp_test(N,_I,O),((lp2p1(O,I1),p2lpconverter([string,I1],O2),O=O2)->(writeln([bt-p2lpverify,N,passed]),A=passed);(writeln([bt-p2lpverify,N,failed],A=failed))),!.

% to do
% back-translation, on I not O

/*
bt1-p2lpverify(BL,RL) :-
findall(A,(p2lp_test(N,I,_O),(p2lpconverter([string,I],O1),(lp2p1(O1,I1),I=I1)->(writeln([bt1-p2lpverify,N,passed]),A=passed);(writeln([bt1-p2lpverify,N,failed]),A=failed))),B),
length(B,BL),
findall(_,member(passed,B),R),length(R,RL),!.
*/
