/* Assignment A
   Student name: Ruiqi Li
   Student number: D18125180
   Student course & year : MSc Advanced Software Development 2018
   Class Information: DT228A – PROGRAMMING PARADIGMS
   Lecture Name: Dr. Pierpaolo Dondio
*/

/* Part 1 - Flights */

flight(london,dublin,aerlingus,500,45,150).
flight(rome,london,ba,1500,150,400).
flight(rome,paris,airfrance,1200,120,500).
flight(paris,dublin,airfrance,600,60,200).
flight(berlin,moscow,lufthansa,3000,300,900).
flight(paris,amsterdam,airfrance,400,30,100).
flight(berlin,dublin,lufthansa,1200,120,900).
flight(london,newyork,ba,5000,700,1100).
flight(dublin,newyork,aerlingus,4500,360,800).
flight(dublin,cork,ryanair,300,50,50).
flight(dublin,rome,ryanair,2000,150,70).
flight(dublin,chicago,aerlingus,5500,480,890).
flight(amsterdam,hongkong,klm,7000,660,750).
flight(london,hongkong,ba,7500,700,1000).
flight(dublin,amsterdam,ryanair,1000,90,60).
flight(moscow,newyork,aerflot,9000,720,1000).
flight(moscow,hongkong,aerflot,5500,420,500).
flight(newyork,chicago,aa,3000,240,430).
flight(dublin,london,aerlingus,500,45,150).
flight(london,rome,ba,1500,150,400).
flight(paris,rome,airfrance,1200,120,500).
flight(dublin,paris,airfrance,600,60,200).
flight(moscow,berlin,lufthansa,3000,300,900).
flight(amsterdam,paris,airfrance,400,30,100).
flight(dublin,berlin,lufthansa,1200,120,900).
flight(newyork,london,ba,5000,700,1100).
flight(newyork,dublin,aerlingus,4500,360,800).
flight(cork,dublin,ryanair,300,50,50).
flight(rome,dublin,ryanair,2000,150,70).
flight(chicago,dublin,aerlingus,5500,480,890).
flight(hongkong,amsterdam,klm,7000,660,750).
flight(hongkong,london,ba,7500,700,1000).
flight(amsterdam,dublin,ryanair,1000,90,60).
flight(newyork,moscow,aerflot,9000,720,1000).
flight(hongkong,moscow,aerflot,5500,420,500).
flight(chicago,newyork,aa,3000,240,430).
flight(dublin,sao_paulo,airfrance,10000,900,800).
flight(sao_paulo,newyork,airfrance,7000,840,650).
flight(rio,berlin,lufthansa,11000,1200,1100).

country(dublin,ireland).
country(cork,ireland).
country(london,uk).
country(rome,italy).
country(moscow,russia).
country(hongkong,china).
country(amsterdam,holland).
country(berlin,germany).
country(paris,france).
country(newyork,usa).
country(chicago,usa).
country(sao_paulo,brazil).
country(rio,brazil).

/* Q1 */
list_airport(X,L):- findall(City,country(City,X),L).

/* Q2 */
trip(X,Y,[X|P]):- path_list(X,Y,[Y],P). /* stop even when meet Y*/
path_list(A,B,_,[B]):- flight(A,B,_,_,_,_).
path_list(A,B,V,[Z|P1]):- flight(A,Z,_,_,_,_), not(member(Z,V)), path_list(Z,B,[A|V],P1).

/* Q3 */
all_trip(X,Y,T):- findall(Trip,trip(X,Y,Trip),T).

/* Q4 */
trip_dist(X,Y,[[X|P],D]):- dist_list(X,Y,[Y],P,D).
dist_list(A,B,_,[B],D1):- flight(A,B,_,D1,_,_).
dist_list(A,B,V,[Z|P1],D):- flight(A,Z,_,D1,_,_), not(member(Z,V)), dist_list(Z,B,[A|V],P1,D2), D is D1 + D2.

/* Q5 */
trip_cost(X,Y,[[X|P],C]):- cost_list(X,Y,[Y],P,C).
cost_list(A,B,_,[B],C1):- flight(A,B,_,_,_,C1).
cost_list(A,B,V,[Z|P1],C):- flight(A,Z,_,_,_,C1), not(member(Z,V)), cost_list(Z,B,[A|V],P1,C2), C is C1 + C2.

/* Q6 */
trip_change(X,Y,[T,I]):- trip(X,Y,T), length(T,I1), I is I1-2.

/* Q7 */
trip_avoid(X,Y,[X|P],A):- path_avoid(X,Y,[Y],P,A).
path_avoid(X,Y,_,[Y],A):- flight(X,Y,R,_,_,_), R \= A.
path_avoid(X,Y,V,[Z|P1],A):- flight(X,Z,R,_,_,_), R \= A, not(member(Z,V)), path_avoid(Z,Y,[X|V],P1,A).

all_trip_noairline(X,Y,T,A):- findall(Trip,trip_avoid(X,Y,Trip,A),T).

/* Q8 */
trip_time(X,Y,[[X|P],T]):- time_list(X,Y,[Y],P,T).
time_list(A,B,_,[B],T1):- flight(A,B,_,_,T1,_).
time_list(A,B,V,[Z|P1],T):- flight(A,Z,_,_,T1,_), not(member(Z,V)), time_list(Z,B,[A|V],P1,T2), T is T1 + T2.

cheapest(X,Y,T,C):- aggregate(min(C1,T1),trip_cost(X,Y,[T1,C1]),min(C,T)). /* sorted alreday */

shortest(X,Y,T,C):- aggregate(min(C1,T1),trip_dist(X,Y,[T1,C1]),min(C,T)).

fastest(X,Y,T,C):- aggregate(min(C1,T1),trip_time(X,Y,[T1,C1]),min(C,T)).

/* Q9 */
trip_to_nation(X,Y,T):- list_airport(Y,L),trip_nation(X,L,T).
trip_nation(X,[],T).
trip_nation(X,[H|P],[X|T]):- path_list(X,H,[H],T).
trip_nation(X,[H|P],T):- trip_nation(X,P,T).

/* Q10 */
all_trip_to_nation(X,Y,T) :- findall(Trip,trip_to_nation(X,Y,Trip),T).


/* Part 2 - Blocks */

/* Q1 */
print_list([]).
print_list([H|T]):- write(H), write("|"),print_list(T).

print_status([]).
print_status([H|T]):- write("|"), print_list(H),nl, print_status(T).

/* Q2 */
count_blocks([H],[R]):- length(H,R).
count_blocks([H|T],[R|P]):- length(H,R), count_blocks(T,P).

/* Q3 */
high([H1|T],X,H):- nth0(H,H1,X).
high([H1|T],X,H):- high(T,X,H).

/* Q4 */
all_same_height(B,H,L):- findall(Blocks,high(B,Blocks,H),L).

/* Q5 */
same_height(B,X,Y):- high(B,X,H1), high(B,Y,H2), H1 == H2, !.

/* Q6 */
check_top([H1|T],X):- nth0(H,H1,X), length(H1,R1), R is R1-1, R == H,!.
check_top([H1|T],X):- check_top(T,X).

replace_del([H|T],X,1,[L|T]):- delete(H,X,L), !.
replace_del([H|T],X,I,[H|P]):- I > 0, I1 is I-1, replace_del(T,X,I1,P),!.

replace_app([H|T],X,1,[L|T]):- append(H,[X],L), !.
replace_app([H|T],X,I,[H|P]):- I > 0, I1 is I-1, replace_app(T,X,I1,P),!.

moveblock(B,X,S1,S2):- check_top(B,X),replace_del(B,X,S1,B1),replace_app(B1,X,S2,B3),print_status(B3).

/* Part 3 */
