:- use_module(library(julian/lang/en), [english_form//1]).
:- use_module(library(tap)).

:- set_prolog_flag(double_quotes, codes).

% simple day of the week names
phrase(english_form(dow(monday)),"Monday").
phrase(english_form(dow(thursday)),"thursday").


% an empty repitition is no repition at all
empty(fail) :-
    phrase(english_form(_),"").


'jibberish after day of week'(fail) :-
    phrase(english_form(dow(friday)),"friday is fun").


weekday :-
    phrase(english_form(X), "weekday"),
    X == weekday.


'days of the week list' :-
    phrase(english_form(X), "monday, Tuesday or thursday"),
    X == dow([monday,tuesday,thursday]).


'second tuesday' :-
    phrase(english_form(X), "second tuesday"),
    X == nth(2, dow(tuesday)).
'final weekday' :-
    phrase(english_form(X), "final weekday"),
    X == nth(-1, weekday).


'each day' :-
    phrase(english_form(X), "each day"),
    X == true.


'single month' :-
    phrase(english_form(X), "August"),
    X == month(august).
'months list' :-
    phrase(english_form(X), "January, April, july and October"),
    X == month([january,april,july,october]).


'day of week in month' :-
    phrase(english_form(X), "second friday in May"),
    X == [month(may), nth(2,dow(friday))].


'list of refinements' :-
    phrase(english_form(X), "first Monday of April in even years"),
    X = [gregorian(_, _, _), month(april), nth(1, dow(monday))].


'month name and year' :-
    phrase(english_form(X), "August 2013"),
    X = gregorian(2013,8,_).


'second and fourth Sunday' :-
    phrase(english_form(X), "second and fourth sunday"),
    X == nth([2,4], dow(sunday)).


'month name and day number' :-
    phrase(english_form(X), "February 1st"),
    X = gregorian(Y,M,D),
    var(Y),
    M == 2,
    D == 1.

'even years' :-
    phrase(english_form(X), "even years"),
    X = gregorian(Y,M,D),

    % none of the date components is bound
    var(Y),
    var(M),
    var(D),

    % and Y only allows even numbered years
    forall( member(Year,[1998, 2000, 2002, 2012])
          , Y = Year
          ),
    forall( member(Year,[1997, 1999, 2001, 2013])
          , Y \= Year
          ).
