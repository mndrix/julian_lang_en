:- module(julian_lang_en, [english_form//1]).

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics), [ integer//1
                                   , string//1
                                   ]).
:- use_module(library(lambda)).
:- use_module(library(julian/util), [dow_number/2]).
:- use_module(library(julian/calendar/gregorian), [month_number/2]).



% TODO implement the module
% TODO make string//1 an argument like Separator
% TODO so that I can call phrase(split(comma, day_of_week, Parts), Xs)
split(Separator, [Part|Parts]) -->
    string(Part),
    Separator,
    split(Separator, Parts).
split(_, [Part], Part, []).


end_of_content([],[]).


comma --> " and ".
comma --> " or ".
comma --> ", ".
comma --> ",".


within --> " during ".
within --> " in ".
within --> " of ".

ordinal(N) -->
    ordinal_short(N).
ordinal(N) -->
    ordinal_long(N).

ordinal_long(1) --> "first".
ordinal_long(2) --> "second".
ordinal_long(3) --> "third".
ordinal_long(4) --> "fourth".
ordinal_long(5) --> "fifth".

ordinal_short(N) -->
    integer(N),
    ( "th"; "st"; "nd"; "rd" ).


parity(X) -->
    "even",
    !,
    { X mod 2 #= 0 }.
parity(X) -->
    "odd",
    !,
    { X mod 2 #= 1 }.


% True if Day is an atom representing the day of week named in Codes
codes_dow(Codes, Day) :-
    atom_codes(Atom, Codes),
    downcase_atom(Atom, Day),
    dow_number(Day, _).


% True if Month is an atom representing the month named in Codes
codes_month(Codes, Month) :-
    atom_codes(Atom, Codes),
    downcase_atom(Atom, Month),
    month_number(Month, _).


:- multifile english_form//1.
english_form(true) -->
    "each day",
    !.
english_form(weekday) -->
    "weekday",
    !.
english_form(dow(Day)) -->
    string(Word),
    end_of_content,
    { codes_dow(Word, Day) },
    !.
english_form(dow(Days)) -->
    split(comma, Words),
    { maplist(codes_dow, Words, Days) },
    !.
english_form(month(Month)) -->
    string(Word),
    end_of_content,
    { codes_month(Word, Month) },
    !.
english_form(month(Months)) -->
    split(comma, Words),
    { maplist(codes_month, Words, Months) },
    !.
english_form(gregorian(Y,M,_)) -->
    string(Word),
    { codes_month(Word, Month) },
    { month_number(Month, M) },
    " ",
    integer(Y),
    { Y > 999 },
    !.
english_form(gregorian(_,M,D)) -->
    string(Word),
    { codes_month(Word, Month) },
    { month_number(Month, M) },
    " ",
    ordinal(D),
    !.
english_form(gregorian(Y,_,_)) -->
    parity(Y),
    " years",
    !.
english_form([A,B|T]) -->  % list of at least two elements (indexable)
    { Forms = [A,B|T] },
    split(within, Parts),
    { length(Parts, N), N > 1 },
    { maplist(\Part^Form^once(phrase(english_form(Form),Part)), Parts, Fs) },
    !,
    % optimization: constraints on the end are usually more concrete
    { reverse(Fs, Forms) }.
english_form(nth(-1,Form)) -->
    "final ",
    english_form(Form),
    !.
english_form(nth([N0,N1],Form)) -->
    ordinal(N0),
    comma,
    ordinal(N1),
    " ",
    english_form(Form),
    !.
english_form(nth(N,Form)) -->
    ordinal(N),
    " ",
    english_form(Form),
    !.

