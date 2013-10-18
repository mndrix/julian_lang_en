# Synopsis

    ?- use_module(library(julian)).
    ?- use_module(library(julian/lang/en)).

    % parse English into Julian constraints
    ?- phrase(english_form(Form), "Tuesday in April").
    Form = [month(april), dow(tuesday)].

    % iterate dates matching an English description
    ?- form_time(english("weekday in August 2013"), Dt),
       form_time(Y-M-D, Dt),
       date(T).
    Y = 2013,
    M = 8,
    D = 1 ;
    Y = 2013,
    M = 8,
    D = 2 ;
    Y = 2013,
    M = 8,
    D = 5 ;
    Y = 2013,
    M = 8,
    D = 6 ;
    ...

# Description

This module allows one to parse English language descriptions of date and time constraints.  For example, the phrase "monday or tuesday" describes any date which falls on a Monday or Tuesday in some week.  As a `library(julian)` constraint that's `dow([monday,tuesday])`.  This module converts between the two representations.

The following phrases are supported.  `$phrase` represents any other phrase the module supports (in other words, a recursive application of the parsing rules).

  * "each day" - matches all dates
  * "weekday" - matches Monday through Friday, inclusive
  * "Monday" - matches that day of the week (case insensitive)
  * "January" - matches days in that month (case insensitive)
  * "June 2014" - matches days in that month and year
  * "May 3rd" - matches a specific day without reference to year
  * "first Tuesday" - matches an ordinal day within its month
  * "final Thursday" - matches the final named day within its month
  * "even years" - years evenly divisible by 2
  * "odd years" - years not evenly divisible by 2
  * "$phrase and $phrase" - list of day names or month names (supports "or" and "," too)
  * "$phrase in $phrase" - apply both constraints (supports "of" and "during" too)

Rules can be combined to support fairly complex date constraints.  For example, "first Monday of April in even years" correctly recognizes April 7, 2014 and April 4, 2016 with no matching date in 2015.

# Caution

The rules for "second Wednesday" and "final Thursday", etc. can be a somewhat fragile.  They work great for recognizing whether a given date matches the pattern, but sometimes loop forever when trying to iterate all matching days.  This is considered a mistake and should be fixed.

# Changes in this Version

  * First public release

# Installation

Using SWI-Prolog 6.3 or later:

    ?- pack_install(julian_lang_en).

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/mndrix/julian_lang_en

@author Michael Hendricks <michael@ndrix.org>
@license BSD
