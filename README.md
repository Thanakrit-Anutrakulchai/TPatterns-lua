
## Pattern matching on tables like in functional programming, in Lua

****This was made and tested with Lua 5.1 !

A module that brings pattern matching (in the style of functional programming) to Lua's tables.
This relies heavily on (one-sided) unification. It is deeply inspired by the ideas of 
miniKanren and microKanren (http://minikanren.org/ & http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf)
The 'substitution hash' referred to below is just a table with keys as variable names and table values as the 
values of the variable.

## Exported functions
10 functions are exported: case, var, is_var, call, DO, 
                            match, match_all, match_nomt, match_all_nomt and match_cond

```lua
  var: var(name) = setmetatable({name = name}, Var)
```
var('x') returns a variable named x. Used in matching.
Note that if the variable is named '_', it can be used to match multiple different values,
but it cannot be referred to from a Do/Call block (value will be nil)

```lua
  is_var: is_var(obj) = getmetatable(obj) == Var
```
is_var returns true if obj is a variable. obj is not an unwrapped string.

```lua
  case: case(obj) = setmetatable({obj}, Case)
```
wraps an object (table, var, normal value) to be combined with a Call or Do object. Used in matching.

```lua
  call: call(fn, [...]) = setmetatable( {func = fn, args = {...}}, Call )
```
wraps up a function and arguments passed to it to be combined with a Case object. Used in matching.
when the function is called, a table storing the variable-value pairs gets passed as the final argument.

```lua
  DO:  DO(str, env) = setmetatable( {str, env}, Do )
```
wraps up a string and an environment table to be passed to loadstring to generate a function.
Combined with a Case object. Used in matching. If the table passed doesn't have a metatable, 
its metatable gets set to {__index = _G}. str should be an expression.

```lua
    match: match (obj) { case(c1) - DO(str, env), case(c2) - call(fn, a1, a2) } 
            = loadstring(etc..'return '..str, setmetatable(env, {__index = _G}))(substitution hash)
              OR fn(a1, a2, substitution hash)
```
attempts to unify values in obj to values in c1, and then, if unsuccessful, on c2.
This returns nil if neither c1 nor c2 matches. Most non-table values can also be matched on.
Some things to note:
  * To access the hash in a DO block, use the varArgs (...) pattern.
  * If a1 and a2 are variables, their values (what they matched) will be passed. 
  * Modifying the hash with-in str or fn *shouldn't* affect the values (unless if it's a reference e.g. to a table).
  TODO: check if __eq is actually ignored. Check how __index works with for
  * If obj has a metatable, the cases will not match if they have a different one (or none at all).
      However, the metatable's __index field is ignored.
  * Positions of arguments don't matter e.g. match { cases } (obj) works provided obj isn't also a case block.
  * match (o1, o2, ...) { cases } is "syntatic sugar" for match {o1, o2, ...}  { cases }
        e.g. case{o1, o2, ...} will match, as well as case(o1, o2, ...)   
        cases must still be in an array. ( case1, case2, ... ) will not work. TODO: doesn't it?

Note: match will fail on a table if the array parts of a case and the object have different length.
However, it will not fail if there are less keys in the *case* than in the *object*!
Use match_all if all keys must match! 
(This is so you don't have to match every single key in a complex object.)

```lua
    match_all: match_all (obj) { case(c1) - str, case(c2) - function(hash) ... end }
```
same as above, except that all keys in a table must match.

```lua
    match_nomt: match_nomt ( point(3, 5) ) { case{ x = var'x', y = var'y' } - DO/call(...) }
```
same as match, except that metatables are ignored completely. especially useful
when a table cannot be constructed with a variable inside.
e.g. point(var'x', 5) might error with 'point> Bad Argument #1: number expected, got table' (variable)
If point(n, m) = setmetatable { x = n, y = m }, then the case will match.

```lua
    match_all_nomt
```
same as above, except it checks for all keys, like match_all.
```lua
    match_cond: match_cond (constraint) (obj) { case(c1) - call(fn, a1, s2) }
```
This can be thought of as the meat of the module.
Originally, match_cond wasn't going to be exported. However, its usefulness and
flexibility, despite its awkward usage, was too good to pass up on.

Let's say obj = { x1, x2, x3 } and c1 = { y1, y2, y3 },
`constraint` is a predicate that will be called with `obj`, `c1`, and the substitution hash.
If `constraint` returns `false` or `nil`, `obj` will fail to match on `c1`.
If it succeeds however, `constraint` will *then* be called with `x1`, `y1`, and the hash again.
Now if it returns false, the matching fails. else `x2`, `y2`, and hash is passed to it... 
and on and on until everything is compared.

`match_cond` can also be thought of as a "factory" that takes a predicate (constraint),
and returns a "matcher." In fact, all the match functions are defined in terms of `match_cond`.
```lua
    match_nomt     = match_cond(function() return true end)
    match          = match_cond(checkMt)
    match_all_nomt = match_cond(checkSize)
    match_all      = match_cond(function(a, b) return checkSize(a, b) and checkMt(a, b) end)
```

As stated above, ALL values in a and b, as well as a and b themselves, are passed to the function.
Thus, checkSize and checkMt checks the size (in terms of keys) and metatable of a, b and ALL tables
in them, and returns true for values of any other type.

## Examples
```lua
    local T = require 'TPatterns'
    local case, match, var, match_cond, match_all = T.case, T.match, T.var, T.match_cond, T.match_all
    local call, DO, match_nomt, match_all_nomt = T.call, T.DO, T.match_nomt, T.match_all_nomt

    -- ans == 12.
    local ans = match {2, 4, 6, 8} { case{2, var'x', 6, var'y'} - 'x+y', 
                                     case{1, 3, 5, var'z'} - 'z' }

    -- returns 138
    match_nomt (point(1, 2)) { case{ x = var'x', y = var'y' } - 'x+y+135', case(point(1, 2)) - '1' } 

    --returns 16
    match {a = 1, b = 3, c = 5, d = 7} { 
        case{a = var's', b = var't', c = var'u', d = var'v'} - function(t) 
            return t.s + t.t + t.u + t.v 
        end 
    }

    --returns 16
    match_cond (function(x) return type(x) ~= 'number' or x > 3 end) (pair(4, 7)) { 
        case(pair(var'x', var'y')) - call(function(a, b, c) return a + b + c end, var'x', var'y', 5),
        case(pair(4, var'z')) - DO('double(z)',{double = function(x) return x*2 end} )
    }
```
   -------------------------------------------------------------------------------------------------------------------------
```lua
    -- The matchers can be used as switch statements e.g.
    match (value) {
        case(5) - ... ,
        case('hello') - ...
    }

    -- But their strength is in using the binded variables. e.g.
    local append;
    append = match_all_nomt { 
        case( empty_list, var'ys' ) - 'ys',
        case({head = var'x', tail = var'xs'}, var'ys') - 
            function(t) return cons(t.x, append(t.xs, t.ys)) end 
    }

    --  defines a function `append` that takes two lists of the form { head = value, tail = restOfList } 
    --    or {} (for the empty list), and concatenates them together.

```
   -------------------------------------------------------------------------------------------------------------------------

Fuurther examples may be found in 'tests/tests.lua' and 'examples.lua'

NOTE: I realized too late that there was already a module that does something very similar 
to this called tamale, by silentbicycle. There was no intent in copying his work,
we both used the same method of unification. Tamale has more functionality right off the bat
with a slightly bigger footprint, although TPatterns provides enough primitives to build
more functionalities.
