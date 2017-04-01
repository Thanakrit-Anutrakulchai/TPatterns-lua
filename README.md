# TPatterns-lua
     Pattern matching on tables like in functional programming, in Lua
 
   ****THIS MODULE CURRENTLY SUPPORTS LUA 5.1 ONLY!
    A module that brings pattern matching from functional programming languages into Lua's tables.
    This module relies heavily on (one-sided) unification. This file refers to a 'substitution hash'
    which is just a table with keys being "variable" names and values as the values they represent.
    
    10 functions are exported: case, var, is_var, call, DO, 
                                match, match_all, match_nomt, match_all_nomt and match_cond
    
    % Usages down below %
    Descriptions of functions:
    var: var(name) = setmetatable({name = name}, Var) 
      var('x') returns a "variable" named x. Used in matching.
      Note that if the variable is named '_', it can be used to match multiple different values,
      but it cannot be referred to from a Do/Call block (value will be nil)
      
    is_var: is_var(obj) = getmetatable(obj) == Var
      is_var returns true if obj is a "variable". obj is not a string.
    
    case: case(obj) = setmetatable({obj}, Case) 
      wraps an object (table, var, normal value) to be combined with a Call or Do object. Used in matching.
    
    call: call(fn, [...]) = setmetatable( {func = fn, args = {...}}, Call )
      wraps up a function and arguments passed to it to be combined with a Case object. Used in matching.
      when the function is called, a hash storing the variable-value pairs gets passed as the final argument.
    
    DO:  DO(str, env) = setmetatable( {str, env}, Do )
         wraps up a string and an environment table to be passed to loadstring to generate a function.
      Combined with a Case object.Used in matching. If the table passed doesn't have a metatable, 
      its metatable gets set to {__index = _G}. str should be an expression.
      
    match: match (obj) { case(c1) - DO(str, env), case(c2) - call(fn, a1, a2) } 
            = loadstring(etc..'return '..str, setmetatable(env, {__index = _G}))(substitution hash)
              OR fn(a1, a2, substitution hash)
    > attempts to unify values in obj to values in c1 and (if c1 doesn't match-up) c2.
    > returns nil if neither c1 nor c2 matches
    > most non-table values can also be matched on.
    > to access the hash in a DO block, the varArgs (...) pattern must be used.
    > a1 and a2 can be variables, and their values (what they matched) will be passed. Modifying 
      the hash with-in str or fn *shouldn't* affect the values (unless if it's a reference e.g. to a table).
    > if obj has a metatable, the cases will not match if they have a different one (or none at all).
    > *the metatable's __eq field is, however, ignored. its __index field is also ignored.
    > position of arguments don't matter e.g. match { cases } (obj) works
    > match (o1, o2, ...) { cases } is "syntatic sugar" for match {o1, o2, ...}  { cases }
        e.g. case{o1, o2, ...} will match, as well as case(o1, o2, ...)   
        cases must still be in an array. ( case1, case2, ... ) will not work.
    
      ***IMPORTANT: match will fail on a table if the array parts of a case and the object have different length!
           HOWEVER!> IT WILL NOT FAIL IF THERE ARE LESS KEYS IN THE *CASE* THAN IN THE *OBJECT*
                      Use match_all if all keys must match! (This is so you don't have to match every single key in a complex object.)
    
    
    match_all: match_all (obj) { case(c1) - str, case(c2) - function(hash) ... end }
      same as above, except that all keys in a table must match.
      
    match_nomt: match_nomt ( point(3, 5) ) { case{ x = var'x', y = var'y' } - DO/call(...) }
         same as match, except that metatables are ignored completely. especially useful
      when a table cannot be constructed with a variable inside
      e.g. point(var'x', 5) might error 'point> Bad Argument #1: number expected, got table' (variable)
      if point(n, m) = setmetatable { x = n, y = m }, then the case will match.
      
      
    match_all_nomt: same as above, except it checks for all keys, like match_all.
    
    
    match_cond: match_cond (constraint) (obj) { case(c1) - call(fn, a1, s2) }
    > This can be thought of as the `meat` of the module. It might had been assumed
      that match_cond is implemented in terms of match, but it's rather the other way around.
    > Originally, match_cond wasn't meant to be exported. However, its usefulness and
      flexibility, despite its awkward usage, was too good to pass up on.
      
      
         let's say obj = { x1, x2, x3 } and c1 = { y1, y2, y3 }
      constraint is a predicate that will be called with obj, c1, and the substitution hash
      if constraint returns false or nil, obj will fail to match on c1.
      if it succeeds however, constraint will *then* be called with x1, y1, and the hash again.
      now if it returns false, the matching fails. else x2, y2, and hash is passed to it... 
      and on and on until everything is compared.
      
         match_cond can also be thought of as a `factory` that takes a predicate (constraint),
      and returns a `matcher.` In fact, all the match functions are defined in terms of match_cond.
      
      match_nomt     = match_cond(function() return true end)
      match          = match_cond(checkMt)
      match_all_nomt = match_cond(checkSize)
      match_all      = match_cond(function(a, b) return checkSize(a, b) and checkMt(a, b) end)
      
      ***As stated above, ALL values in a and b, as well as a and b themselves, are passed to the function.
      Thus, checkSize and checkMt checks the size(in terms of keys) and metatable of a, b and ALL tables
      in them AND returns true for values of any other type (else matching will fail on those types)
    
    --more examples in 'test.lua' and 'examples.lua'
    
    
    local T = require 'TPatterns'
    local case, match, var, match_cond, match_all = T.case, T.match, T.var, T.match_cond, T.match_all
    local call, DO, match_nomt, match_all_nomt = T.call, T.DO, T.match_nomt, T.match_all_nomt
    
    Usages>> match (obj) { case(c1) - "*result here*", case(c2) - function(t) return 'functions also allowed.' end }
      
      local ans = match {2, 4, 6, 8} { case{2, var'x', 6, var'y'} - 'x+y', 
                                       case{1, 3, 5, var'z'} - 'z' }
        ans will be 12.
        
        
        match (1) { case(1) - 'true' } returns true
        match {2, 4, 6, 8} { case{2, var'x', 6, var'y'} - 'x+y', case{1, 3, 5, var'z'} - 'z' } returns 12
        match {pi = 3.14, deg = 60, lucky = 7} { case{pi = var'x', deg = 60} - 'x' } returns 3.14 
        match_all {pi = 3.14, deg = 60, lucky = 7} { case{pi = var'x', deg = 60} - 'x' } returns nil
        match { case{a = var'x', b = var'y', c = var'z'} - 'z', case{a = var'x'} - 'x' } {a=3, b=5} returns 3
        
        match (point(1, 2)) { case(var'x') - 'x' } returns point(1, 2)
        match_all {1, 3, a = 5, b = 7}
          { case{var'_', var'_', a = var'_', b = var'x'} - DO('type(x)', {}) } returns 'number'
         
        match {1, 2} 
          { case{var'x', var'x'} - '3', case{var'x', var'_', var'_', var'_'} - '5' } returns nil
          
        match_all (pair(3, 4)) 
          { case{fst = 3, snd = 4} - '1', case(pair(3, 4)) - '2' } returns 2
          
        match_nomt (point(1, 2)) 
          { case{ x = var'x', y = var'y' } - 'x+y+135', case(point(1, 2)) - '1' } returns 138
          
        match {a = 1, b = 3, c = 5, d = 7} 
          { case{a = var's', b = var't', c = var'u', d = var'v'} - function(t) return t.s + t.t + t.u + t.v end }
          returns 16
          
        match_cond (function(x) return (type(x) ~= 'number' and true) or x > 3 end) (pair(4, 7))
          { case(pair(var'x', var'y')) - call(function(a, b, c) return a + b + c end, var'x', var'y', 5),
            case(pair(4, var'z')) - DO('double(z)',{double = function(x) return x*2 end} ) }
            returns 16
            
        They can be used as switch statements e.g.
          match (value) {
                          case(5) - ... ,
                          case('hello') - ...
                        }
                        
        But their strength is in using the binded variables. e.g.
        local append;
        append = match_all_nomt { 
                          case( mempty, var'ys' ) - 'ys',
                          case({head = var'x', tail = var'xs'}, var'ys') - function(t) return cons(t.x, append(t.xs, t.ys)) end 
                        }
                        
         defines a function `append` that takes two lists of the form { head = value, tail = restOfList } where mempty = empty list
         and concatenates them together.
         
