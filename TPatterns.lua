--[==[  ****THIS MODULE CURRENTLY SUPPORTS LUA 5.1 ONLY!
    A module that brings pattern matching in the style of functional programming languages to Lua's tables.
    This module relies heavily on (one-sided) unification.

    This file refers to a 'substitution hash' which is just a table with keys being "variable" names
    and values as the values they represent. If the value is nil, then it is a "free variable" and
    may later be bound to a more concrete value. The value can also be another variable, which will
    usually be further looked up if it is bound.

    Find examples in 'tests.lua' and 'examples.lua'.
    Further documentation is in the README.md
  ]==]

-- [ INIT ]
local sm, gm, ipairs, pairs = setmetatable, getmetatable, ipairs, pairs
local loadstring, type, assert = loadstring, type, assert
local error, setfenv, unpack, select = error, setfenv, unpack, select
local walk, isVar


-- Generates a string that will assign values to (all) the variable(s) provided
--  based on the substitution hash once loaded and ran as a function
--  with the hash as first argument
local function declareVars(obj)
  if isVar(obj) then
    -- the (...) argument refers to the subst hash
    return 'local '..obj.name..'='..'(...).'..obj.name..';'
  elseif type(obj) == 'table' then
    local accu = ''
    for _, v in pairs(obj) do
      accu = accu..declareVars(v)
    end
    return accu
  else
    return ''
  end
end

-- Wraps a string to be load(string)ed and an environment
--  for the resulting function to look up with
--  NOTE: If env does not have a meta-table, it will be set to from_G
--        Also, in the expression, access the subst hash with (...)
local doMT = {}
local function DO(str, env)
  return sm({str, env}, doMT)
end

-- Stores a function and the arguments to it,
-- which may be variables (to be looked up in the subst hash later)
-- NOTE: Final parameter to 'f' is the substitution hash!
local callMT = {}
local function call(f, ...)
  return sm({func = f, args = {...}}, callMT)
end

-- meta-table for environments to declare that the global environment is used
local from_G = {__index = _G}

-- Meta-table for the completed case, something of the form case(...) - DO/call(...)
-- getFunc returns a function that takes a substitution hash and evaulates the call/DO blocks
local caseMT = {getFunc =   function(self)
                local expr = self[2] --extract rhs

                -- if rhs is a DO block                        
                if gm(expr) == doMT then
                  -- append the str expression with all variables being pre-bound 
                  --   to their corresponding values inside the given hash (first arg.)
                  local header = declareVars(self[1])

                  -- Generate the function with the given env
                  local f = loadstring( header .. 'return '..expr[1] )
                  local env = expr[2]
                  if gm(env) then
                    return setfenv(f, env)
                  else
                    return setfenv(f, sm(env, from_G))
                  end

                -- if rhs is a call block
                elseif gm(expr) == callMT then
                    -- generate the function that takes the hash
                    return function(subst)

                      -- each variable is looked up in the hash before being passed
                      --  to inner function
                      local accu = {}
                      for _, v in ipairs(expr.args) do
                        if isVar(v) then accu[#accu+1] = walk(v, subst)
                        else accu[#accu+1] = v
                        end
                      end
                      -- final argument is subst hash
                      accu[#accu+1] = subst

                      -- eventually, call the inner, wrapped function with bound args and hash
                      return expr.func(unpack(accu))
                    end
                else error("match> 'DO' or 'call' block expected.", 2)
                end
              end
  } ; caseMT.__index = caseMT

-- Meta table for generating full cases
local halfcaseMT = {__sub = function(self, expr)
                              -- Syntatic sugars. case(...) - "{expr}" = case(...) - DO("{expr}", {})
                              --  and case(...) - function(hash) ??? end = case(...) - call(function(hash) ??? end)
                              if type(expr) == 'string' then return sm({self[1], DO(expr, {})}, caseMT) end
                              if type(expr) == 'function' then return sm({self[1], call(expr)}, caseMT) end
                              return sm({self[1], expr}, caseMT)
                            end}

-- Metatable for variables                                    
local varMT = {__tostring = function(self) return '$('..self.name..')' end}

-- [ FUNCTIONS ]
isVar = function(x) return gm(x) == varMT end --forward declared

--Create a custom variable with the given name
local function var(name)
  return sm({name = name}, varMT)
end

-- Look up the ultimate value of v in subst
-- i.e. return subst[v.name] or subst[subst[v.name]] or ... 
--      until it reaches a value or a free variable z such that subst[z.name] is nil
function walk(v, subst) --forward declared
  if not isVar(v) then
    return v
  else
    local res = walk(subst[v.name], subst)

    -- returns the end result of the look-up chain, whether it's a variable or value
    if res ~= nil then
      return res
    else
      return v
    end
  end
end


-- Helper function. Look at 'unify(case, obj, constraint)' below
local function unify_helper(x, y, subst, constraint)
  assert(not isVar(y), 'match> There can only be variables in the Cases!')

  -- Look up the value or free variable ultimately associated with x in subst
  local u = walk(x, subst)

  -- Check constraint given                              
  if not constraint(y, u, subst) then return nil end

  -- With "_", always passes ("unification" succeeds, nothing new is bound)                              
  if isVar(u) and u.name == '_' then return subst end

  -- If u is a (free!) variable then bind y to it
  if isVar(u) then
    subst[u.name] = y
    return subst

  -- If it's a value, check if it's equal to y
  elseif u == y then
    return subst

  -- If they're both tables, check inside contents     
  elseif type(u) == 'table' and type(y) == 'table' then
    -- Check array part
    if #u > 0 then
      -- If their arrays don't have the same size, it fails to unify
      if #u ~= #y then return nil end
      -- Check that elements at each index can be unified
      --  while remembering the same variables (and constraint)
      for i, e in ipairs(u) do
        subst = unify_helper(e, y[i], subst, constraint)
        if not subst then return nil end
      end
    end

    -- Check hash part, make sure all elements at each key *of only u* can be unified                                
    for k, e in pairs(u) do
      local r = y[k]
      -- Fails if case has a key not in obj                                  
      if r == nil then
        return nil
      end

      subst = unify_helper(e, r, subst, constraint)
      if not subst then return nil end
    end

    -- If nothing fails, passes, binding necessary inner-variables to values                                
    return subst
  end

  -- If they aren't both tables and they're not equal, unification fails
end


-- Attempts to unify the case and the object
-- Unification succeeds if the constraint holds AND: case == obj or case is a (free) variable or
--  both are tables and the values at each key *of the case* can be unified, 
--                  and if case has an array part, values at each indexes can be unified
--                      (remembering all already-bound variables, given the same constraint)

-- The constraint is called each time a unification attempt is made
--  It is called with the two inputs and the substitution hash.
--  It must always return true for unification to succeed
local function unify(case, obj, constraint) --BECAREFUL!!! Unification is one-sided!
  return unify_helper(case, obj, {}, constraint)
end


-- Wraps obj with the halfcaseMT meta-table. Needs other half (right-side of '-') to be completed
-- case(a1, a2, ...) is basically syntactic sugar for case({a1, a2, ...})
local function case(obj, ...)
  return select('#', ...)==0 and sm({obj}, halfcaseMT) or sm({ {obj, ...} }, halfcaseMT)
end

-- USAGE: match_cond (constraint) (obj) (cases) where cases is an array of cases
--      Alternatively: match_cond (constraint) (cases) (obj) where obj is an *empty* table or a non-table

-- match_cond (constraint) (o1, o2, o3, ...) (c1, c2, c3, ...) is
--  syntatic sugar for match_cond (constraint) {o1, o2, o3, ...} {c1, c2, c3, ...}
local function match_cond(constraint)
  --stores first set of values
  return function(a, ...)
    local x = (select('#', ...) == 0 and a) or {a, ...} ;
    --stores second set of values
    return function(b, ...)
      local y = (select('#', ...) == 0 and b) or {b, ...} ;
      --check that cases is (probably) given and assign appropriate sets of values to obj/cases
      local obj, cases
      assert(type(x) == 'table' or type(y) == 'table', 'match> an array of Cases must be given!')
      if type(y) == 'table' and #y > 0 and gm(y[1]) == caseMT then
        obj = x; cases = y
      else
        obj = y; cases = x
      end

      -- Attempt to unify (under constraint) each case based on index until it succeeds
      --  Then, calls first succeeded case's function/expression with the substition hash
      for _, c in ipairs(cases) do
        local subst = unify(c[1], obj, constraint)
        if subst then return (c:getFunc())(subst) end
      end
    end
  end
end

-- match_cond with no additional constraint on unification
local match_nomt = match_cond(function() return true end)


-- Check that all (sub-)tables have the same meta-table
-- non-tables pass the constraint automatically
local function checkMt(vals, vars)
  if type(vals) == 'table' and type(vars) == 'table' and not isVar(vars) then
    return gm(vals) == gm(vars)
  else
    return true
  end
end
local match = match_cond(checkMt) -- attempts to unify with checkMt constraint

-- Counts the number of keys in a table
local function size(t)
  local accu = 0
  for _, _ in pairs(t) do accu = accu + 1 end
  return accu
end

-- Constraint that ensures all (sub-)tables have all the same keys                                    
local function checkSize(vals, vars)
  if type(vals) == 'table' and type(vars) == 'table' and not isVar(vars) then
    return size(vals) == size(vars)
  else
    return true
  end
end

-- Attempts unification but ensure all keys match                                    
local match_all_nomt = match_cond(checkSize)

-- Attempts unification with all keys and meta-tables matching                                    
local match_all = match_cond(function(x, y) return checkSize(x, y) and checkMt(x, y) end)


-- [ MODULE EXPORT ]

return {
    case = case,
    match = match,
    match_all = match_all,
    match_nomt = match_nomt,
    match_all_nomt = match_all_nomt,
    match_cond = match_cond,
    DO = DO,
    call = call,
    var = var,
    is_var = isVar
}
