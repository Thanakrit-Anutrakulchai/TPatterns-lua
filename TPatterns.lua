--[==[  ****THIS MODULE CURRENTLY SUPPORTS LUA 5.1 ONLY!
    A module that brings pattern matching in the style of functional programming languages to Lua's tables.
    This module relies heavily on (one-sided) unification.

    This file refers to a *substitution hash* which is just a table with keys being *variable* names
    and *values* as the values they represent. If the value is nil, then it is a "free variable" and
    may later be bound to a more concrete value. The value can also be another variable, which will
    usually be further looked up if it is bound.

    Find examples in 'tests.lua' and 'examples.lua'.
    Further documentation is in the README.md.
  ]==]

-- [ INIT ]
local sm, gm, ipairs, pairs = setmetatable, getmetatable, ipairs, pairs
local loadstring, type, assert = loadstring, type, assert
local error, setfenv, unpack, select = error, setfenv, unpack, select
local walk, isVar, unify_helper, unify_table


-- Generates a string that will assign values to (all) the variable(s) provided
--  based on the substitution hash once loaded and ran as a function.
---@param obj any Object whose contained variables will be declared
---@return string declarations
local function declareVars(obj)
  if isVar(obj) then
    -- the (...) argument refers to the substitution hash
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

---@alias Environment table

---@class DoBlock
---@field [1] string Code to execute
---@field [2] Environment Environment to execute code in

local doMT = {}
-- Wraps a string to be load(string)ed and an environment for the resulting 
-- function to look up with.
--
-- NOTE: If env does not have a metatable, it will be set to from\_G = {\_\_index = \_G}.
--        Also, in the expression, access the substitution hash with (...).
---@param expr string Code to execute
---@param env Environment Environment to execute code in. Defaults to \_G.
---@return DoBlock
local function DO(expr, env)
  return sm({expr, env}, doMT)
end

---@class CallBlock
---@field func function|table The function or table to be called
---@field args table An array of arguments to be passed to the function

local callMT = {}
-- Stores a function or callable table with its arguments, which may be variables.
-- Variables are looked up in the substitution hash when the function is called.
--
-- NOTE: Final parameter to the function 'f' is the substitution hash!
---@param f function|table Function or table to be called with remaining arguments and hash
---@param ... any Arguments to pass to callable, along with the hash
---@return CallBlock
local function call(f, ...)
  return sm({func = f, args = {...}}, callMT)
end

-- Metatable for environments extending the global environment
local from_G = {__index = _G}

---@class CaseBlock
---@field [1] Value|Variable
---@field [2] DoBlock|CallBlock
---@field getFunc function

-- Metatable for the completed case, something of the form case(...) - DO/call(...).
local caseMT = {}
caseMT.__index = caseMT

---@param self CaseBlock
---@return fun(subst : SubstHash) : any theFunction ...which evaluates the DO/call block
function caseMT.getFunc(self)
  local expr = self[2] --extract rhs

  -- if rhs is a DO block                        
  if gm(expr) == doMT then
    -- append the str expression with all variables being pre-bound 
    --   to their corresponding values inside the given hash (first arg.)
    local header = declareVars(self[1])

    -- Generate the function with the given env
    local f = loadstring( header .. 'return '..expr[1] )

    -- blame user on failure to load function
    if f == nil then
      error("match> Failed to parse expression given in string/DO block.", 2)
    end

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

---@class HalfCaseBlock
---@field [1] Value|Variable
---@operator sub : CaseBlock

-- Metatable for generating cases
local halfcaseMT = {}

---@param self HalfCaseBlock
---@param expr DoBlock|CallBlock|string|function
---@return CaseBlock
function halfcaseMT.__sub(self, expr)
  -- Syntatic sugars. case(...) - "~expr~" = case(...) - DO("~expr~", {})
  --  and case(...) - function(hash) ~expr~ end = case(...) - call(function(hash) ~expr~ end)
  if type(expr) == 'string' then return sm({self[1], DO(expr, {})}, caseMT) end
  if type(expr) == 'function' then return sm({self[1], call(expr)}, caseMT) end
  return sm({self[1], expr}, caseMT)
end

---@class Variable
---@field name string

-- These aliases are for documentation purposes only.
---@alias Value any
---@alias SubstHash table<string, Value|Variable>
---@alias Constraint fun(v : Value, u : Value|Variable, s : SubstHash) : any
---@alias Matcher function
---@alias PartiallyAppliedMatcher function


-- Metatable for variables                                    
local varMT = {__tostring = function(self) return '$('..self.name..')' end}

-- [ FUNCTIONS ]

-- Checks whether the argument is a variable.
---@return boolean
isVar = function(x) return gm(x) == varMT end

-- Create a custom variable with the given name.
---@param name string The name of the variable to return
---@return Variable
local function var(name)
  return sm({name = name}, varMT)
end

-- Look up the ultimate value of a variable in the substitution hash.
-- i.e. return subst[v.name] or subst[subst[v.name]] or ... 
--      until it reaches a value or a free variable z such that subst[z.name] is nil.
---@param v Value|Variable The variable to lookup, or value to return
---@param subst SubstHash
---@return Value|Variable
function walk(v, subst)
  if not isVar(v) then
    return v
  else
    ---@cast v Variable
    local res = walk(subst[v.name], subst)

    -- returns the end result of the look-up chain, whether it's a variable or value
    if res ~= nil then
      return res
    else
      return v
    end
  end
end


-- Attempt to unify two tables structurally, i.e. without regards to __eq.
---@param tvals table
---@param tvars table<any, Variable>
---@param subst SubstHash
---@param constraint fun(v : Value, u : Value|Variable, s : SubstHash) : any
---@return nil|SubstHash
function unify_table(tvals, tvars, subst, constraint)
  -- Check array part
  -- If their arrays don't have the same size, it fails to unify
  -- This is finicky because of how # works in Lua...
  if #tvars > 0 and #tvars ~= #tvals then
    return nil
  end

  -- Check hash part, make sure all elements at each key *of only u* can be unified                                
  for k, e in pairs(tvars) do
    local r = tvals[k]
    -- Fails if case has a key not in obj                                  
    if r == nil then
      return nil
    end

    local subst_or_nil = unify_helper(e, r, subst, constraint)
    if not subst_or_nil then return nil end
    subst = subst_or_nil
  end

  -- If nothing fails, passes, binding necessary inner-variables to values                                
  return subst
end

-- Helper function for unification.
---@see unify
---@param subst SubstHash
---@param constraint fun(v : Value, u : Value|Variable, s : SubstHash) : any
---@return nil|SubstHash
function unify_helper(x, y, subst, constraint)
  assert(not isVar(y), 'match> There can only be variables in the cases!')

  -- Look up the value or free variable ultimately associated with x in subst
  local u = walk(x, subst)

  -- Check constraint given                              
  if not constraint(y, u, subst) then return nil end

  -- With "_", always passes ("unification" succeeds, nothing new is bound)                              
  if isVar(u) and u.name == '_' then return subst end

  -- If u is a (necessarily free!) variable then bind it
  if isVar(u) then
    subst[u.name] = y
    return subst
  end

  -- If they're both tables, check inside contents     
  local subst_or_nil
  if type(u) == 'table' and type(y) == 'table' then
    subst_or_nil = unify_table(y, u, subst, constraint)
  end

  if subst_or_nil then
      return subst_or_nil
  -- If unification failed or one side is not a table, check for equality
  elseif y == u then
      return subst
  end
  -- If they aren't both tables and they're not equal, unification fails
end


-- Attempts one-sided unification.
-- Unification succeeds if the constraint holds *and* case == obj or case is a (free) variable or
-- both are tables and the values at each key *of the case* can be unified, 
--                 and if case has an array part, values at each indexes can be unified.
--                 (While remembering all already-bound variables, given the same constraint.)
--
-- The constraint is called each time a unification attempt is made.
-- It is passed the two values, or the value and free variable, 
-- involved in the unification, and the substitution hash.
-- It must always return true (or a truthy value) for unification to succeed.
---@param constraint fun(v : Value, u : Value|Variable, s : SubstHash) : any
---@return nil|table substitutions The variables substitutions made during unification.
local function unify(case, obj, constraint)
  return unify_helper(case, obj, {}, constraint)
end

-- Wraps arguments into half of a case block.
-- Needs other half (right-side of '-') to be completed.
-- case(a1, a2, ...) is syntactic sugar for case({a1, a2, ...}).
---@return HalfCaseBlock
local function case(obj, ...)
  return select('#', ...)==0 and sm({obj}, halfcaseMT) or sm({ {obj, ...} }, halfcaseMT)
end

-- Usage: `match\_cond (constraint) (obj) (cases)` where cases is an array of cases.
-- Alternatively: `match\_cond (constraint) (cases) (obj)`,
--   where `obj` is an *empty* table or a non-table.
--
-- `match\_cond (constraint) (o1, o2, o3, ...) (c1, c2, c3, ...)` is nearly 
-- syntatic sugar for:
-- ```
--   match_cond (constraint) {o1, o2, o3, ...} {c1, c2, c3, ...}
-- ```
-- but with the former, the i-th index of `(...)` in a DO block or the final
-- argument in a call block is the i-th object argument. With the latter,
-- the 1st index is the entire table `{o1, ...}`, while the i-th index
-- for `i>1` is `nil`. In either case, the 0th index is the entire table `{o1, ...}`.
---@param constraint fun(v : Value, u : Value|Variable, s : SubstHash) : any
---@return Matcher
local function match_cond(constraint)
  -- A matcher for pattern matching on an object
  ---@return PartiallyAppliedMatcher
  return function(a, ...)
    local x = (select('#', ...) > 0 and {a, ...}) or (gm(a) == caseMT and {a}) or a
    local arg1 = {...} -- couldn't get it to work with `arg` and I'm not sure why?
    -- A partially applied matcher
    return function(b, ...)
      local y = (select('#', ...) > 0 and {b, ...}) or (gm(b) == caseMT and {b}) or b
      -- check that cases is (probably) given and assign appropriate sets of values to obj/cases
      local obj, cases, firstObj, restObj
      assert(type(x) == 'table' or type(y) == 'table', 'match> An array of cases must be given!')
      if type(y) == 'table' and #y > 0 and gm(y[1]) == caseMT then
        obj = x; cases = y; firstObj = a; restObj = arg1
      else
        obj = y; cases = x; firstObj = b; restObj = {...}
      end

      -- Attempt to unify (under constraint) each case based on index until it succeeds
      --  Then, calls first successful case's function/expression with the substition hash
      for _, c in ipairs(cases) do
        local subst = unify(c[1], obj, constraint)
        if subst then
          -- add object arguments to substitution hash passed in
          subst[0] = obj
          -- using pairs instead of ipairs so passed nils do not hide keys
          subst[1] = firstObj
          if restObj then
            for i, v in pairs(restObj) do
              subst[i+1] = v
            end
          end
          return (c:getFunc())(subst)
        end
      end
    end
  end
end

-- Checks that all (sub-)tables have the same metatable.
-- Non-tables pass the constraint automatically
---@param vals Value
---@param vars Value|Variable
---@return boolean
local function checkMt(vals, vars)
  if type(vals) == 'table' and type(vars) == 'table' and not isVar(vars) then
    return gm(vals) == gm(vars)
  else
    return true
  end
end


-- Counts the number of keys in a table.
---@param t table The table whose keys are counted
---@return integer numKeys The number of keys according to `pairs`
local function size(t)
  local accu = 0
  for _, _ in pairs(t) do accu = accu + 1 end
  return accu
end

-- Ensures all (sub-)tables have the same sizes.
-- When the (sub-)tables of one argument contains keys only belonging to the
-- corresponding (sub-)tables in the other argument, this is enough to ensure
-- that the tables have the same keys.
---@param vals Value
---@param vars Value|Variable
---@return boolean
local function checkSize(vals, vars)
  if type(vals) == 'table' and type(vars) == 'table' and not isVar(vars) then
    return size(vals) == size(vars)
  else
    return true
  end
end

-- A matcher not ensuring keys or metatables match.
local match_nomt = match_cond(function() return true end)

-- The default matcher, which unifies ensuring all metatables match.
local match = match_cond(checkMt)

-- A matcher which unifies ensuring all keys match.
local match_all_nomt = match_cond(checkSize)

-- A matcher which unifies ensuring all keys and metatables match.
local match_all = match_cond(function(x, y) return checkSize(x, y) and checkMt(x, y) end)


-- [ MODULE EXPORT ]

return {
    DO = DO,
    call = call,
    case = case,
    is_var = isVar,
    match = match,
    match_all = match_all,
    match_all_nomt = match_all_nomt,
    match_cond = match_cond,
    match_nomt = match_nomt,
    var = var
}
