--[==[  ****THIS MODULE CURRENTLY SUPPORTS LUA 5.1 ONLY!
    A module that brings pattern matching from functional programming languages into Lua's tables.
    This module relies heavily on (one-sided) unification. This file refers to a 'substitution hash'
    which is just a table with keys being "variable" names and values as the values they represent.
    
    10 functions are exported: case, var, is_var, call, DO, 
                                match, match_all, match_nomt, match_all_nomt and match_cond
    
    
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
         
  ]==]

-- [ INIT ]
local sm, gm, ipairs, pairs = setmetatable, getmetatable, ipairs, pairs
local loadstring, type, assert, tostring = loadstring, type, assert, tostring
local error, setfenv, unpack, select = error, setfenv, unpack, select
local walk, isVar

local function declareVars(obj)
  if isVar(obj) then
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

local doMT = {}
local function DO(str, env)
  return sm({str, env}, doMT)
end

local callMT = {}
local function call(f, ...)
  return sm({func = f, args = {...}}, callMT)
end

local from_G = {__index = _G}
local caseMT = {getFunc =   function(self)
                local expr = self[2]
                if gm(expr) == doMT then
                  local header = declareVars(self[1])
                  local f = loadstring( header .. 'return '..expr[1] )
                  local env = expr[2]
                  if gm(env) then
                    return setfenv(f, env)
                  else
                    return setfenv(f, sm(env, from_G))
                  end
                elseif gm(expr) == callMT then
                    return function(subst)
                      local accu = {}
                      for _, v in ipairs(expr.args) do
                        if isVar(v) then accu[#accu+1] = walk(v, subst)
                        else accu[#accu+1] = v
                        end
                      end
                      accu[#accu+1] = subst
                      return expr.func(unpack(accu))
                    end
                else error("match> 'DO' or 'call' block expected.", 2)
                end
              end
  } ; caseMT.__index = caseMT
local halfcaseMT = {__sub = function(self, expr) 
                              if type(expr) == 'string' then return sm({self[1], DO(expr, {})}, caseMT) end
                              if type(expr) == 'function' then return sm({self[1], call(expr)}, caseMT) end
                              return sm({self[1], expr}, caseMT) 
                            end}
local varMT = {__tostring = function(self) return '$('..self.name..')' end}

-- [ FUNCTIONS ]
isVar = function(x) return gm(x) == varMT end --forward declared

--Create a custom "variable" with the given name
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
    return (res~=nil and res) or v 
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
                            

-- Attempts to unify the case and the obj(ect)
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


local function case(obj, ...)
  return select('#', ...)==0 and sm({obj}, halfcaseMT) or sm({ {obj, ...} }, halfcaseMT)
end

local function match_cond(constraint)
  return function(a, ...) local x = (select('#', ...) == 0 and a) or {a, ...} ;
  return function(b, ...) local y = (select('#', ...) == 0 and b) or {b, ...} ;
    local obj, cases
    assert(type(x) == 'table' or type(y) == 'table', 'match> an array of Cases must be given!')
    if type(y) == 'table' and #y > 0 and gm(y[1]) == caseMT then obj = x; cases = y
    else obj = y; cases = x
    end
    for _, c in ipairs(cases) do
      local subst = unify(c[1], obj, constraint)
      if subst then return (c:getFunc())(subst) end
    end
  end
  end
end

local match_nomt = match_cond(function() return true end)


local function checkMt(vals, vars)
  if type(vals) == 'table' and type(vars) == 'table' and not isVar(vars) then
    return gm(vals) == gm(vars)
  else
    return true
  end
end
local match = match_cond(checkMt)

local function size(t)
  local accu = 0
  for _, _ in pairs(t) do accu = accu + 1 end
  return accu
end
local function checkSize(vals, vars)
  if type(vals) == 'table' and type(vars) == 'table' and not isVar(vars) then
    return size(vals) == size(vars)
  else
    return true
  end 
end
local match_all_nomt = match_cond(checkSize)
local match_all = match_cond(function(x, y) return checkSize(x, y) and checkMt(x, y) end)


-- [ MODULE TEST/EXPORT ]

return {case = case, 
        match = match, 
        match_all = match_all, 
        match_nomt = match_nomt,
        match_all_nomt = match_all_nomt,
        match_cond = match_cond, 
        DO = DO,
        call = call,
        var = var,
        is_var = isVar}
