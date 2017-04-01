-- Examples of using Table Pattern Matching in Lua.
local sm, gm, select, tostring, assert = setmetatable, getmetatable, select, tostring, assert
local T = require 'TPatterns'
local case, var, call, DO = T.case, T.var, T.call, T.DO 
local match, match_all, match_cond = T.match, T.match_all, T.match_cond
local match_nomt, match_all_nomt = T.match_nomt, T.match_all_nomt

local mempty
local List = { __tostring = function(self)  
                              local accu = '[ '
                              while self ~= mempty do
                                accu = accu..tostring(self.head)..', '
                                self = self.tail
                              end
                              if accu == '[ ' then return '[]' end
                              return accu:sub(1, -3) .. ' ]'
                            end }
mempty = sm({}, List)
local function cons(x, xs)
  assert(gm(xs) == List, 'cons> Second argument must be a list!')
  return sm( {head = x, tail = xs}, List )
end
local function list(...)
  if select('#', ...) == 0 then return mempty end
  return cons( (...), list(select(2, ...)) ) 
end

local testList = list(1, 3, 'hi', function() return 0 end, {c = 'pie'}, 9)
local testList2 = list(999, '888', 777, '666', 555, '444', {})
-- Functional Declarations, assume List is from another module with only `cons` exported.
local head = match_nomt { case{ head = var'x', tail = var'xs' } - 'x', case( mempty ) - ' "Throw Error Here" ' }
local tail = match_nomt { case( {head = var'x', tail = var'xs'} ) - 'xs', case( mempty ) - ' "Throw Error Here" ' }

local append;
append = match_all_nomt { 
                          case( mempty, var'ys' ) - 'ys',
                          case({head = var'x', tail = var'xs'}, var'ys') - function(t) return cons(t.x, append(t.xs, t.ys)) end 
                        }


assert( head(testList) == 1 )
assert( head(mempty) == 'Throw Error Here' )

assert( tail(testList) == testList.tail )
assert( tail(mempty) == 'Throw Error Here' )

print(append( testList, testList2 ))
