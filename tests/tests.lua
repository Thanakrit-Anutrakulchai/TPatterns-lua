local assert, type = assert, type
local utils = require 'tests.utils'
local tostring, equal = utils.tostring, utils.equal

local function test(expected, got)
  assert(equal(expected, got), 'Expected '..tostring(expected)..'; Got '..tostring(got))
end

local Pair = {__eq = function(a, b) return (a.fst == b.fst) and (a.snd == b.snd) end}
local function pair(x, y)
  return setmetatable({fst = x, snd = y}, Pair)
end

local Point = {__tostring = function(self)
    return table.concat{'Point','(', tostring(self.x), ', ', tostring(self.y), ')'}
  end
}
local function point(x, y)
  return setmetatable({x = x, y = y}, Point)
end

local T = require 'TPatterns'
local case, var, call, DO = T.case, T.var, T.call, T.DO
local match, match_nomt, match_all = T.match, T.match_nomt, T.match_all
local match_all_nomt, match_cond = T.match_all_nomt, T.match_cond

test( true,
  match (1) { case(1) - 'true' }
)

test( 12,
  match {2, 4, 6, 8} { case{2, var'x', 6, var'y'} - 'x+y', case{1, 3, 5, var'z'} - 'z' }
)

test( 'Yep',
  match { case(3) - ' "Nope" ', case(5) - ' "Yep" ', case{'banana'} - ' "Wut" ' } (5)
)

test( 3.14,
  match {pi = 3.14, deg = 60, lucky = 7} { case{pi = var'x', deg = 60} - 'x' }
)

test( nil,
  match_all {pi = 3.14, deg = 60, lucky = 7} { case{pi = var'x', deg = 60} - 'x' }
)

test( {dish = 8, pie = 7},
  match {3, 5, {pie = 7, dish = 8}} { case{3, 5, var'x'} - 'x' }
)

test( 15,
  match {3, 5, {7, pie = 8}} { case{3, 5, {var'x', pie = var'y'}} - 'x + y' }
)

test( 12,
  match_all (point(3, 4)) {
    case(pair(var'x', var'y')) - 'x + y',
    case(point(var'x', var'y')) - 'x * y'
  }
)

test( point(1, 2),
  match (point(1, 2)) { case(var'x') - 'x' }
)

test( 3,
  match { case{a = var'x', b = var'y', c = var'z'} - 'z', case{a = var'x'} - 'x' }
    { a = 3, b = 5 }
)

test( 16,
  match {a = 1, b = 3, c = 5, d = 7} {
    case{a = var's', b = var't', c = var'u', d = var'v'} - function(t)
      return t.s + t.t + t.u + t.v
    end
  }
)

test( 16,
  match_cond (function(x) return (type(x) ~= 'number' and true) or x > 3 end) (pair(4, 7)) {
    case(pair(var'x', var'y')) - call(function(a, b, c) return a + b + c end, var'x', var'y', 5),
    case(pair(4, var'z')) - DO('double(z)',{double = function(x) return x*2 end} )
  }
)

test( false,
  match_all {pie = true, apple = true, banana = false} {
    case{pie = var'x', apple = true} - 'x',
    case{pie = true, apple = var'_', banana = var'x'} - DO("id(x)", {id = function(e) return e end})
  }
)

test( point(2, point(3, 4)),
  match_cond (function(v) return (type(v) ~= 'number' and true) or v > 0 end) {2, point(3, 4)} {
    case{2, pair(var'x', var'y')} - 'x+y',
    case{var'x', var'z'} - function(t) return point(t.x, t.z) end
  }
)

test( 10,
  match {2, {function(x) return x * 2 end, 5}, 'hi'}
    { case{2, var'x'} - 'false', case{2, {var'doub', 5}, 'hi'} - ' doub(5) ' }
)

test( 'number',
  match_all {1, 3, a = 5, b = 7}
    { case{var'_', var'_', a = var'_', b = var'x'} - DO('type(x)', {}) }
)

test( nil,
  match {1, 2} { case{var'x', var'x'} - '3', case{var'x', var'_', var'_', var'_'} - '5' }
)

test( 2,
  match_all (pair(3, 4)) { case{fst = 3, snd = 4} - '1', case(pair(3, 4)) - '2' }
)

test( 138,
  match_nomt (point(1, 2)) {
    case{ x = var'x', y = var'y' } - 'x+y+135',
    case(point(1, 2)) - '1'
  }
)

test( 142,
  match_all_nomt (pair('a', 'b')) {
    case{fst = var'x'} - 'x',
    case{fst = var'x', snd = var'y'} - 'x:byte() + y:byte() - 53'
  }
)

test( true,
  match {2, 4, 6, {8, 10}} { case{var'x', var'_', var'z', {var'_', var'_'}} - '_ == nil' }
)

test( true,
  match (25) ( case (25) - 'true')
)
