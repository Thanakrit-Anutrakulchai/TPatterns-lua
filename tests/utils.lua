local old_tostring, type, pairs, gm = tostring, type, pairs, getmetatable

-- pretty print for tables
local function tostring(x)
  if type(x) == 'table' then
    if gm(x) and gm(x).__tostring then return gm(x).__tostring(x) end
    local accu = '{ '
    for k, v in pairs(x) do
      accu = accu .. '['..tostring(k)..']' .. ' = ' .. tostring(v) .. ', '
    end
    if accu == '{ ' then return '{}' end
    return accu:sub(1, -3) .. ' }'
  elseif type(x) == 'string' then
    return '"'..old_tostring(x)..'"'
  else
    return old_tostring(x)
  end
end

local function size(t)
  local accu = 0
  for _, _ in pairs(t) do
    accu = accu + 1
  end
  return accu
end

-- equality check using the __eq metamethod or, failing that, via structural equality
local function equal(x, y)
  if type(x) == 'table' and type(y) == 'table' then
    if size(x) ~= size(y) then return false end
    if gm(x) or gm(y) then
      if gm(x) ~= gm(y) then return false end
      if gm(x) and gm(x).__eq then
        return gm(x).__eq(x, y)
      elseif gm(y) and gm(y).__eq then
        return gm(y).__eq(y, x)
      end
    end

    for k, v in pairs(x) do
      if not equal(v, y[k]) then return false end
    end
    return true
  else
    return x == y
  end
end

return { tostring = tostring, size = size, equal = equal }
