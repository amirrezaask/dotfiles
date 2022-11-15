local __ngrams_cache = {}
local quicksort = function(arr, l, h)
  local function partition(t, low, high)
    local i = (low - 1)
    local pivot = t[high]

    for j = low, high - 1 do
      if t[j].score >= pivot.score then
        i = i + 1
        t[i], t[j] = t[j], t[i]
      end
    end
    t[i + 1], t[high] = t[high], t[i + 1]
    return (i + 1)
  end

  local function table_with_size(size, default_value)
    local t = {}
    for i = 1, size do
      table.insert(t, default_value)
    end
    return t
  end

  local size = h - l + 1
  local stack = table_with_size(size, 0)

  local top = -1
  top = top + 1
  stack[top] = l
  top = top + 1
  stack[top] = h

  while top >= 0 do
    h = stack[top]
    top = top - 1
    l = stack[top]
    top = top - 1
    local p = partition(arr, l, h)
    if p - 1 > l then
      top = top + 1
      stack[top] = l
      top = top + 1
      stack[top] = p - 1
    end
    if p + 1 < h then
      top = top + 1
      stack[top] = p + 1
      top = top + 1
      stack[top] = h
    end
  end
  return arr
end

local function ngrams_of(s, n)
  n = n or 3
  local ngrams = __ngrams_cache[s] or {}
  if #vim.split(s, " ") > 1 then
    local words = vim.split(s, " ")
    for _, w in ipairs(words) do
      if __ngrams_cache[w] == nil then
        __ngrams_cache[w] = ngrams_of(w)
      end
      for _, c in ipairs(__ngrams_cache[w]) do
        table.insert(ngrams, c)
      end
    end
  end
  if #ngrams > 0 then
    return ngrams
  end
  for i = 1, #s do
    local last = i + n - 1
    if last >= #s then
      last = #s
    end
    local this_ngram = string.sub(s, i, last)
    table.insert(ngrams, this_ngram)
  end
  __ngrams_cache[s] = ngrams
  return ngrams
end

local __cache_score = {}

local function levenshtein_distance(str1, str2)
  if str1 == str2 then
    return 0
  end
  if #str1 == 0 then
    return #str2
  end
  if #str2 == 0 then
    return #str1
  end
  if __cache_score[str1 .. str2] ~= nil then
    return __cache_score[str1 .. str2]
  end
  if str1 == str2 then
    return 0
  end
  if str1:len() == 0 then
    return str2:len()
  end
  if str2:len() == 0 then
    return str1:len()
  end
  if str1:len() < str2:len() then
    str1, str2 = str2, str1
  end

  local t = {}
  for i = 1, #str1 + 1 do
    t[i] = { i - 1 }
  end

  for i = 1, #str2 + 1 do
    t[1][i] = i - 1
  end
  local function min(a, b, c)
    local min_val = a
    if b < min_val then
      min_val = b
    end
    if c < min_val then
      min_val = c
    end
    return min_val
  end
  local cost
  for i = 2, #str1 + 1 do
    for j = 2, #str2 + 1 do
      cost = (str1:sub(i - 1, i - 1) == str2:sub(j - 1, j - 1) and 0) or 1
      t[i][j] = min(t[i - 1][j] + 1, t[i][j - 1] + 1, t[i - 1][j - 1] + cost)
    end
  end
  __cache_score[str1 .. str2] = t[#str1 + 1][#str2 + 1]
  return t[#str1 + 1][#str2 + 1]
end

local function sort(list)
  list = quicksort(list, 1, #list)
  return list
end

local function match(query, collection)
  local list = {}
  for i = 1, #collection do
    if not (query == nil or collection[i] == nil) then
      local ngrams_data = ngrams_of(string.gsub(collection[i], " ", ""), 3)
      local ngrams_query = ngrams_of(query:gsub(" ", ""), 3)
      local total = 0
      for _, nq in ipairs(ngrams_query) do
        local min_distance_of_ngrams = 100000
        for _, nd in ipairs(ngrams_data) do
          local distance = levenshtein_distance(string.lower(nq), string.lower(nd))
          if distance < min_distance_of_ngrams then
            min_distance_of_ngrams = distance
          end
        end
        total = total + min_distance_of_ngrams
      end
      local word_score = { score = total, word = collection[i] }
      table.insert(list, word_score)
    end
  end
  local output = {}
  list = sort(list)
  for _, v in ipairs(list) do
    table.insert(output, v.word)
  end
  return output
end

return {
  match = match,
  clean = function()
    __cache_score = {}
    __ngrams_cache = {}
  end,
}
