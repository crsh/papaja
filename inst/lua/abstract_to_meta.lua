--[[
  https://github.com/pandoc/lua-filters/blob/master/abstract-to-meta/abstract-to-meta.lua

  Turns section with ID "abstract" into metadata element
  ]]

local looking_at_abstract = false
local abstract = {}

local looking_at_authornote = false
local authornote = {}

function Block (elem)
  if looking_at_abstract then
    abstract[#abstract + 1] = elem
    return {}
  end
  if looking_at_authornote then
    authornote[#authornote + 1] = elem
    return {}
  end
end

function HorizontalRule (elem)
  if looking_at_abstract then
    looking_at_abstract = false
    return {}
  end
  if looking_at_authornote then
    looking_at_authornote = false
    return {}
  end
end

function Header (elem)
  if elem.level == 1 and elem.identifier == 'abstract' then
    looking_at_abstract = true
    return {}
  else
    looking_at_abstract = looking_at_abstract and elem.level ~= 1
  end
  if elem.level == 1 and elem.identifier == 'authornote' then
    looking_at_authornote = true
    return {}
  else
    looking_at_authornote = looking_at_authornote and elem.level ~= 1
  end
  if elem.level == 1 and elem.identifier == 'title' then
    return {}
  end
end

function Meta (meta)
  meta.abstract = meta.abstract or pandoc.MetaBlocks(abstract)
  meta.authornote = meta.authornote or pandoc.MetaBlocks(authornote)
  return meta
end
