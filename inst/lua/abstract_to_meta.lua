--[[
  https://github.com/pandoc/lua-filters/blob/master/abstract-to-meta/abstract-to-meta.lua

  Turns section with ID "abstract" into metadata element
  ]]
local looking_at_abstract = false
local abstract = {}

function Block (elem)
  if looking_at_abstract then
    abstract[#abstract + 1] = elem
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
end

function Meta (meta)
  meta.abstract = meta.abstract or pandoc.MetaBlocks(abstract)
  return meta
end
