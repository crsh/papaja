--[[
  Parse metadata fields and append them to the document body

  Contains adapted code from
  https://github.com/pandoc/lua-filters/blob/master/author-info-blocks/author-info-blocks.lua
  Copyright © 2017–2020 Albert Krewinkel
  ]]

local List = require 'pandoc.List'

local function intercalate(lists, elem)
  local result = List:new{}
  for i = 1, (#lists - 1) do
    result:extend(lists[i])
    result:extend(elem)
  end
  if #lists > 0 then
    result:extend(lists[#lists])
  end
  return result
end

--- Create inlines for a single author (includes all author notes)
local equal_contributor_mark = FORMAT == "latex"
  and {pandoc.RawInline("latex", "$\\dagger{}$")}
  or {pandoc.Str("*")}

local function author_inline_generator(sups)
  if not sups then
    return function (author)
      return List.clone(author.name)
    end
  else
    return function (author)
      local author_marks = List:new{}

      if author.equal_contrib then
        author_marks[#author_marks + 1] = equal_contributor_mark
      end

      if author.affiliation then
        local idx_str = pandoc.utils.stringify(author.affiliation)
        author_marks[#author_marks + 1] = {pandoc.Str(idx_str)}
      end

      -- if is_corresponding_author(author) then
      --   author_marks[#author_marks + 1] = get_mark 'corresponding_author'
      -- end

      local res = List.clone(author.name)
      res[#res + 1] = pandoc.Superscript(intercalate(author_marks, {pandoc.Str ","}))

      return res
    end
  end
end

--- Generate a list of inlines containing all authors.
local function create_authors_inlines(authors, and_str, sups)
  local padded_and_str = List:new{pandoc.Space(), pandoc.Str (and_str), pandoc.Space()}

  local inlines_generator = author_inline_generator(sups)
  local inlines = List:new(authors):map(inlines_generator)
  local last_author = inlines[#inlines]
  inlines[#inlines] = nil

  local result
  if #authors > 2 then
    result = intercalate(inlines, {pandoc.Str ",", pandoc.Space()})
    result:extend(List:new{pandoc.Str ","})
  else
    result = intercalate(inlines, {})
  end

  if #authors > 1 then
    result:extend(padded_and_str)
  end

  result:extend(last_author)

  return result
end


--- Check whether the given author is a corresponding author
local function is_corresponding_author(author)
  return author.corresponding and author.address and author.email
end

--- Generate a block element containing the correspondence information for the
--- author note
local function create_correspondence_blocks(authors)
  local corresponding_authors = List:new{}
  local equal_contributors = {table.unpack(authors)}

  for i, author in ipairs(authors) do
    if is_corresponding_author(author) then
      contact_info = List:new(
        author.name .. List:new{pandoc.Str ",", pandoc.Space()} ..
        author.address ..
        List:new{pandoc.Str ".", pandoc.Space(), pandoc.Str "E-mail:", pandoc.Space()} ..
        author.email
      )
      table.insert(corresponding_authors, {pandoc.Str(contact_info)})
    end
    if not authors[i].equal_contrib then
      table.remove(equal_contributors, i)
    end
  end

  local equal_contributor_inline = #equal_contributors < # authors
    and create_authors_inlines(equal_contributors, "and", false)
    or List:new{pandoc.Str"All", pandoc.Space(), pandoc.Str"authors"}

  if #corresponding_authors == 0 and #equal_contributors == 0 then
    return nil
  end

  local test = List:new{
    pandoc.Superscript(equal_contributor_mark[1]), pandoc.Space()
  }

  local contributor_line = List:new{
    pandoc.Space(), pandoc.Str"contributed", pandoc.Space(), pandoc.Str"equally",
    pandoc.Space(), pandoc.Str"to", pandoc.Space(), pandoc.Str"this", pandoc.Space(),
    pandoc.Str"work", pandoc.Str".", pandoc.Space()
  }

  local correspondence_line = List:new{
    pandoc.Str"Correspondence", pandoc.Space(), pandoc.Str"concerning", pandoc.Space(),
    pandoc.Str"this", pandoc.Space(), pandoc.Str"article", pandoc.Space(),
    pandoc.Str"should", pandoc.Space(), pandoc.Str"be", pandoc.Space(),
    pandoc.Str"addressed", pandoc.Space(), pandoc.Str"to", pandoc.Space()
  }

  return pandoc.Para(test .. equal_contributor_inline .. contributor_line .. correspondence_line .. contact_info)
end


--- Create raw LaTeX environments from metadata fields
local function make_latex_envir(name, metadata)
  local data = {table.unpack(metadata)}
  local pandoc_type = data[1].t

  if pandoc_type == "Str" or pandoc_type == "RawInline" then
    return List:new{pandoc.Para({pandoc.RawInline("latex", "\\" .. name), pandoc.Span(data)})}
  end
  if pandoc_type == "Para" or pandoc_type == "Plain" then
    local envir = List:new{pandoc.Para(pandoc.RawInline("latex", "\\" .. name .. "{"))}
    envir:extend(data)
    envir:extend(List:new{pandoc.Para(pandoc.RawInline("latex", "}"))})
    return envir
  end
end

function Pandoc (document)

  if document.meta.note ~= nil or document.meta.authornote ~= nil then
    document.blocks:extend(List:new{pandoc.Para(pandoc.RawInline("latex", "% papaja Lua-filter additions"))})
  end

  if document.meta.note ~= nil then
    document.blocks:extend(make_latex_envir("note", document.meta.note))
  end

  if document.meta.authornote ~= nil then
    if document.meta.author ~= nil then
      table.insert(document.meta.authornote, create_correspondence_blocks(document.meta.author))
    end
    document.blocks:extend(make_latex_envir("authornote", document.meta.authornote))
  end

  if document.meta.note ~= nil or document.meta.authornote ~= nil then
    document.blocks:extend(List:new{pandoc.Para(pandoc.RawInline("latex", "% End of papaja Lua-filter additions"))})
  end

  if document.meta.author ~= nil then
    document.meta.author = pandoc.MetaInlines(create_authors_inlines(document.meta.author, "&", true))
  else
    document.meta.author = FORMAT == "latex"
      and pandoc.MetaInlines({pandoc.RawInline("latex", "\\phantom{0}")})
      or nil
  end

  return document
end


