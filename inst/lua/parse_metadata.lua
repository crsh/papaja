--[[
  Parse metadata fields and append them to the document body

  Contains some adapted code from
  https://github.com/pandoc/lua-filters/blob/master/author-info-blocks/author-info-blocks.lua
  Copyright © 2017–2020 Albert Krewinkel
  ]]

local List = require 'pandoc.List'

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

--- Check whether the given author is a corresponding author
local function is_corresponding_author(author)
  return author.corresponding and author.address and author.email
end

--- Generate a block element containing the correspondence information
local function create_correspondence_blocks(author)
  local corresponding_authors = List:new{}
  for _, author in ipairs(author) do
    if is_corresponding_author(author) then
      contact_info = List:new(
        author.name .. List:new{pandoc.Str ",", pandoc.Space()} ..
        author.address ..
        List:new{pandoc.Str ".", pandoc.Space(), pandoc.Str "E-mail:", pandoc.Space()} ..
        author.email
      )
      table.insert(corresponding_authors, {pandoc.Str(contact_info)})
    end
  end

  if #corresponding_authors == 0 then
    return nil
  end
  local correspondence = List:new{
    pandoc.Str"Correspondence", pandoc.Space(), pandoc.Str"concerning", pandoc.Space(),
    pandoc.Str"this", pandoc.Space(), pandoc.Str"article", pandoc.Space(),
    pandoc.Str"should", pandoc.Space(), pandoc.Str"be", pandoc.Space(),
    pandoc.Str"addressed", pandoc.Space(), pandoc.Str"to", pandoc.Space()
  }

  return pandoc.Para(correspondence .. contact_info)
end


function Pandoc (document)

  if document.meta.note ~= nil or document.meta.authornote ~= nil then
    document.blocks:extend(List:new{pandoc.Para(pandoc.RawInline("latex", "% papaja Lua-filter additions"))})
  end

  if document.meta.note ~= nil then
    document.blocks:extend(make_latex_envir("note", document.meta.note))
  end

  if document.meta.authornote ~= nil then
    table.insert(document.meta.authornote, create_correspondence_blocks(document.meta.author))
    document.blocks:extend(make_latex_envir("authornote", document.meta.authornote))
  end

  return document
end


