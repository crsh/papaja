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

      local res = List.clone(author.name)
      res[#res + 1] = pandoc.Superscript(intercalate(author_marks, {pandoc.Str ","}))

      return res
    end
  end
end

--- Generate a list of inlines containing all authors
local function create_authors_inlines(authors, and_str, sups)
  local padded_and_str = List:new{pandoc.Space(), pandoc.Str(and_str), pandoc.Space()}

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


--- Create inlines for a single affilliation
local function affiliation_inline_generator()
  return function (affiliation)
    local affiliation_marks = List:new{}

    if not affiliation.id and not affiliation.institution then
      return nil
    end

    if affiliation.id[1] == nil then
      affiliation.id[1] = ""
    end

    local res = List:new{
      pandoc.Superscript(affiliation.id[1]), pandoc.Space(),
      table.unpack(affiliation.institution)
    }

    return res
  end
end

--- Generate a list of inlines containing all affiliations
local function create_affiliation_inlines(affiliation)
  local inlines_generator = affiliation_inline_generator()
  local inlines = List:new(affiliation):map(inlines_generator)

  local result = List:new{}
  result:extend(inlines)
  result = intercalate(result, {pandoc.RawInline("latex", "\\\\")})

  local test = List:new{}
  for i=1, #result do
    table.insert(test, result[i])
  end

  return List:new{pandoc.RawInline("latex", "\\vspace{0.5cm}")} .. test
end


--- Generate equal contributor elements for the author note
local function create_equal_contributors(authors)
  local equal_contributors_i = List:new{}
  local equal_contributors = {table.unpack(authors)}

  for i, author in ipairs(authors) do
    if not author.equal_contrib then
      equal_contributors_i[#equal_contributors_i + 1] = i
    end
  end

  for i = 1, #equal_contributors_i do
    table.remove(equal_contributors, equal_contributors_i[#equal_contributors_i + 1 - i])
  end

  if #equal_contributors == 0 then
    return List.new{}
  end

  local mark
  if #equal_contributors > 0 then
    mark = List:new{pandoc.Superscript(equal_contributor_mark[1]), pandoc.Str(" ")}
  else
    mark = List:new{}
  end

  local equal_contributors_inline
  if #equal_contributors == 0 then
    equal_contributors_inline = List:new{}
  else
    if #equal_contributors < #authors then
      equal_contributors_inline = create_authors_inlines(equal_contributors, "and", false)
    else
      equal_contributors_inline = List:new{pandoc.Str"All", pandoc.Space(), pandoc.Str"authors"}
    end
  end

  local contribution_line
  if #equal_contributors > 0 then
    contribution_line = List:new{
      pandoc.Space(), pandoc.Str"contributed", pandoc.Space(), pandoc.Str"equally",
      pandoc.Space(), pandoc.Str"to", pandoc.Space(), pandoc.Str"this", pandoc.Space(),
      pandoc.Str"work", pandoc.Str".", pandoc.Space()
    }
  else
    contribution_line = List:new{}
  end

  return mark .. equal_contributors_inline .. contribution_line
end


--- Generate a roles elements for the author note
local function create_roles(authors)
  local authors_roles = List:new{}

  authors_roles:extend(
    List:new{
      pandoc.Str "The", pandoc.Space(), pandoc.Str "authors", pandoc.Space(),
      pandoc.Str "made", pandoc.Space(), pandoc.Str "the", pandoc.Space(),
      pandoc.Str "following", pandoc.Space(), pandoc.Str "contributions",
      pandoc.Str ".", pandoc.Space()
    }
  )

  local no_roles = #authors_roles

  for i, author in ipairs(authors) do
    if author.role then
      authors_roles:extend(author.name)
      authors_roles:extend(List:new{pandoc.Str ":", pandoc.Space()})
      authors_roles:extend(intercalate(author.role, {pandoc.Str ",", pandoc.Space()}))
      authors_roles:extend(List:new{pandoc.Str ";", pandoc.Space()})
    end
  end

  if #authors_roles == no_roles then
    return List.new{}
  else
    table.remove(authors_roles, #authors_roles)
    table.remove(authors_roles, #authors_roles)
    authors_roles:extend(List:new{pandoc.Str ".", pandoc.Space()})
    return authors_roles
  end
end


--- Generate a correspondence elements for the author note
local function create_correspondence(authors)
  local corresponding_authors = List:new{}

  for i, author in ipairs(authors) do
    if author.corresponding and author.address and author.email then
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
    return List.new{}
  end

  local correspondence_line
  if #corresponding_authors > 0 then
    correspondence_line = List:new{
      pandoc.Str"Correspondence", pandoc.Space(), pandoc.Str"concerning", pandoc.Space(),
      pandoc.Str"this", pandoc.Space(), pandoc.Str"article", pandoc.Space(),
      pandoc.Str"should", pandoc.Space(), pandoc.Str"be", pandoc.Space(),
      pandoc.Str"addressed", pandoc.Space(), pandoc.Str"to", pandoc.Space()
    }
  else
    contact_info = List:new{}
    correspondence_line =  List:new{}
  end

  return correspondence_line .. contact_info
end






function table.flatten(arr)
	local result = { }

	local function flatten(arr)
		for _, v in ipairs(arr) do
			if type(v) == "table" then
				flatten(v)
			else
				table.insert(result, v)
			end
		end
	end

	flatten(arr)
	return result
end




--- Create raw LaTeX environments from metadata fields
local function make_latex_envir(name, metadata)
  local data = {table.unpack(metadata)}
  local pandoc_type = data[1].t


  -- if pandoc_type == "Str" or pandoc_type == "RawInline" then
  --   return List:new{pandoc.Para({pandoc.RawInline("latex", "\\" .. name), pandoc.Span(data)})}
  -- end
  if pandoc_type == "Para" or pandoc_type == "Plain" or pandoc_type == "RawBlock" then
    local envir = List:new{pandoc.Para(pandoc.RawInline("latex", "\\" .. name .. "{"))}
    envir:extend(data)
    envir:extend(List:new{pandoc.Para(pandoc.RawInline("latex", "}"))})
    return envir
  else
    return List:new{pandoc.Para({pandoc.RawInline("latex", "\\" .. name), pandoc.Span(data)})}
  end
end

function Pandoc (document)

  document.blocks:extend(List:new{pandoc.Para(pandoc.RawInline("latex", "% papaja Lua-filter additions"))})

  if document.meta.note ~= nil then
    document.blocks:extend(make_latex_envir("note", document.meta.note))
  end

  if document.meta.authornote ~= nil then
    if document.meta.author ~= nil then
      local roles = create_roles(document.meta.author)
      local equal_contributors = create_equal_contributors(document.meta.author)
      if #roles > 0 or #equal_contributors > 0 then
        table.insert(document.meta.authornote, pandoc.Para(roles .. equal_contributors))
      end

      local correspondence = create_correspondence(document.meta.author)
      if  #correspondence > 0 then
        table.insert(document.meta.authornote, pandoc.Para(correspondence))
      end
    end
    document.blocks:extend(make_latex_envir("authornote", document.meta.authornote))
  end

  if document.meta.affiliation ~= nil then
    local affiliations = create_affiliation_inlines(document.meta.affiliation)
    if #affiliations > 0 then
      document.meta.affiliation = affiliations
      document.blocks:extend(make_latex_envir("affiliation", document.meta.affiliation))
    end
  else
    document.blocks:extend(make_latex_envir("affiliation", {pandoc.RawInline("latex", "\\phantom{0}")}))
  end

  document.blocks:extend(List:new{pandoc.Para(pandoc.RawInline("latex", "% End of papaja Lua-filter additions"))})

  if document.meta.author ~= nil then
    document.meta.author = pandoc.MetaInlines(create_authors_inlines(document.meta.author, "&", true))
  else
    document.meta.author = FORMAT == "latex"
      and pandoc.MetaInlines({pandoc.RawInline("latex", "\\phantom{0}")})
      or nil
  end

  return document
end


