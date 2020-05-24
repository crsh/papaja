function Str(elem)
  if FORMAT:match 'latex' then
    i, j = string.match(elem.text, "^<@~")
    k, l = string.match(elem.text, "^~@>")
    if i ~= nil or k ~= nil then
      return pandoc.RawInline("latex", "% " .. elem.text)
    else
      return elem
    end
  end
end

