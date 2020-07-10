function Str(elem)
  i, j = string.match(elem.text, "^<@~")
  k, l = string.match(elem.text, "^~@>")

  if i ~= nil or k ~= nil then
    if FORMAT:match "latex" then
      return pandoc.RawInline("latex", "% " .. elem.text)
    else
      return pandoc.Str("")
    end
  else
    return elem
  end
end

