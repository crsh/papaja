if FORMAT ~= "docx" then
  return
end

function Header (elem)
  if elem.level >= 3 then
    elem.content[#elem.content + 1] = pandoc.Str(".")
  end

  return elem
end

function Image (img)
  local caption = img.caption

  if caption[1] ~= nil then
    caption[1] = pandoc.Emph(caption[1])
    caption[3] = pandoc.Emph(string.gsub(pandoc.utils.stringify(caption[3]), ":", ".  "))
  end
  return img
end

function Table (tbl)
  local caption = tbl.caption.long[1].content

 if caption[1] ~= nil then
    caption[3] = pandoc.Str(string.gsub(pandoc.utils.stringify(caption[3]), ":", ""))
    caption[4] = pandoc.LineBreak()

    for i = 5, (#caption) do
      caption[i] = pandoc.Emph(caption[i])
    end
 end

 tbl.caption.long[1].content = caption

 return tbl
end

