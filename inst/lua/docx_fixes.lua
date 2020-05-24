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
  if img.caption[1] ~= nil then
    img.caption[1] = pandoc.Emph(img.caption[1])
    img.caption[3] = pandoc.Emph(string.gsub(img.caption[3].c, ":", ".  "))
  end
  return img
end
