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
  img.caption[1] = pandoc.Emph(img.caption[1])
  img.caption[3] = pandoc.Emph(img.caption[3])
  img.caption[4] = pandoc.Str(".  ")
  return img
end
