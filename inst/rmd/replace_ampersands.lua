function Cite (el)
  if el.c[1][1].mode == "AuthorInText" then
    for key,value in pairs(el.c[2]) do
      for key2,value2 in pairs(value) do
        if value2 == "&" then el.c[2][key][key2] = "and" end
      end
    end
  end
  return el
end
