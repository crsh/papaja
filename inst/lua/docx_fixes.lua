if FORMAT ~= "docx" then
  return
end

function Header (elem)
  if elem.level >= 3 then
    elem.content[#elem.content + 1] = pandoc.Str(".")
  end

  return elem
end

function Figure (fig)
  if fig.caption and #fig.caption.long > 0 then

    local caption = fig.caption.long[1].content

    if #caption >= 4 then
      caption[2] = pandoc.Emph(caption[2])
      -- Remove the colon from the 4th element (if it's a string)
      if caption[4] and pandoc.utils.stringify(caption[4]):match(":") then
        caption[4] = pandoc.Str(string.gsub(pandoc.utils.stringify(caption[4]), ":", "."))
      end
    end

     -- Assign the modified caption back to the table
     fig.caption.long[1].content = caption
  end

  -- Return the modified table
  return fig
end

function Table (tbl)
  -- Check if tbl.caption.long is not empty
  if tbl.caption and #tbl.caption.long > 0 then

    local caption = tbl.caption.long[1].content

    -- Modify the caption only if there are enough elements
    if #caption >= 4 then
      -- Remove the colon from the 4th element (if it's a string)
      if caption[4] and pandoc.utils.stringify(caption[4]):match(":") then
        caption[4] = pandoc.Str(string.gsub(pandoc.utils.stringify(caption[4]), ":", ""))
      end

      -- Add a line break as the 5th element
      if #caption >= 5 then
        caption[5] = pandoc.LineBreak()
      else
        table.insert(caption, 5, pandoc.LineBreak())
      end

      for i = 6, (#caption) do
        caption[i] = pandoc.Emph(caption[i])
      end
    end

     -- Assign the modified caption back to the table
     tbl.caption.long[1].content = caption
  end

  -- Return the modified table
  return tbl
end
