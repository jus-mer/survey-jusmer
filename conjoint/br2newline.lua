function RawInline(el)
  if el.format == "html" and el.text:match("^<br%s*/?>$") then
    return pandoc.RawInline("latex", "\\newline ")
  end
end
