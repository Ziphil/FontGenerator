#


import fontforge
import os
import re
import sys
import xml.etree.ElementTree as ElementTree


font = fontforge.font()

font.familyname = __familyname__
font.fontname = __fontname__
font.fullname = __fullname__
font.weight = __weight__
font.version = __version__
font.copyright = __copyright__

font.encoding = "UnicodeFull"

font.em = __em__
font.ascent = __ascent__
font.descent = __descent__

for file in os.listdir("."):
  result = re.match(r"(\d+)\.svg", file)
  if result:
    codepoint = int(result.group(1))
    root = ElementTree.parse(file).getroot()
    width = float(root.attrib["width"])
    glyph = font.createMappedChar(codepoint)
    glyph.importOutlines("%d.svg" % codepoint)
    glyph.autoHint()
    glyph.width = width

font.generate(__fontfilename__)
font.close()