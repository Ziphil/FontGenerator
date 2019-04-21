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

font.em = int(__em__)
font.ascent = int(__ascent__)
font.descent = int(__descent__)

for file in os.listdir("."):
  result = re.match(r"(\d+)\.svg", file)
  if result:
    codepoint = int(result.group(1))
    tree = ElementTree.parse(file)
    root = tree.getroot()
    width = float(root.attrib["width"])
    glyph = font.createMappedChar(codepoint)
    glyph.width = width
    if root.find("{http://www.w3.org/2000/svg}g") is not None:
      glyph.importOutlines("%d.svg" % codepoint)
      glyph.autoHint()

font.generate(__fontfilename__)
font.close()