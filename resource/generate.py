#


import fontforge
import os
import re
import sys
import xml.etree.ElementTree as ElementTree


font = fontforge.font()

font.familyname = "Vekos"
font.fontname = "Vekos Regular"
font.fullname = "Vekos Regular"
font.weight = "Regular"
font.version = "0.0.0"
font.copyright = "Copyright 2019 Ziphil"

font.encoding = "UnicodeFull"

font.em = 1000
font.ascent = 750
font.descent = 250

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
    print("Glyph imported @ %d" % codepoint)
    sys.stdout.flush()

font.generate("font.ttf")
font.close()