title: Prosidy README
---

Prosidy is a small language for writing documents.

Like #link[url='https://daringfireball.net/projects/markdown/']{Markdown},
Prosidy's syntax is lightweight; it doesn't get in the way of your text.

Like #link[url='https://www.w3.org/XML/']{XML},
Prosidy is extensible: it doesn't make any assumptions about your content.
You'll never have to fight to make your data fit a structure that wasn't designed for it.

#=prosidy:end
recipe: A simple roux
---
In a #ware{medium saucepan},
heat #ingredient[amount='25', unit='g']{oil}
until dropping a pinch of flour into the oil causes it to bubble.

#ware{Whisk} #ingredient[amount='25', unit='g']{flour}
into the oil until the roux is the desired color.
#:end


All of the code here is under #b{heavy} development;
be careful before using it for anything critical!

That said, feedback is more than welcome!
Reach me at #link[url='mailto:alex@fldcr.com']{alex@fldcr.com}.

#-h{Changelog}
#-h+{2020-01-20}
Switched from Cabal to Shake to allow better CI tooling.

#-h+{2020-01-19}
Added source tags to all Prosidy elements meaning better errors!

#-h+{2020-01-03}
Refactored the various Prosidy repositories into a single monorepo.
