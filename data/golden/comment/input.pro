case: Comments

## Empty line in header
    ## Empty line after spaces in header

prop            ## After a property
empty =         ## After an empty setting
nonempty = yes  ## After a non-empty setting

---

## Empty line in the body
    ## Empty line after spaces in the body


#-tag   ## Immediately after an empty block-tag

## On the line preceeding an empty block tag
#-tag

#-tag
## On the line proceeding an empty block tag


#-tag[prop, setting='']  ## Immediately after an empty block-tag with metadata

## On the line preceeding an empty block tag with metadata
#-tag[prop, setting='']

#-tag[prop, setting='']
## On the line proceeding an empty block tag with metadata

#-tag[    ## After the metadata open bracket
prop      ## After a property in the metadata
,         ## After a metadata separator
setting   ## After a setting key in the metadata
=         ## After the association operator
"foo"     ## After a quoted value
]         ## After a metadata close bracket

#-tag[    ## After the metadata open bracket
prop      ## After a property in the metadata
,         ## After a metadata separator
setting   ## After a setting key in the metadata
=         ## After the association operator
"foo"     ## After a quoted value
]:        ## After the block open colon
    ## Within a block
    Inner contents

    Another paragraph
    ## Within a block
#:        ## After a named block close

#-tag[    ## After the metadata open bracket
prop      ## After a property in the metadata
,         ## After a metadata separator
setting   ## After a setting key in the metadata
=         ## After the association operator
"foo"     ## After a quoted value
]:end     ## After the block open colon
    ## Within a block
    Inner contents

    Another paragraph
    ## Within a block
#:end     ## After a named block close

#-tag[    ## After the metadata open bracket
prop      ## After a property in the metadata
,         ## After a metadata separator
setting   ## After a setting key in the metadata
=         ## After the association operator
"foo"     ## After a quoted value
]{        ## After the inline brace

## Within the brace

Inner contents

## Within the brace

}        ## After the inline brace

## Before a paragraph
Blah blah  ## After a line in a paragraph
blah blah. ## After a line in a paragraph
#inline[  ## After a dangling inline meta open
prop      ## After a property in the metadata
,         ## After a metadata separator
setting   ## After a setting key in the metadata
=         ## After the association operator
"foo"     ## After a quoted value
]         ## After the inline meta close
#inline[  ## After a dangling inline meta open
prop      ## After a property in the metadata
,         ## After a metadata separator
setting   ## After a setting key in the metadata
=         ## After the association operator
"foo"     ## After a quoted value
]{        ## Within the brace
          ## Within the brace
look ma   ## After a paragraph in a brace
          ## Within the brace
          ## Within the brace
}         ## After a brace closes
#inline[  ## After a dangling inline meta open
prop      ## After a property in the metadata
,         ## After a metadata separator
setting   ## After a setting key in the metadata
=         ## After the association operator
"foo"     ## After a quoted value
]{        ## Within the brace
          ## Within the brace
no hands ## After a paragraph in a brace
          ## Within the brace
          ## Within the brace
} content ## After a brace closes with trailing content
## After a paragraph


#=src:end ## After a literal tag's opening line
          ## This is _not_ a comment; it's part of the tag around it
#:end     ## After a literal tag's closing line

## As the last line of a document, without a trailing newline
## This is a direct remediation of a bug I found