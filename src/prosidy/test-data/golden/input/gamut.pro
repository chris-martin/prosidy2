## This is an example document which enumerates many valid syntaxes. It is
## probably not exhaustive, but should serve as a good document to implement
## against.

## Properties
prop
prop-with-comment    ## this has a trailing comment
    prop-with-leading-spaces ## arbitrary leading spaces are permitted

## Settings
set-a1:42
set-a2=42
set-a3: 42
set-a4= 42
set-a5 :42
set-a6 =42
set-a7 = 42
set-a8 : 42

    set-a9 = 42  ## arbitrary leading spaces are permitted

## Settings with trailing comments
set-b1:42            ## this has a trailing comment
set-b2=42            ## this has a trailing comment
set-b3: 42           ## this has a trailing comment
set-b4= 42           ## this has a trailing comment
set-b5 :42           ## this has a trailing comment
set-b6 =42           ## this has a trailing comment
set-b7 = 42          ## this has a trailing comment
set-b8 : 42          ## this has a trailing comment

## Settings with escaped values
set-c1 = foo \# bar
set-c2 = \#\# not a comment ## is a comment

## Settings with null values
set-d1 =
set-d2 :
set-d3 = ## Just a comment, not a value
set-d4 : ## Just a comment, not a value
---

Paragraph with only one line.

Paragraph trailed by comment. ## This is the comment

Paragraph split
over multiple lines.

  Paragraph split ## with an indent!
over multiple lines ## Another sneaky comment
with a comment trailing some.

#tag{This} paragraph starts with a simple tag.

This paragraph ends with a simple #tag{tag.}

This #tag{paragraph} has a few #tag{tags}.

#a{}b#c{}#d{}

This paragraph has a number of inline tag styles.
#tag              ## no meta, no children
#tag[]            ## no meta, no children
#tag[]{}          ## no meta, no children
#tag[prop]        ## 1 prop, no children
#tag[prop1,prop2] ## 2 props, no children
#tag[set = '42']  ## 1 setting (equal, single quote), no children
#tag[set = "42"]  ## 1 setting (equal, double quote), no children
#tag[set: '42']   ## 1 setting (colon, single quote), no children
#tag[set: "42"]   ## 1 setting (colon, double quote), no children
#tag[prop, foo: 'bar', baz = "quz"] ## complicated, no children
#tag{content}     ## no meta, children
#tag[]{content}   ## no meta, children
#tag{#tag{ok}}    ## no meta, children containing tags
#tag[prop, foo: 'bar', baz = "quz"]{#tag{ok}} ## ok wow
#tag[
] ## Meta blocks can span multiple lines
#tag[ ## Comments can go here
  ## Because items in the metadata block are relatively structured
  ## compared to the rest of the document, you can go wild with
  ## how properties are laid out

  prop1, prop2 ## Comments can go here
, set ## Comments can go here
= '42', prop3 ## Comments can go here
]{This one is pathological}

This paragraph has some tags that are awfuly close to one another.
#tag{}text#tag{}text#tag#tag{}.

#tag{This is a paragraph in an #kind{inline} tag.}

#tag{This is a paragraph in an #kind{inline} tag.
     It spans many lines.
     Content may follow.} This follows!

#tag{This is a paragraph in an #kind{inline} tag. ## Comments can go here
     It spans many lines .                        ## Comments can go here
     Content may follow.} This follows!           ## Comments can go here

#-tag{This is a paragraph in a #kind{block} tag.  ## Comments can go here
      Content may not follow.}                    ## Comments can go here

#-block: ## Comments can go here
  This is the other kind of block tag.
  It can have #italic{nested} block children.

  #-block: ## Empty blocks are fine too
  #:

  #-block[prop]:label ## Comments can go here
    Block starts/ends can have aribtrary labels attached.
  #:label ## Comments can go here

  #-block[set='42']:label ## Comments can go here
    Block starts/ends can have aribtrary labels attached.
  #:label ## Comments can go here

  #-block[prop1, set: '42', prop2]:label ## Comments can go here
    Block starts/ends can have aribtrary labels attached.
  #:label ## Comments can go here
#: ## Comments can go here

#-block[ ## Comments can go here
  ## Because items in the metadata block are relatively structured
  ## compared to the rest of the document, you can go wild with
  ## how properties are laid out

  prop1, prop2 ## Comments can go here
, set ## Comments can go here
= '42', prop3 ## Comments can go here
]:end ## Comments can go here
  This is another pathological tag, just a block this time.
  #:end

## This is a literal tag
## The label is once again optional like block tags,
## but a label is _very_ helpful for quoting prosidy
#=lit:eof ## Comments can go here, but not on the next line
#-block: ## this is treated literally!
  This is the other kind of block tag.
  It can have #italic{nested} block children.

  #-block: ## this is treated literally!
  #:

  #-block[prop]:label ## this is treated literally!
    Block starts/ends can have aribtrary labels attached.
  #:label ## this is treated literally!

  #-block[set='42']:label ## this is treated literally!
    Block starts/ends can have aribtrary labels attached.
  #:label ## this is treated literally!

  #-block[prop1, set: '42', prop2]:label ## this is treated literally!
    Block starts/ends can have aribtrary labels attached.
  #:label ## this is treated literally!
#: ## this is treated literally!
#:eof ## Comments can go here again

Some unicode escapes:
  "\u1F638" is a smiling cat face,
  and "\uB" is a vertical tab.
