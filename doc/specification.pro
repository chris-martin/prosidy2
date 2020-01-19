title:     Language Specification
toc-title: Spec
---

Prosidy is a small language for composing documents using plain text.
Its syntax is simple with the hope that it will be pleasent to write with
as well as straightforward to customize and specialize to the user's needs.

#-note[level='caution']:
  This is a provisional specification.

  Until it is finalized, be prepared for the specification to change.

  Additionally, Prosidy tooling is still in development 
  and may not conform to all parts of this specification,
  although any such cases are considered errors and should be reported to
  #link[url='mailto:ask@prosidy.org']{ask@prosidy.org}.
#:

#-section[title='Basic Syntax']:
  #-section[title='Character']:
    A #def{character} is a single Unicode codepoint
    (regardless of whether or not that codepoint represents a single glyph
    or combines with other neighboring codepoints as a modifier).

    Because Prosidy documents are #link[url='#encoding']{encoded in UTF-8},
    some byte sequences may not represent a valid codepoint.
    Invalid byte-sequences should be normalized to the
  #:

  #-section[title='Line break']:
    A #def{line break} is a sequence of characters that marks the end of a #term{line}.

    All Prosidy parsers must treat the following sequences as a line break:

    #-list:
      #-item{#chars[rep='\n']{Unix newline}}
      #-item{#chars[rep='\r\n']{Windows newline}}
      #-item{#chars[rep='\r']{Classic Mac OS newline}}
    #:
  #:

  #-section[title='Line']:
    A #def{line} is a sequence of characters terminated by a #term{line break}.
    A #term{line break} cannot appear in a line unless it is #term[lemma='escape sequence']{escaped}.
  #:

  #-section[title='Escape sequences']:
    In order to allow characters that Prosidy has reserved for its syntax to be used,
    Prosidy supports a number of #def[lemma='escape sequence']{escape sequences}
    which convert to special characters.

    Every escape sequence begins with a #chars[rep='\\']{backslash}, 
    followed by one or more characters which specify the intended character.

    The following characters are inserted literally when preceeded by a backslash:

    #-list:
      #-item{#chars[rep='\\']{backslashes}}
      #-item{#chars[rep='#']{hashes}}
      #-item{#chars[rep='\'']{single quotes}}
      #-item{#chars[rep='\"']{double quotes}}
      #-item{both #chars[rep='{']{left} and #chars[rep='}']{right} braces}
    #:

    Some characters are reserved by Prosidy syntax, 
    but #b{never} occur in a position that would require escaping.
    Prosidy supports escaping these symbols anyway in order to reduce the burden of remembering 
    exactly which symbols are special. They are:

    Some control characters are also used by Prosidy syntax, 
    and have corresponding escape sequences for literal use.

    #-list:
      #-item{#chars[rep='n']: #chars[rep='\r']{carriage-returns}}
      #-item{#chars[rep='r']: #chars[rep='\n']{line-feeds}}
    #:

    Finally, Prosidy supports escape sequences for a few cases which do not require escaping,
    but may be difficult to type.

    #-list:
      #-item{#chars[rep='t']: #chars[rep='\t']{tabs}}
      #-item{#chars[rep='u'] followed by 1-6 hexadecimal digits inserts a unicode codepoint}
    #:
  #:
#:

#-section[title='Document structure']:
  Prosidy is organized into #def[lemma='document']{documents};
  Each #term{document} is normally stored in a single file on a filesystem,
  but ultimately the storage mechanism is application dependent.

  Each #term{document} contains exactly two parts: 
  a #term{header} followed by the #term{body}.

  #-section[title='The Header']:
    The #def{header} begins at the start of the document 
    and continues until a #term{line} containing only 
    #chars[rep='---']{three hyphens} is encountered.
  #:

  #-section[title='The Body']:
    The #def{body} begins on the first line proceeding the #term{header},
    and continues until the end of the document.
  #:
#:

#-section[title='Encoding']:
#: