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

#-section[title='Base Syntax']:
  This section defines the smallest parts of Prosidy syntax,
  such as the syntax for #term[lemma='character']{characters}
  and #term[lemma='escape sequence']{escapes}.

  #-note[level='info']:
    This section is low-level.

    If you're writing a parser, it's a great place to start!
    However, newer Prosidy users may wish to skip forward to 
    #ref[section='Document Structure']{Document Structure}
    to get a larger view of what Prosidy is.
  #:

  #-section[title='Characters']:
    A #def{character} is a single Unicode codepoint
    (regardless of whether or not that codepoint represents a single glyph
    or combines with other neighboring codepoints as a modifier).

    #-spec[test='test.prosidy.invalid-utf-8']:
      Because Prosidy documents are #ref[section='Encoding']{encoded in UTF-8},
      some byte sequences may not represent a valid codepoint.
      Implementations should either fail on inputs not encoded in valid UTF-8,
      or replace all invalid sequences with the #chars[rep='\uFFFD', no-escape]{replacement character}.
    #:
  #:

  #-section[title='Line breaks']:
    #-spec[test='test.prosidy.line-break']:
      A #def{line break} is a sequence of characters that marks the end of a #term{line}.

      All Prosidy parsers must treat the following sequences as a line break:

      #-list:
        #-item{#chars[rep='\n']{Unix newline}}
        #-item{#chars[rep='\r\n']{Windows newline}}
        #-item{#chars[rep='\r']{Classic Mac OS newline}}
      #:
    #:
  #:

  #-section[title='Lines']:
    A #def{line} is a sequence of characters terminated by a #term{line break}.
    A #term{line break} cannot otherwise appear as plain-text 
    unless it is #term[lemma='escape sequence']{escaped}.
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

  #-section[title='Comments']:
    A #def{comment} is text present in a Prosidy document that is not present in its output.
    A comment begins with #chars[rep='##']{two hash signs} and 
    continues until #ref[section='Line breaks']{the end of the line}.

    #-spec[test='test.prosidy.comment']:
      Comments can appoear in any #ref[section='Tags & contexts']{context}
      #i{except} for #ref[section='Literal context']{literal contexts}
      (where they are included as text)
      and on the same line as the #term{document divider}.
    #:

    This document consists only of the text “hello, world”;
    the comment is stripped while parsing.

    #=src[lang='pro']:
---
Hello, world!   ## Maybe a bit cliché?
    #:
  #:
#:

#-section[title='Document structure']:
  Prosidy is organized into #def[lemma='document']{documents};
  each #term{document} is normally stored in a single file on a filesystem,
  but ultimately the storage mechanism is application dependent.

  Each #term{document} contains exactly two parts: 
  a #term{header} followed by the #term{body}.

  #-section[title='The header']:
    The #def{header} begins at the start of the document 
    and continues until the #term{document divider}.

    The #term{header} contains #term{metadata} about the rest of the document.
    Each non-empty #term{line} in the header represents a single #term{property} or #term{setting}.

    ## TODO: Fill in with information about header syntax.

    Its syntax is different from the #term{body}'s syntax to simplify scripting of the metadata.
    For instance, you can build a table of contents from the #term[lemma='header']{headers}
    of all of your documents with tools like #lit{sed}, 
    while doing the same for the #term{body} would require a full Prosidy parser.
  #:

  #-section[title='The body']:
    The #def{body} begins on the first line proceeding the #term{header},
    and continues until the end of the document.
  #:

  #-section[title='The divide']:
    The #term{header} and the #term{body} are separated by the #def{document divider}—
    a #term{line} containing #chars[rep='---']{three consecutive hyphens} and nothing else.
    
    #-spec[test='test.prosidy.empty-document']:
      This delimiter is the #i{only} required element in a #term{document}.
      A #term{document} with a completely empty #term{header} and #term{body}
      looks like this:

      #=src[lang='pro']:
---
      #:
    #:
  #:
#:

#-section[title='Metadata']:
  Metadata is composed of #term[lemma='property']{properties} and #term[lemma='setting']{settings},
  applied to a region of a #term{document}.
  #def[lemma='metadata']{Metadata} can be attached to both #term[lemma='tag']{tags} 
  and #term[lemma='document']{documents}.

  #-section[title='Properties']:
    A #def{property} is a #term{key} which may either be specified or unspecified.
    #term[lemma='property']{Properties} are #i{boolean} attributes— they're either #i{on} or #i{off}.
    For instance, a hypothetical #term{dialect} may use attributes to style text.

    #=src[lang='pro']:
---
This isn't a #style[italic]{real} dialect, 
but it #style[bold]{does} work as an example!
    #:

    Here, the #term[lemma='property']{properties} #i{bold} and #i{italic}
    annotate how the text in the #i{style} #term{tag} should be displayed.
  #:

  #-section[title='Settings']:
    A #def{setting} is a #term{key} which is either unspecified or assigned a value.
    The same hypothetical #term{dialect} from #ref[section='Properties']{before}
    may allow #i{color} to be specified as a setting.

    #=src[lang='pro']:
---
#style[color='#F00']{This text is red};
but #style[bold, color='#0F0']{this text is blue and bold}.
    #:
  #:
#:

#-section[title='Keys']:
  A #def{key} is a sequence of characters used as the names of 
  #term[lemma='property']{properties}, #term[lemma='setting']{settings}, and #term[lemma='tag']{tags}.
#:

#-section[title='Tags & contexts']:
  A #def{tag} is a #term{key} used to annotate a region of a document.

  #-section[title='Literal context']:
  #:
#:

#-section[title='Encoding']:
  Prosidy #b{only} supports UTF-8 encoding.
#:

#-section[title='Dialects']:
  A #def{dialect} is a convention for writing documents with a standardized structure.
#: