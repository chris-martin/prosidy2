title: Syntax Guide
style: style.css
---

#-h{Headers}

#=code[lang='prosidy']:
#-h{This will be rendered as an h2 tag.}
#-h+{This will be rendered as an h3 tag.}
#-h++{This will be rendered as an h4 tag.}
#:

#-h{This will be rendered as an h2 tag.}
#-h+{This will be rendered as an h3 tag.}
#-h++{This will be rendered as an h4 tag.}

#-h{Emphasis}

#=code[lang='prosidy']:
#i{This text will be italic,} #b{and this text will be bold.}
You #i{#b{can}} combine them if you like.
#:

#i{This text will be italic,} #b{and this text will be bold.}
You #i{#b{can}} combine them if you like.

#-h{Lists}

#-h+{Unordered}

#=code[lang='prosidy']:end
#-list:
  #-item{Item 1}
  #-item:
    Item 2
    #-list:
      #-item{Item 2a}
      #-item{Item 2b}
    #:
  #:
  #-item:
    Item 3
    #-list[ord]:
      #-item{Item 3a}
      #-item{Item 3b}
    #:
  #:
#:
#:end

#-list:
  #-item{Item 1}
  #-item:
    Item 2
    #-list:
      #-item{Item 2a}
      #-item{Item 2b}
    #:
  #:
  #-item:
    Item 3
    #-list[ord]:
      #-item{Item 3a}
      #-item{Item 3b}
    #:
  #:
#:

#-h+{Ordered}
#=code[lang='prosidy']:end
#-list[ord]:
  #-item{Item 1}

  #-item:
    Item 2
    #-list[ord]:
      #-item{Item 2a}
      #-item{Item 2b}
    #:
  #:

  #-item:
    Item 3
    #-list:
      #-item{Item 3a}
      #-item{Item 3b}
    #:
  #:
#:
#:end

#-list[ord]:
  #-item{Item 1}

  #-item:
    Item 2
    #-list[ord]:
      #-item{Item 2a}
      #-item{Item 2b}
    #:
  #:

  #-item:
    Item 3
    #-list:
      #-item{Item 3a}
      #-item{Item 3b}
    #:
  #:
#:

#-h{Images}
#=code[lang='prosidy']:end
These images are inline—
#image[url='https://placekitten.com/32/32?1'],
#image[url='https://placekitten.com/32/32?2', title='The "title" key attaches "alt-text"'],
but block images are also supported!

#-image[url='https://placekitten.com/400/200?3']{And block images support captions!}
#:end

These images are inline—
#image[url='https://placekitten.com/32/32?1'],
#image[url='https://placekitten.com/32/32?2', title='The "title" key attaches "alt-text"'],
but block images are also supported!

#-image[url='https://placekitten.com/400/200?3']{And block images support captions!}

#-h{Links}
#=code[lang='prosidy']:end
Here's a link to the #link[url='https://prosidy.org']{Prosidy homepage}.
#:end

Here's a link to the #link[url='https://prosidy.org']{Prosidy homepage}.

#-h{Blockquotes}
#=code[lang='prosidy']:end
#-quote:
  Everything within a "quote" block is rendered as block quotes.
#:
#:end

#-quote:
  Everything within a "quote" block is rendered as block quotes.
#:

#-h{Code}

#-h+{Code blocks}
#=code[lang='prosidy']:end
#=code[lang='haskell']:
fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fib (tail fib)
#:

#=code[lang='python']:
def fibonacci(n):
    a = 0
    b = 1
    for _ in range(0, n):
      (a, b) = (b, a + b)
    return a
#:
#:end

#=code[lang='haskell']:
fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fib (tail fib)
#:

#=code[lang='python']:
def fibonacci(n):
    a = 0
    b = 1
    for _ in range(0, n):
      (a, b) = (b, a + b)
    return a
#:

#-h+{Inline code}
#=code[lang='prosidy']:end
Some code: #lit{foo} #lit{bar}.
#:end

Some code: #lit{foo} #lit{bar}.