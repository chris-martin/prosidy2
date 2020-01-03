title: Syntax Guide
style: style.css
---

#-section:endsection
#-h{Headers}

#=prosidy:
#-h{This will be rendered as an h2 tag.}
#-h+{This will be rendered as an h3 tag.}
#-h++{This will be rendered as an h4 tag.}
#:

#-section[class='out']:
#-h{This will be rendered as an h2 tag.}
#-h+{This will be rendered as an h3 tag.}
#-h++{This will be rendered as an h4 tag.}
#:
#:endsection

#-section:endsection
#-h{Emphasis}

#=prosidy:
#i{This text will be italic,} #b{and this text will be bold.}
You #i{#b{can}} combine them if you like.
#:

#-section[class='out']:
#i{This text will be italic,} #b{and this text will be bold.}
You #i{#b{can}} combine them if you like.
#:
#:endsection

#-section:endsection
#-h{Lists}

#-section:endsubsection
#-h+{Unordered}

#=prosidy:end
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

#-section[class='out']:
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
#:
#:endsubsection

#-section:endsubsection
#-h+{Ordered}
#=prosidy:end
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

#-section[class='out']:
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
#:
#:endsubsection
#:endsection

#-section:endsection
#-h{Images}
#=prosidy:end
These images are inline—
#image[url='https://placekitten.com/32/32?1'],
#image[url='https://placekitten.com/32/32?2', title='The "title" key attaches "alt-text"'],
but block images are also supported!

#-image[url='https://placekitten.com/400/200?3']{And block images support captions!}
#:end

#-section[class='out']:
These images are inline—
#image[url='https://placekitten.com/32/32?1'],
#image[url='https://placekitten.com/32/32?2', title='The "title" key attaches "alt-text"'],
but block images are also supported!

#-image[url='https://placekitten.com/400/200?3']{And block images support captions!}
#:
#:endsection

#-section:endsection
#-h{Links}
#=prosidy:end
Here's a link to the #link[url='https://prosidy.org']{Prosidy homepage}.
#:end

#-section[class='out']:
Here's a link to the #link[url='https://prosidy.org']{Prosidy homepage}.
#:
#:endsection

#-section:endsection
#-h{Blockquotes}
#=prosidy:end
#-quote:
  Everything within a "quote" block is rendered as block quotes.
#:
#:end

#-section[class='out']:
#-quote:
  Everything within a "quote" block is rendered as block quotes.
#:
#:
#:endsection

#-section:endsection
#-h{Code}

#-section:endsubsection
#-h+{Code blocks}
#=prosidy:end
#=haskell:
fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fib (tail fib)
#:

#=python:
def fibonacci(n):
    a = 0
    b = 1
    for _ in range(0, n):
      (a, b) = (b, a + b)
    return a
#:
#:end

#-section[class='out']:
#=haskell:
fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fib (tail fib)
#:

#=python:
def fibonacci(n):
    a = 0
    b = 1
    for _ in range(0, n):
      (a, b) = (b, a + b)
    return a
#:
#:
#:endsubsection

#-section:endsubsection
#-h+{Inline code}
#=prosidy:end
Some code: #lit{foo} #lit{bar}.
#:end
#-section:
Some code: #lit{foo} #lit{bar}.
#:
#:endsubsection
#:endsection

