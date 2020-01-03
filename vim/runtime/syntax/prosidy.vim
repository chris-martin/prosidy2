" Vim syntax file
" Language:        Haskell
" Maintainer:      J Alexander Feldman-Crough <alex@fldcr.com>
" Latest Revision: 2020-01-01

if exists("b:current_syntax")
    finish
endif

" -----------------------------------------------------------------------------

syntax iskeyword 33,36-38,40-43,45-57,59,60,62-90,94-122,124,126-255
syntax sync minlines=100

" -----------------------------------------------------------------------------

syntax match proComment
    \ /\v##.*$/
    \ display

" -----------------------------------------------------------------------------

syntax region proHeader
    \ start=/\v%^.*/
    \ end=/\v^---/
    \ transparent
    \ contains=proComment,proHeaderKey

syntax match proHeaderKey
    \ /\v^\s*\v[_[:alpha:][:digit:]]\k*/
    \ contained display
    \ nextgroup=proHeaderVal
    \ skipwhite

syntax region proHeaderVal
    \ end=/\v(##)@=/ end=/$/
    \ matchgroup=proHeaderSep
    \ start=/\v[:=]\s*/
    \ contained display

" -----------------------------------------------------------------------------

syntax match proTag
    \ /\v[_[:alpha:][:digit:]]\k*/
    \ contained display
    \ nextgroup=proMeta

syntax region proMeta
    \ matchgroup=proSigil
    \ start=/\v\[/
    \ end=/\v\]/
    \ contained transparent
    \ contains=proMetaKey,proMetaAssign,proMetaVal,proMetaSep

syntax match proMetaKey
    \ /\v[_[:alpha:][:digit:]]\k*/
    \ display contained
    \ skipwhite skipempty

syntax match proMetaAssign
    \ /\v[:=]/
    \ contained display

syntax region proMetaVal
    \ start=/\v\z(['"])/
    \ skip=/\v\\\z1/
    \ end=/\v\z1/
    \ contained

syntax match proMetaSep
    \ /\v,/
    \ display contained

syntax region proBlock
    \ matchgroup=proSigil
    \ start=/\v^\s*#-/
    \ excludenl end=/\v$/
    \ contains=proTag,proBlockR,proInlineR

syntax region proBlockR
    \ matchgroup=proSigil
    \ start=/\v:\z(\S*)\s*$/
    \ excludenl end=/\v^\s*#:\z1\s*$/
    \ contained transparent
    \ contains=TOP,proHeader

syntax region proLiteral
    \ matchgroup=proSigil
    \ start=/\v^\s*#\=/
    \ excludenl end=/\v$/
    \ contains=proTag,proLiteralR

syntax region proLiteralR
    \ matchgroup=proSigil
    \ start=/\v:\z(\S*)\s*$/
    \ excludenl end=/\v^\s*#:\z1\s*$/
    \ contained

syntax region proInline
    \ matchgroup=proSigil
    \ start=/\v#[^:#=-]/rs=s+1
    \ end=/\v(]\{@!)@<=/
    \ end=/\v}@<=/
    \ end=/\s/
    \ excludenl end=/\v$/
    \ contains=proTag,proInlineR

syntax region proInlineR
    \ matchgroup=proSigil
    \ start=/\v\{/
    \ skip=/\\\}/
    \ end=/\v\}/
    \ contained
    \ contains=TOP,proHeader,proBlock,proLiteral

" -----------------------------------------------------------------------------

if exists('g:prosidy_literal_languages')
    for key in keys(g:prosidy_literal_languages)
        exec 'syntax include @ProLiteral_' . key g:prosidy_literal_languages[key]
        exec 'syntax region proLiteralR_' . key
           \ 'matchgroup=proSigil'
           \ 'start=/\v:\z(\S*)\s*$/'
           \ 'excludenl end=/\v^\s*#:\z1\s*$/'
           \ 'transparent contained'
           \ 'contains=@ProLiteral_' . key
        exec 'syntax region proLiteral_' . key
           \ 'matchgroup=proSigil start=/\v^\s*#\=' . key . '/rs=s+2'
           \ 'excludenl end=/\v$/ contains=proTag,proLiteralR_' . key
    endfor
endif

" -----------------------------------------------------------------------------

highlight default link proComment    Comment
highlight default link proHeaderKey  ProsidyKey
highlight default link proMetaKey    ProsidyKey
highlight default link proLiteralR   ProsidyLiteral
highlight default link proHeaderSep  ProsidySyntax
highlight default link proMetaSep    ProsidySyntax
highlight default link proMetaAssign ProsidySyntax
highlight default link proSigil      ProsidySyntax
highlight default link proTag        ProsidyTag
highlight default link proHeaderVal  ProsidyValue
highlight default link proMetaVal    ProsidyValue

highlight default link ProsidyKey     Constant
highlight default link ProsidyLiteral PreProc
highlight default link ProsidySyntax  Type
highlight default link ProsidyTag     Keyword
highlight default link ProsidyValue   String

let b:current_syntax = "prosidy"
