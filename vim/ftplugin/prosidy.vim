" Vim filetype plugin file
" Language:         Prosidy
" Maintainer:       J Alexander Feldman-Crough <alex@fldcr.com>
" Latest Revision:  2019-11-23

if exists("b:prosidy_ftplugin")
    finish
endif

let b:prosidy_ftplugin = 1

setlocal iskeyword=33,36-38,40-43,45-57,59,60,62-90,94-122,124,126-255
setlocal commentstring=##\ %s
