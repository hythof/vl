" :h group-name
if exists("b:current_syntax")
  finish
endif

setlocal isk+=!
setlocal isk+=:
setlocal isk+=@
setlocal isk+=-
syn sync minlines=500

" keybord helper
"inoremap {  {<CR>}<Esc>O
"inoremap (  ()<Esc>i
"inoremap [  []<Esc>i
"inoremap "  ""<Esc>i
"inoremap '  ''<Esc>i

" vl global syntax
syn match   vlComment / *#.*$/
hi def link vlComment Comment

" --- include
"syntax include @spa syntax/spa.vim
"
"syn region vlSpa start=/^--.\{-}| *spa$/ end=/\(^--\)\@=/ contains=@spa

" -- vl syntax
syn case match

syn match vlType /^ *[A-Z][a-zA-Z0-9_]*/
syn match vlVariable /^[a-z.][a-zA-Z0-9._]*/
syn match vlVariable /^| *[a-z.][a-zA-Z0-9._]*/
syn match vlNumber / [0-9][0-9]*\(\.[0-9]\+\)\?/
syn region vlString start='"' end='"' skip='\\"'

syn keyword vlKeyword if case do end as
"syn region vlBlock start=/if/ end="end"

hi def link vlDefine Define
hi def link vlType Type
hi def link vlIdentifier Identifier
hi def link vlVariable Identifier
hi def link vlFunction Function
hi def link vlKeyword Keyword
hi def link vlNumber Number
hi def link vlString String
