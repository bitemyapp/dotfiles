augroup WorkingDirCmd
autocmd WorkingDirCmd VimEnter * call s:ChangeWorkingDir(expand("<amatch>"))

" Changes the working directory
" to the directory mvim opens
function s:ChangeWorkingDir(directory)
  let explicitDirectory = isdirectory(a:directory)
  let directory = explicitDirectory || empty(a:directory)

  if explicitDirectory
    exe "cd " . fnameescape(a:directory)
  endif

  " Allows reading from stdin
  " ex: git diff | mvim -R -
  if strlen(a:directory) == 0
    return
  endif

  if directory
    wincmd p
    bd
  endif

  if explicitDirectory
    wincmd p
  endif
endfunction

