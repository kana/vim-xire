function s:hello(a, b, c)
  return printf("%s/%s/%s", a:c, a:b, a:a)
endfunction
echo s:hello(100, 10, 1)

function s:hi()
  return 'hi'
endfunction
function s:call(f)
  return a:f()
endfunction
echo s:call(function('s:hi'))

function! s:rest(a, b, c, ...)
  return [a:a, a:b, a:c, a:000]
endfunction
echo s:rest(1, 2, 3, 4, 5, 6)
