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
