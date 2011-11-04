function s:hi(x, y, z)
  echo 'x = ' a:x
  echo 'y = ' a:y
  echo 'z = ' a:z
  return 'w = ...'
endfunction
echo s:hi(1, 2, 3)
call s:hi(4, 5, 6)
