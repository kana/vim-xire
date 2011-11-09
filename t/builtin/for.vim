function s:for()
  let i0 = 123
  for i1 in [1, 2, 3]
    echo i1
  endfor
  echo i0
endfunction
call s:for()

let s:i0 = 123
for s:i1 in [1, 2, 3]
  echo s:i1
endfor
echo s:i0
