function s:let()
  echo '==== let in function'
  let x1 = 2
  let y1 = 3
  echo 'let (outer) 1:' 'x =' x1 'y = ' y1
  let x2 = 7
  let z2 = x1 + y1
  echo "let (inner):" "x =" x2 "y = " y1 "z = " z2
  echo 'let (outer) 2:' 'x =' x1 'y = ' y1
endfunction
call s:let()

echo '==== let in top-level'
let s:x1 = 2
let s:y1 = 3
echo 'let (outer) 1:' 'x =' s:x1 'y = ' s:y1
let s:x2 = 7
let s:z2 = s:x1 + s:y1
echo "let (inner):" "x =" s:x2 "y = " s:y1 "z = " s:z2
echo 'let (outer) 2:' 'x =' s:x1 'y = ' s:y1

function s:let_star()
  echo '==== let* in function'
  let x1 = 3
  let y1 = x1 + 1
  let z1 = x1 * y1
  echo 'let* (outer) 1:' x1 y1 z1
  let x2 = x1 + y1 + z1
  echo 'let* (inner):' x2 y1 z1
  echo 'let* (outer) 2:' x1 y1 z1
endfunction
call s:let_star()

echo '==== let* in top-level'
let s:x1 = 3
let s:y1 = s:x1 + 1
let s:z1 = s:x1 * s:y1
echo 'let* (outer) 1:' s:x1 s:y1 s:z1
let s:x2 = s:x1 + s:y1 + s:z1
echo 'let* (inner):' s:x2 s:y1 s:z1
echo 'let* (outer) 2:' s:x1 s:y1 s:z1
