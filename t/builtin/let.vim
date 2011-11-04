function s:let()
  let x1 = 2
  let y1 = 3
  echo 'let (outer) 1:' 'x =' x1 'y = ' y1
  let x2 = 7
  let z2 = x1 + y1
  echo "let (inner):" "x =" x2 "y = " y1 "z = " z2
  echo 'let (outer) 2:' 'x =' x1 'y = ' y1
endfunction
call s:let()

let x = 3
let y = x + 1
let z = x * y
echo x y z
