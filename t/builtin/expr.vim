let r = 1
let s = 'then'
let t = 'else'
echo 0 ? 1 : 2
echo 3 ? 4 : 5
echo r ? s : t

echo 0 || 0 ? 'then' : 'else'
echo 0 || 1 ? 'then' : 'else'
echo 1 || 0 ? 'then' : 'else'
echo 1 || 1 ? 'then' : 'else'
echo 0 || 0 || 0 ? 'then' : 'else'
echo 0 || 1 || 1 ? 'then' : 'else'
echo 1 || 0 || 1 ? 'then' : 'else'
echo 1 || 1 || 0 ? 'then' : 'else'
echo 1 || 1 || 1 ? 'then' : 'else'

echo 0 && 0 ? 'then' : 'else'
echo 0 && 1 ? 'then' : 'else'
echo 1 && 0 ? 'then' : 'else'
echo 1 && 1 ? 'then' : 'else'
echo 0 && 0 && 0 ? 'then' : 'else'
echo 0 && 1 && 1 ? 'then' : 'else'
echo 1 && 0 && 1 ? 'then' : 'else'
echo 1 && 1 && 0 ? 'then' : 'else'
echo 1 && 1 && 1 ? 'then' : 'else'

for b in ['a', 'b', 'A', 'B']
  echo 'a' != b
  echo 'a' !=# b
  echo 'a' !=? b
  echo 'a' !~ b
  echo 'a' !~# b
  echo 'a' !~? b
  echo 'a' < b
  echo 'a' <# b
  echo 'a' <= b
  echo 'a' <=# b
  echo 'a' <=? b
  echo 'a' <? b
  echo 'a' == b
  echo 'a' ==# b
  echo 'a' ==? b
  echo 'a' =~ b
  echo 'a' =~# b
  echo 'a' =~? b
  echo 'a' > b
  echo 'a' ># b
  echo 'a' >= b
  echo 'a' >=# b
  echo 'a' >=? b
  echo 'a' >? b
  echo 'a' is b
  echo 'a' is# b
  echo 'a' is? b
  echo 'a' isnot b
  echo 'a' isnot# b
  echo 'a' isnot? b
endfor
let l = ['a']
for lb in [['a'], ['b'], ['A'], ['B'], l]
  echo 'a' is lb
  echo 'a' is# lb
  echo 'a' is? lb
  echo 'a' isnot lb
  echo 'a' isnot# lb
  echo 'a' isnot? lb
endfor
echo 1 < 2 && 2 < 3
echo 1 < 3 && 3 < 2

echo []
echo [0, 1, 2, 3, 4, 5]
echo [0, 1, [2, 3], 4, 5]
