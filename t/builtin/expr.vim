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

let n1 = 12
let n2 = 2
let n3 = 3
echo n1 + n2
echo n1 + n2 + n3
echo n1 - n2
echo n1 - n2 - n3
echo n1 . n2
echo n1 . n2 . n3

let n1 = 36
let n2 = 5
let n3 = 3
echo n1 * n2
echo n1 * n2 * n3
echo n1 / n2
echo n1 / n2 / n3
echo n1 % n2
echo n1 % n2 % n3

echo !0
echo !1
echo -(1)
echo +(1)

echo 'abc'[1]
echo [0, 4, 8][1]
echo {'a': 'A', 'b': 'B', 'c': 'C'}['c']
let s = 1
let x = 3
echo [0, 1, 2, 3, 4][(s):x]
echo [0, 1, 2, 3, 4][:x]
echo [0, 1, 2, 3, 4][(s):]
echo {'a': 'A'}.a
function s:dict(foo_to_bar)
  echo a:foo_to_bar
  return {'foo_to_bar': 'A'}.foo_to_bar
endfunction
echo s:dict(123)
let F = function('printf')
echo F('%02x', 33)
echo 123
echo 'abc'
echo &ignorecase
echo &g:expandtab
echo &l:expandtab
echo (10 + 10) / 2
let foo_bar_baz = 1
echo foo_bar_baz
echo $HOME
let @a = 'abc'
echo @a
echo function('printf')

let s = 1
echo []
echo [0, 1, 2, 3, 4, 5]
echo [0, 1, [2, 3], 4, 5]
echo {}
echo {'a': 'A', 'b': 'B', 'c': 'C'}
echo {(s): 2}
echo {'key1': 'val1', 'key2': 'val2'}
echo {'key1': 'val1', 'key2': 'val2'}
