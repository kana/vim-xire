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

echo []
echo [0, 1, 2, 3, 4, 5]
echo [0, 1, [2, 3], 4, 5]
