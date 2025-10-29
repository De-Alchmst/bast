func [main]:[
  foo:N
]:[
  foo := [ fact-fun fact-arg ]
  [println foo]

  [println [ fact-loop fact-arg ]]

  [println
    cond [F]:[ 1 ]:[T]:[ 2 ]]

  [println {1 2 3+4}]

  var mat : { {1 2 3} {4 5 6} {7 8 9}}
  w [mat 1 2 37]
  [println r [mat 1 2]] ; should be 6
  [println mat]
]
