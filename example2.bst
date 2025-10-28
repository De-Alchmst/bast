func [main]:[
  foo:N
]:[
  foo := [ fact-fun fact-arg ]
  [println foo]

  [println [ fact-loop fact-arg ]]

  [println
    cond [F]:[ 1 ]:[T]:[ 2 ]]
]
