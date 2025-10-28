func [main]:[
  foo:N
]:[
  foo := [ fact-fun fact-arg ]
  [println foo]

  [println [ fact-loop fact-arg ]]

  var [
    l1: 1\2\3\4\n
    l2: list [1 2 3 4]
  ]
  [println [caddr l1]]

  for [ind 8 -> 14]:[
    [println ind]
  ]

  for [ind 8 <= 14]:[
    foo : `ind
  ]:[
    [println foo]
  ]

  do [
    [println foo]
  ]

  do [foo:7]:[
    [println foo]
  ]
]

