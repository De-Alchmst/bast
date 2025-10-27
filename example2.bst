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
  [println l1 = l2]

  while [false]:[
    [println 1\2]
  ]

  do-while [false]:[
    [println 1\3]
  ]
]

