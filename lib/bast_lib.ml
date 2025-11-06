let src = {|

func [rev iter]:[
  cond
  [+> list? iter]:[
    var aux : lamb [acc lst]:[
      if [+> nil? lst]:[acc]:[+> aux  [car lst]\acc  [cdr lst]]
    ]
    +> aux N iter
  
  ]:[+> array? iter]:[
    var [
      length : [len iter]
      new    : [array-make length Nil]
    ]

    for [ind 0 => length]:[
      w [new ind r [iter length -1- ind]]
    ]
    new

  ]:[T]:[
    [panic "cannot reverse " ~ [2debug iter]]
  ]
]


func [map iter fn]:[
  cond
  [+> list? iter]:[
    var aux : lamb [acc lst]:[
      if [+> nil? lst]:[+> rev acc]:[+> aux  [fn [car lst]]\acc  [cdr lst]]
    ]
    +> aux N iter

  ]:[+> array? iter]:[
    var new : [array-make [len iter] Nil]

    for [ind 0 => [len iter]]:[
      w [new ind [fn r [iter ind]]]
    ]
    new

  ]:[T]:[
    [panic "cannot map onto " ~ [2debug iter]]
  ]
]
|}
