let sanitize str = Str.global_replace  (Str.regexp "-") "_" str

let encode_prefix str = "_bast_" ^ sanitize str 
