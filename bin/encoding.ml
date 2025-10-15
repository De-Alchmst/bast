let sanitize str = Str.global_replace  (Str.regexp "-") "_" str

let encode_prefix pref str = pref ^ sanitize str 
