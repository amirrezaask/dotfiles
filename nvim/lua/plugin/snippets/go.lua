return {
  iferr = [[if err != nil { 
      $1
    }
    ]],
  ["for"] = [[for $1 := range $2 {

  }]],
  fori = [[for $1=0; $1 < $2 ; $1++ {
  }]],
}
