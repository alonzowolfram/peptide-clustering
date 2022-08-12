LevenshteinDistance = function(x) {
  # https://en.wikipedia.org/wiki/Wagner%E2%80%93Fischer_algorithm
  # For all i and j, d[i,j] will hold the Levenshtein distance between the first i characters of s and the first j characters of t.
  # Note that d has (m+1)*(n+1) values.
  s = x[1]
  t = x[2]
  s = unlist(strsplit(s, ""))
  t = unlist(strsplit(t, ""))
  m = length(s)
  n = length(t)
  d = matrix(0, nrow = m + 1, ncol = n + 1)
  
  # Source prefixes can be transformed into empty string by dropping all characters.
  for(i in 1:m) {
    d[i+1, 1] = i
  }
  
  # Target prefixes can be reached from empty source prefix by inserting every character.
  for(j in 1:n) {
    d[1, j+1] = j
  }
  
  for(j in 1:n) {
    for(i in 1:m) {
      if(s[i] == t[j]) {
        substitution_cost = 0
      } else {
        substitution_cost = sub_costs_melted %>% .[(.[,1]==s[i] & .[,2]==t[j]), 3]
      }
      
      d[i+1, j+1] = min(d[i, j+1] + 1,                # deletion
                    d[i+1, j] + 1,                    # insertion
                    d[i, j] + substitution_cost)      # substitution
    }
  }
  
  return(d[m+1, n+1])
    
}
