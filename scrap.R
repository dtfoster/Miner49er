A_j0 = removeCard(A_j,0)
for (k in A_j0){
  
  A_jk = removeCard(A_j,k)
  p2stay = p2stay + V(A_jk,m+j,n+k, turn)
  
}



p2stay = (1/(length(A)-1)) * p2stay

p2value = min(p2leave, p2stay)

p1stay = p1stay + p2value