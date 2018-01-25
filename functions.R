xrayVmiss = function(A,X,m,n){
  if (length(A) == 1){
    value = 0
  }else{
    A_X = removeCard(A, X)
    play = V(A_X,m+X,n)
    replace = V(A,m,n)
    value = max(play, replace)
  }
  return (value)
}

xrayV = function(A,X,m,n){
  if (length(A) == 1){
    value = 0
  }else{
    A_X = removeCard(A, X)
    play = -V(A_X,n,m+X)
    replace = -V(A,n,m)
    value = max(play, replace)
  }
  return (value)
}

xrayW = function(A,X,m,n){
  if (length(A) == 1){
    value = m-n
  }else{
    A_X = removeCard(A, X)
    play = W(A_X,m+X,n)
    replace = W(A,m,n)
    value = max(play, replace)
  }
  return (value)
}

miss = function(A,m,n){
  #This function gives the value of the game from your perspective, given that you now have (potentially) two turns before handing back to the other player
  if (length(A) == 1){
    value = max(0,m - n)
  }else{
    leave = -W(A,n,m)
    stay = 0
    A_0 = removeCard(A,0)
    for (j in A_0){
      A_j = removeCard(A, j)
      if (j==-1){ #-1 = FLOOD
        stay = stay + V(A_j, 0, n)
      }else if (j==-2){ #-2 = MISS A TURN  
        stay = stay - miss(A_j,n,m) #- V(A_j, n, m) #
      }else if (j==-5){ #-5 = COLUMN
        stay = stay + V(A_j, m, n)
      }else if (j==-3){ #LOSE
        stay = stay + V(A_j, max(0,m-losePoints),n)
      }else if (j==-4){#XRAY
        Xvalue = 0
        for (X in A_j){
          Xvalue = Xvalue + xrayVmiss(A_j,X,m,n)
        }
        Xvalue = Xvalue / length(A_j)
        stay = stay + Xvalue
      }else{
        stay = stay + V(A_j, m + j, n)
      }
    }
    stay = (1/length(A)) * stay
    value = max(leave, stay)
  }
  return (value)
  
}

V = function(A,m,n){
  if (length(A) == 1){
    value = max(0,m - n)
  }else{
    leave = -W(A,n,m)
    stay = 0
    A_0 = removeCard(A,0)
    for (j in A_0){
        A_j = removeCard(A, j)
        if (j==-1){ #FLOOD
          stay = stay - V(A_j, n, 0)
        }else if (j==-2){ #-2 = MISS A TURN  
          stay = stay - miss(A_j,n,m) #- V(A_j, n, m) #
        }else if (j==-5){ #-5 = COLUMN
          stay = stay - V(A_j, n, m)
        }else if (j==-3){ #LOSE
          stay = stay - V(A_j, n, max(0,m-losePoints))
        }else if (j==-4){#XRAY
          Xvalue = 0
          for (X in A_j){
            Xvalue = Xvalue + xrayV(A_j,X,m,n)
          }
          Xvalue = Xvalue / length(A_j)
          stay = stay + Xvalue
        }else{
          stay = stay - V(A_j, n, m + j)
        }
    }
    stay = (1/length(A)) * stay
    value = max(leave, stay)
  }
  return (value)
}

Vcalc = function(A,m,n){
  
  leave = -W(A,n,m)
  value = V(A,m,n)
  
  if (value == leave){
    action = 'leave'
  }else{
    action = 'stay'
  }
  
  return (list(leave,value,action))
}
  
W = function(A,m,n){
  
  if (length(A) == 1){
    value = max(0,m - n)
  }else{
    leave = m - n
    A_0 = removeCard(A,0)  
    stay = 0
    for (j in A_0){
      A_j = removeCard(A, j)
      if (j==-1){ #FLOOD
        stay = stay + W(A_j, 0, n)
      }else if (j==-2 | j==-5){ #-2 = MISS A TURN    -5 = COLUMN
        stay = stay + W(A_j, m, n)
      }else if (j==-3){ #LOSE
        stay = stay + W(A_j, max(0,m-losePoints), n)
      }else if (j==-4){ #XRAY
        Xvalue = 0
        for (X in A_j){
          Xvalue = Xvalue + xrayW(A_j,X,m,n)
        }
        Xvalue = Xvalue / length(A_j)
        stay = stay + Xvalue
      }else{
        stay = stay + W(A_j, m + j, n)
      }
    }
    stay = (1/length(A)) * (stay - n)
    value = max(leave, stay)
  }
  
  return (value)
}

Wcalc = function(A,m,n){
  
  leave = m - n
  value = W(A,m,n)
  
  if (value == leave){
    action = 'leave'
  }else{
    action = 'stay'
  }
  
  return (list(leave,value,action))
}

removeCard= function(A,card){
  
  out = A[-(which(A==card))]
  return (out)
  
  
}