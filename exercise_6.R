DoubleDigestProblem <- function(A, B, AB_correct){
  len_A <- length(A)
  len_B <- length(B)
  perms_A <- permn(A)
  perms_B <- permn(B)
  good_comb <- list()
  for (AA in perms_A){
    for (BB in perms_B){
      new_seq_A <- c(0)
      new_seq_B <- c(0)
      for (i in 1:len_A){
        new_seq_A <- append(new_seq_A,(new_seq_A[i]+AA[i]))
      }
      for (i in 1:len_B){
        new_seq_B <- append(new_seq_B,(new_seq_B[i]+BB[i]))
      }
      new_seq <- append(new_seq_B, new_seq_A)
      new_seq_A <- new_seq_A[2:(len_A)]
      new_seq_B <- new_seq_B[2:(len_B)]
      new_seq <- sort(new_seq)
      new_seq <- new_seq[2:(length(new_seq)-1)]
      new_seq <- sort(diff(new_seq))
      if (all(new_seq == AB_correct)){
        if (length(good_comb) == 0) {
          good_comb <- list(c(new_seq_A), c(new_seq_B))
        } else {
          good_comb <- list(good_comb, list(c(new_seq_A), c(new_seq_B)))
        }
        print("Good combination found!")
        print(new_seq_A)
        print(new_seq_B)
      }
    }
  }
  return(good_comb)
}

library(combinat)
Sequence_A = c(2,3,10,5)
Sequence_B = c(3,7,10)
AB <- c(1, 2, 2, 5, 5, 5)
combinations <- DoubleDigestProblem(Sequence_A,Sequence_B, AB)


Remove <- function(deltaX, difference){
  new_deltaX <- deltaX
  for (i in length(deltaX)){
    for (j in length(difference))
      if (deltaX[i] == difference[j]){
        new_deltaX <- remove(new_deltaX, difference[j])
      }
  }
  print(new_deltaX)
  return(new_deltaX)
}

Place <- function(deltaX, X, width){
  if (length(deltaX) == 0){
    return(X)
  }
  y <- max(deltaX)
  difference <- c()
  for (i in length(X)){
    difference <- append(difference, (y-X[i]))
  }
}

Remove(c(4,4,5,2,3,5), c(8,6,8,9,6,3))