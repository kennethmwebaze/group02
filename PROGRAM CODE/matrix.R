height <- c(54,67,89)
breadth <- c(45,55,65)
age <- c(6,7,8)
df<-data.frame(height, breadth, age)

P <- function(df){
  coln <- data.frame(df$breadth, df $age)
  print(coln[2,])
  
}

P(df)
