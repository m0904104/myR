setwd("D:/myR")
rm(list=ls())
fn <- 'in1.txt'
cat("",file = "out.txt",append=F)
for (file in fn) {
  print(file)
  x = read.csv(
    file,
    header = F,
    sep = ",",
    skip = 1,
    col.names = c("data"),
    stringsAsFactors = F,
    fileEncoding = "UTF-8"
  )
  out <-  vector()
  for (s in x[, 1]) {
    s1 = substring(s, 1:nchar(s), 1:nchar(s))
    score = rep(0, length(s1))
    o = 1
    for (i in 1:length(s1)) {
      if (s1[i] == 'O') {
        score[i] = o
        o = o + 1
      } else{
        o = 1
      }
    }
    out = append(out, sum(score))
    print(out)
  }
  write.table(
    out,
    file =  "out.txt",
    append =  T,
    row.name =  F,
    col.names = F ,
    fileEncoding = "UTF-8"
  )
  cat("\n", file = "out.txt", append = T)
}