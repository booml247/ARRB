library(stringr)
data_month<-vector("list",length(fileName))

for(k in 1:length(fileName)){
  for(j in 1:10){
    s<-0
    for(i in 1:3469){
      if(as.numeric(substr(dataList[[k]][2][[1]][2][[1]][i],1,4))==(2006+j)){
        if(substr(dataList[[k]][2][[1]][2][[1]][i],6,7)=="12") {
          s<-s+as.numeric(dataList[[k]][2][[1]][4][[1]][i])
        }
      }
    }
    data_month[[k]][12*(j-1)+12]<-s
  }
}
write.csv(data_month, file = "C:/Users/Administrator/Desktop/baidu_index_month12.csv", row.names = F, quote = F)