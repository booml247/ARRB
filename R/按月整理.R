####dataList[[1]][2][[1]][2]  ####date-list
#######dataList[[1]][2][[1]][2][[1]][2]###detailed date  :  "2007-01-02"
#substr(dataList[[1]][2][[1]][2][[1]][i],6,7)=="01"
library(stringr)
data_month<-c(1:114)
for(j in 1:10){
  s=0
for(i in 1:3469){
  
  
    while(as.numeric(substr(dataList[[1]][2][[1]][2][[1]][i],1,4))==2006+j){
      while(substr(dataList[[1]][2][[1]][2][[1]][i],6,7)=="01"){
      s<-s+as.numeric(dataList[[1]][2][[1]][4][[1]][i])
      }
      
    }
  data_month[12*(j-1)+1]<-s
  }

