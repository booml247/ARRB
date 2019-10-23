library(stringr)
data_month<-vector("list",length(fileName))
case<-0
for(k in 1:length(fileName)){
  for(j in 1:10){
    s1<-0
    s2<-0
    s3<-0
    s4<-0
    s5<-0
    s6<-0
    s7<-0
    s8<-0
    s9<-0
    s_10<-0
    s_11<-0
    s_12<-0
    for(i in 1:3469){
      if(as.numeric(substr(dataList[[k]][2][[1]][2][[1]][i],1,4))==(2006+j)){
        if(substr(dataList[[k]][2][[1]][2][[1]][i],6,7)=="01") {case<-1}
        else if(substr(dataList[[k]][2][[1]][2][[1]][i],6,7)=="02") {case<-2}
        else if(substr(dataList[[k]][2][[1]][2][[1]][i],6,7)=="03") {case<-3}
        else if(substr(dataList[[k]][2][[1]][2][[1]][i],6,7)=="04") {case<-4}
        else if(substr(dataList[[k]][2][[1]][2][[1]][i],6,7)=="05") {case<-5}
        else if(substr(dataList[[k]][2][[1]][2][[1]][i],6,7)=="06") {case<-6}
        else if(substr(dataList[[k]][2][[1]][2][[1]][i],6,7)=="07") {case<-7}
        else if(substr(dataList[[k]][2][[1]][2][[1]][i],6,7)=="08") {case<-8}
        else if(substr(dataList[[k]][2][[1]][2][[1]][i],6,7)=="09") {case<-9}
        else if(substr(dataList[[k]][2][[1]][2][[1]][i],6,7)=="10") {case<-10}
        else if(substr(dataList[[k]][2][[1]][2][[1]][i],6,7)=="11") {case<-11}
        else if(substr(dataList[[k]][2][[1]][2][[1]][i],6,7)=="12") {case<-12}
        
      }
      switch(case,
             s1<-s1+as.numeric(dataList[[k]][2][[1]][4][[1]][i]),
             s2<-s2+as.numeric(dataList[[k]][2][[1]][4][[1]][i]),
             s3<-s3+as.numeric(dataList[[k]][2][[1]][4][[1]][i]),
             s4<-s4+as.numeric(dataList[[k]][2][[1]][4][[1]][i]),
             s5<-s5+as.numeric(dataList[[k]][2][[1]][4][[1]][i]),
             s6<-s6+as.numeric(dataList[[k]][2][[1]][4][[1]][i]),
             s7<-s7+as.numeric(dataList[[k]][2][[1]][4][[1]][i]),
             s8<-s8+as.numeric(dataList[[k]][2][[1]][4][[1]][i]),
             s9<-s9+as.numeric(dataList[[k]][2][[1]][4][[1]][i]),
             s_10<-s_10+as.numeric(dataList[[k]][2][[1]][4][[1]][i]),
             s_11<-s_11+as.numeric(dataList[[k]][2][[1]][4][[1]][i]),
             s_12<-s_12+as.numeric(dataList[[k]][2][[1]][4][[1]][i]))
    }
    data_month[[k]][12*(j-1)+1]<-s1
    data_month[[k]][12*(j-1)+2]<-s2
    data_month[[k]][12*(j-1)+3]<-s3
    data_month[[k]][12*(j-1)+4]<-s4
    data_month[[k]][12*(j-1)+5]<-s5
    data_month[[k]][12*(j-1)+6]<-s6
    data_month[[k]][12*(j-1)+7]<-s7
    data_month[[k]][12*(j-1)+8]<-s8
    data_month[[k]][12*(j-1)+9]<-s9
    data_month[[k]][12*(j-1)+10]<-s_10
    data_month[[k]][12*(j-1)+11]<-s_11
    data_month[[k]][12*(j-1)+12]<-s_12
  }
}

###调整list长度
data_month_2<-vector("list",length(fileName))
for(k in 1:length(fileName)){
  data_month_2[[k]]<-data_month[[k]][1:114]
}

###数据写入csv，并调整格式
write.csv(data_month_2, file = "C:/Users/Administrator/Desktop/baidu_index_month.csv", row.names = F, quote = F)

#or write.table(data_month_2, file = "C:/Users/Administrator/Desktop/baidu_index_month.csv", row.names = F, quote = F)

