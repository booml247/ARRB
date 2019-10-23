#####数据恢复######
our_argo[["baidu"]] <-
  argo(logit(y/100), exogen = log((exogen + 0.5) / 100),
       alpha = 1, N_lag=1:26, use_all_previous = FALSE, N_training =26)   #argo



Saledata<-read_excel("C:/Users/Administrator/Desktop/数据/中国大陆汽车销量滞后6个月.xlsx")
names(Saledata[,1])<-"Date"
names(Saledata[,2])<-"Salesvolume"
Saledata_xts<-as.xts(Saledata[,-1],order.by =Saledata$Date)

plot(logit_inv(our_argo$baidu$pred)*100*max(Saledata[,2]))
lines(Saledata_xts,lty=2,lwd=2,col="red")