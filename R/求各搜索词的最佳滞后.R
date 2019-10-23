#####计算各个搜索词的滞后期####
#####导入数据####
library(xts)
library(readxl)
Realdata<-read_excel("C:/Users/Administrator/Desktop/汽车网站搜索词.xlsx")
names(Realdata[,1])<-"Date"
for(i in 1:55){
  Realdata[,i+1]<-100*Realdata[,i+1]/max(Realdata[,i+1])  #规范化
}
Realdata_xts<-as.xts(Realdata[,-1],order.by =Realdata$Date)
Saledata<-read_excel("C:/Users/Administrator/Desktop/数据/中国大陆汽车销量.xlsx")
Saledata[,2]<-100*Saledata[,2]/max(Saledata[,2])   #规范化
names(Saledata[,1])<-"Date"
names(Saledata[,2])<-"Salesvolume"
#####compute correlation#####
datalag<-1:13
x_cor<-matrix(0,nrow=55,ncol=13)
bestcor<-1:13
for(j in 1:55){

for(i in 1:13){
  
  x_cor[j,i]<-cor(Realdata[j+1][(14-i):(115-i),],Saledata[2][7:108,])
}
datalag[j]<-which.max(abs(x_cor[j,]))
bestcor[j]<-max(abs(x_cor[j,]))
}               
