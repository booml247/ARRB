###创建xts对象
library(xts)
library(readxl)
Realdata<-read_excel("C:/Users/Administrator/Desktop/汽车网站搜索词2.xlsx")
names(Realdata[,1])<-"Date"
for(i in 1:56){
  Realdata[,i+1]<-100*Realdata[,i+1]/max(Realdata[,i+1])  #规范化
}
Realdata_xts<-as.xts(Realdata[,-1],order.by =Realdata$Date)
#Realdata_xts<-Realdata_xts[1:97]

Saledata<-read_excel("C:/Users/Administrator/Desktop/数据/中国大陆汽车销量滞后5个月.xlsx")
Saledata[,2]<-Saledata[,2]/max(Saledata[,2])   #规范化
names(Saledata[,1])<-"Date"
names(Saledata[,2])<-"Salesvolume"
Saledata_xts<-as.xts(Saledata[,-1],order.by =Saledata$Date)
Saledata_xts<-Saledata_xts

Realdata_xts_scale<-scale(Realdata_xts,center=F,scale=T)
###使用argo包求参数
library(argo)
#### start of meat ####
santillana_etal <- list()
our_argo <- list()

# The script takes around 10 minutes to run, generating the entire results
# Real-case nowcast is available when dtname == "GT"
Sys.setenv(TZ = "UTC")
exogen <- Realdata_xts
exogen<-exogen[1:108]
yx_merged <- merge(Saledata_xts, exogen, join = "right")
y <- yx_merged$Saledata_xts
y<-y[1:108]
santillana_etal[["OBI"]] <-
  argo(y, exogen = exogen,
       alpha = 1, N_lag = NULL, use_all_previous = TRUE,  N_training = 24)  #不加时间序列

our_argo[["baidu"]] <-
  argo(logit(y / 100), exogen = log((exogen + 0.5) / 100),
       alpha = 1, N_lag=1:12, use_all_previous = FALSE, N_training = 14)   #argo

ar3 <- argo(y, alpha = NA, use_all_previous = FALSE, N_lag=1:12,
            N_training = 24)  #无惩罚项，无百度指数，结果不稀疏

#ar3_2<- argo(y, exogen = exogen,
#                alpha = NA, N_lag=1:3 , use_all_previous =FALSE,
#                N_training =50)


#### presentation and plot ####
start <- which(colnames(our_argo$baidu$coef)=="2010-01-01")
end <- which(colnames(our_argo$baidu$coef)=="2015-12-01")
ARGO_coef <- our_argo$baidu$coef[, start:end]



ARGO_coef_blend <- data.matrix(ARGO_coef[,-1], rownames.force = TRUE)


ARGO_coef_blend <- ARGO_coef_blend[,setdiff(colnames(ARGO_coef_blend),colnames(ARGO_coef))]
ARGO_coef_blend <- merge(ARGO_coef, ARGO_coef_blend, by = "row.names", all=TRUE)
rownames(ARGO_coef_blend) <- ARGO_coef_blend[,1]
ARGO_coef_blend <- data.matrix(ARGO_coef_blend[,-1], rownames.force = TRUE)

pred_xts_blend <- merge(Saledata_xts, logit_inv(our_argo$baidu$pred)*100,
                        santillana_etal$OBI$pred,ar3$pred, all=FALSE)


names(pred_xts_blend) <- c("销量数据", "ARGO", "Santillana", "AR3")

pred_xts_blend$naive <- c(NA, as.numeric(pred_xts_blend$销量数据[-nrow(pred_xts_blend)]))

pred_xts_blend$ARGO[zoo::index(our_argo$baidu$pred)] <- logit_inv(our_argo$baidu$pred[73:96])*100


pred_xts_blend$Santillana[zoo::index(santillana_etal$OBI$pred)] <- santillana_etal$OBI$pred[73:96]


pred_xts_blend <- pred_xts_blend["2010-01-01/"]

model_names <- c("ARGO","Santillana","AR3","naive")
legend_names <- c("ARGO","Santillana_etal","AR(3)","Naive")

zoom_periods <- c("2010-01-01/2010-12-01",
                  "2011-01-01/2011-12-01",
                  "2012-01-01/2012-12-01",
                  "2013-01-01/2013-12-01",
                  "2014-01-01/2014-12-01",
                  "2015-01-01/2015-12-01"
)

argo_cut_date <- tail(zoo::index(our_argo$baidu$pred),1)+1  #只截取最后若干项

setwd("C:/Users/Administrator/Desktop/Baidu all")
pdf("final_plot.pdf", height=11,width=12)
plot_all2(pred_xts_blend, argo_cut_date, model_names[1:4], legend_names[1:4], zoom_periods)
dev.off()
pdf(paste0("heatmap.pdf"),width=4, height=10)
heatmap_argo(ARGO_coef_blend, 0.1)
dev.off()

all_tab <- summary_argo(pred_xts_blend, model_names, legend_names, zoom_periods)

corr_header <- all_tab$corr_print[1,,drop=F]
rownames(corr_header) <- "\\textbf{Correlation}\\hfill\\vadjust{}"
corr_header[,] <- NA

rmse_header <- all_tab$rmse_print[1,,drop=F]
rownames(rmse_header) <- "\\textbf{RMSE}\\hfill\\vadjust{}"
rmse_header[,] <- NA

abse_header <- all_tab$abse_print[1,,drop=F]
rownames(abse_header) <- "\\textbf{MAE}\\hfill\\vadjust{}"
abse_header[,] <- NA

mape_header <- all_tab$mape_print[1,,drop=F]
rownames(mape_header) <- "\\textbf{MAPE}\\hfill\\vadjust{}"
mape_header[,] <- NA

corr_diff_header <- all_tab$corr_diff_print[1,,drop=F]
rownames(corr_diff_header) <- "\\textbf{Corr. of increment}\\hfill\\vadjust{}"
corr_diff_header[,] <- NA


big_tab_print <- rbind(
  rmse_header, all_tab$rmse_print,
  abse_header, all_tab$abse_print,
  mape_header, all_tab$mape_print,
  corr_header, all_tab$corr_print,
  corr_diff_header, all_tab$corr_diff_print)



blank_suffix <- sapply(1:(nrow(big_tab_print)/(nrow(all_tab$rmse_print)+1)), function(i)
  paste(rep(" ",i), collapse = ""))
rownames(big_tab_print) <-
  paste0(rownames(big_tab_print), rep(blank_suffix, each=(nrow(all_tab$rmse_print)+1)))

print(xtable::xtable(big_tab_print), sanitize.text.function=identity,
      sanitize.rownames.function=identity)

rela_effi <- sapply(model_names[-1], function(model_bench)
  bootstrap_relative_efficiency(
    na.omit(pred_xts_blend), model_names[1], model_bench, l=36, sim = "geom"))
rela_effi <- t(rela_effi)
colnames(rela_effi) <- c("point estimate",
                         paste(rep(c("basic","normal","percent"), each=2),
                               c("95% CI lower bond", "95% CI upper bond")))
rownames(rela_effi) <- legend_names[-1]
print(xtable::xtable(rela_effi))



