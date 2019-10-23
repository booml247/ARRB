#' Time series plot of ARGO applied on CDC's ILI data
#'
#' This function is only used to reproduce the ARGO plot of PNAS paper.
#' If you use this plotting routine for other dataset, an error is highly likely to occur.
#'
#' @importFrom zoo index
#'
#' @return a graph on the default plot window
#'
#' @export
plot_all2<- function(GFT_xts, GC_GT_cut_date, model_names, legend_names, zoom_periods){
  par_orig <- par()[c("mar","mfrow")]
  cutting_dates <- which(index(GFT_xts) %in% c(as.Date("2014-02-01"), GC_GT_cut_date-1))#GC_GT_cut_date-94608001
  
  color_code <- c("red","orange", "forestgreen", "goldenrod3")
  term_change_col <- "khaki"
  cdc_lwd <- 2
  line_width <- c(1.5,1.5,2,2,2)
  line_type <- c(1,1,4,4,4)
  
  layout(rbind(rep(1,3),rep(2,3)), heights=c(2.5/3.5*5,6/3.5))
  par(mar=c(4.1, 2.1, 1.1, 1))
  xts::plot.xts(GFT_xts$Salesvolume, ylim=c(0.4,1.0),bar.col="yellow", type='p', auto.grid = FALSE, main=NA,pch=1)
  points(GFT_xts$ARGO,ylim=c(0.2,1.0), type='p', auto.grid = FALSE, main=NA,pch=2)
  abline(v=xts::.index(GFT_xts)[cutting_dates[1]], col=term_change_col, lwd=2, lty=4)
  rect(xts::.index(GFT_xts)[cutting_dates[2]], -10, xts::.index(GFT_xts)[nrow(GFT_xts)], 100,
       border=NA, col="lightgoldenrodyellow")
  box()
  lines(GFT_xts$Salesvolume, lwd=cdc_lwd,col="grey5")
  for(i in 1:length(model_names)){
    lines(GFT_xts[,model_names[i]], col=color_code[i], lwd=line_width[i], lty=line_type[i])
  }
  
  legend(xts::.index(GFT_xts)[1], 1,c("Salesvolume",legend_names),
         col=c("grey5", color_code),
         lwd=c(cdc_lwd, line_width),
         lty=c(1,line_type), cex=1.5)
  
  #arrows(xts::.index(GFT_xts)[cutting_dates[1]-1],0,xts::.index(GFT_xts)[1],0,
  #       length=0.1, col="grey23", code=3)
  #text(xts::.index(GFT_xts)[cutting_dates[1]%/%2+1], 0,
  #     "Google Correlate", pos=3, offset=0.3, cex=1.25)
  #text(xts::.index(GFT_xts)[cutting_dates[1]%/%2+1], 0,
  #     "Search terms identified on 2009-03-28", pos=1, offset=0.3, cex=1.25)
  #arrows(xts::.index(GFT_xts)[cutting_dates[1]+1],0,xts::.index(GFT_xts)[cutting_dates[2]-1],0,
  #       length=0.1, col="grey23", code=3)
  #text(xts::.index(GFT_xts)[mean(cutting_dates)], 0,
  #     "Google Correlate", pos=3, offset=0.3, cex=1.25)
  #text(xts::.index(GFT_xts)[mean(cutting_dates)], 0,
  #     "Search terms identified on 2010-05-22", pos=1, offset=0.3, cex=1.25)
  #arrows(xts::.index(GFT_xts)[cutting_dates[2]+1],0,xts::.index(GFT_xts)[nrow(GFT_xts)-1],0,
  #       length=0.1, col="grey23", code=3)
  #text(xts::.index(GFT_xts)[(cutting_dates[2]+nrow(GFT_xts))%/%2+1], 0,
  #     "Google Trend", pos=3, offset=0.3, cex=1.25)
  #text(xts::.index(GFT_xts)[(cutting_dates[2]+nrow(GFT_xts))%/%2+1], 8,
  #     "Search terms\nidentified on\n2010-05-22", pos=1, offset=0, cex=1.25)
  
  old_mar <- par(mar=c(0.5,2.1,0.5,1))
  
  
  xts::plot.xts(GFT_xts$Salesvolume-GFT_xts[,model_names[i]], ylim=c(-0.5,0.5),main=NA,typ='n',
                xlab=NA, xaxt='n',auto.grid = FALSE)
  
  
  abline(v=xts::.index(GFT_xts)[cutting_dates[1]], col=term_change_col, lwd=2, lty=4)
  
  rect(xts::.index(GFT_xts)[cutting_dates[2]], -10, xts::.index(GFT_xts)[nrow(GFT_xts)], 100,
       border=NA, col="lightgoldenrodyellow")
  
  box()
  mtext("prediction error")
  abline(h=0,lty=3,lwd=cdc_lwd)
  for(i in length(model_names):1){
    lines(-GFT_xts$Salesvolume+GFT_xts[,model_names[i]], col=color_code[i],
          lwd=line_width[i], lty=line_type[i])
  }
  
  
  
  
  #zoom_periods_dates <- strsplit(zoom_periods, "/")
  #zoom_periods_dates <- lapply(zoom_periods_dates, as.Date)
  
  #legend_period <- lapply(1:length(zoom_periods_dates), function(i) {
  #  if(i == 1)
  #    l <- "H1N1 Flu outbreak"
  #  else if(i == length(zoom_periods_dates))
  #    l <- paste0(2008+i, "-",9+i,"\nFlu Season")
  #  else
  #    l <- paste0(2008+i, "-",9+i,"\nFlu Season")
  #  
  #  d <- paste(format(zoom_periods_dates[[i]], "%m/%d/%y"), collapse="\n     --\n")
  #  return(c(l, d))
  #})
  
  #par(mar=c(3,2.1,2.1,1))
  #zoom_id <- c(1,4,6)
  #for(k in 1:length(zoom_id)){
  #  period_counter <- zoom_id[k]
  #  plot_period <- zoom_periods[period_counter]
  #  plot(GFT_xts$Salesvolume[plot_period], ylim=c(1.2,8.5),
  #       main=NA, type='n', auto.grid = FALSE)
  #  lines(GFT_xts$Salesvolume[plot_period], lwd=cdc_lwd)
  #  for(i in length(model_names):1){
  #    lines(GFT_xts[,model_names[i]][plot_period], col=color_code[i], lwd=line_width[i], lty=line_type[i])
  #  }
  #  mtext(paste0("(",letters[k],")"))
  #  if(period_counter == 1){
  #    legend("topleft", rev(legend_period[[period_counter]]), bty='n', cex=1.5)
  #  }else{
  #    legend("topright", legend_period[[period_counter]][1], bty='n', cex=1.5)
  #    legend("topleft", legend_period[[period_counter]][2], bty='n', cex=1.5)
  #  }
  #}
  par(par_orig)
  invisible(NULL)
}