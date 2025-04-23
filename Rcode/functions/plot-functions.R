# functions to plot temperature files in R
# in progress - need to incorporate all common figures - 
# - any figures that are being copy and pasted to make new figures on a regular basis

# things to adjust for the following plot -> 
# add default values
# if no entry, assume:
# interval = 0
# region = all regions

week.plot<-function(data,date.input,interval, region) {
  
  range<-data%>%
    mutate(Date=date(Date.Time))%>%
    filter(Date>=date(date.input) & Date<=date(date.input)+interval)%>%
    filter(Region==region)
  
  w <- ggplot()+
    geom_line(data=filter(range,Recording!='Air'),aes(x=Date.Time,y=Temp.C),size=.7)+
    geom_line(data=filter(range,Recording=='Air'),aes(x=Date.Time, y=Temp.C),size=.5,linetype='dashed',colour='red')+
    facet_wrap(~River,ncol=2)+
    geom_hline(yintercept=20,linetype='dashed')+
    labs(x="Time",
         y="Hourly Temperature")+
    ggtitle(paste(format(min(range$Date), '%d %b'), '-', format(max(range$Date), '%d %b %Y'), sep=" "))+
    # scale_x_datetime(date_breaks = "12 hours",date_labels = '%d-%b %H:%M')+
    theme_bw()+
    theme(axis.text.x = element_text(angle=90,vjust=0),
          plot.title = element_text(hjust = .5))
  print(w)
}

week.plot(tlong,'2022-08-01',7,'Avalon-East')

week.plot(tlong,'2022-08-01',7,'Central-West')
