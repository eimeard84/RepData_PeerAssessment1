RepData <- function(){
  unzip("activity.zip")
  activity <- read.csv("activity.csv",header=TRUE)
  
  library(dplyr)
  
  png(filename="steps_with_missing_data.png")
  
  hist_data <- summarise(group_by(activity,date),sum(steps,na.rm=TRUE))
  hist(unlist(hist_data[,2]),col=c("Red","Green","Blue"),xlab="Total Number of Steps",ylab="Number of Days",main="Frequency of Total Number of Steps per Day")
  
  dev.off()
  
  total_steps <- summarise(group_by(activity,date),sum(steps,na.rm=TRUE))
    
  mean_value <- mean(total_steps$sum)
  median_value <- median(total_steps$sum)
  
  print("Mean Total Number of Steps per Day (ignoring NAs)")
  print(mean_value)
  print("Median Total Number of Steps per Day (ignoring NAs)")
  print(median_value)
  
  interval_steps <- summarise(group_by(activity,interval),mean=mean(steps,na.rm=TRUE))
  png(filename="timeseries_steps_per_interval.png")
  par(xaxt="n")
  plot(interval_steps,type='l',main="Mean Number of Steps in each Time Interval",xlab="Time of Day",ylab="Mean Number of Steps")
  par(xaxt="s")
  axis(1,at=c(0,600,1200,1800,2355),labels=c("0000","0600","1200","1800","0000"))
  dev.off()
  print("Time Interval of Max. Steps:")
  print(filter(interval_steps,mean==max(interval_steps$mean)))
  
  print("Total Number of Missing Values:")
  print(sum(is.na(activity$steps)))
  
  activity_new <- merge(activity,interval_steps)
  index <- is.na(activity_new$steps)
  activity_new$steps[index] <- activity_new$mean[index]
  
  hist_data <- summarise(group_by(activity_new,date),sum(steps,na.rm=TRUE))
  png(filename="steps_with_imputed_data.png")
  hist(unlist(hist_data[,2]),col=c("Red","Green","Blue"),xlab="Total Number of Steps",ylab="Number of Days",main="Frequency of Total Number of Steps per Day")
  dev.off()
  
  total_steps <- summarise(group_by(activity_new,date),sum(steps,na.rm=TRUE))
  
  mean_value <- mean(total_steps$sum)
  median_value <- median(total_steps$sum)
  
  print("Mean Total Number of Steps per Day (Imputed Data)")
  print(mean_value)
  print("Median Total Number of Steps per Day (Imputed Data)")
  print(median_value)
  
  weekday <- weekdays(as.Date(activity$date))
  weekday <- (weekday == "Saturday" | weekday == "Sunday")
  
  activity_new$fac_wd <- factor(x=weekday,levels=c(FALSE,TRUE),labels=c("Weekday","Weekend"))
  
  activity_weekday <- filter(activity_new,fac_wd=="Weekday")
  activity_weekend <- filter(activity_new,fac_wd=="Weekend")
  
  weekday_steps <- summarise(group_by(activity_weekday,interval),mean(steps))
  weekend_steps <- summarise(group_by(activity_weekend,interval),mean(steps))
  
  png(filename="weekday_vs_weekend.png")
  par(mfcol=c(2,1))
  par(xaxt="n")
  plot(weekday_steps,type='l',main="Weekday Steps",xlab="Time of Day",ylab="Mean Number of Steps")
  par(xaxt="s")
  axis(1,at=c(0,600,1200,1800,2355),labels=c("0000","0600","1200","1800","0000"))

  par(xaxt="n")
  plot(weekend_steps,type='l',main="Weekend Steps",xlab="Time of Day",ylab="Mean Number of Steps")
  par(xaxt="s")
  axis(1,at=c(0,600,1200,1800,2355),labels=c("0000","0600","1200","1800","0000"))
  dev.off()
}
