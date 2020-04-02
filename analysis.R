unzip('activity.zip')
data<-read.csv('activity.csv')
file.remove('activity.csv')

#####################################################################################################

par(mfrow = c(1,1))
sdata<-tapply(data$steps,data$date,sum,na.rm=TRUE)
total.steps.per.day<-sdata
hist(total.steps.per.day, xlab = "Total Steps per Day")

#####################################################################################################

mean_data<-mean(sdata)
print(paste('Mean:',as.character(mean_data)))
median_data<-median(sdata)
print(paste('Median:',as.character(median_data)))

#####################################################################################################


mean_across_interval<-tapply(data$steps,data$interval,mean,na.rm=TRUE)
x_val<-as.numeric(names(mean_across_interval))
plot(x_val,mean_across_interval,type="l")
max_value<-which.max(mean_across_interval)
mean_across_interval[max_value]

#####################################################################################################

data<-read.csv('activity.csv')
sum(!complete.cases(data))

data1<-data

for(i in 1:17568)
{
  if(is.na(data1$steps[i]))
  {
    data1$steps[i]<-mean_across_interval[as.character(data1$interval[i])]
  }
}

sdata1<-tapply(data1$steps,data1$date,sum)
total.steps.per.day.after.correction<-sdata1

par(mfrow = c(1,2))
hist(total.steps.per.day.after.correction, xlab = "Total steps per day", ylim = c(0,40))
hist(total.steps.per.day, xlab = "Total Steps per Day", ylim = c(0,40))

mean_data1<-mean(sdata1)
median_data1<-median(sdata1)

#####################################################################################################


data1$day_of_week<-weekdays(as.Date(data1$date))
data1$weekend<- data1$day_of_week=="Saturday" | data1$day_of_week =="Sunday"
tempList<-split(data1,factor(data1$weekend))
df1<-tempList$'FALSE'
df2<-tempList$'TRUE'


par(mfrow = c(2,1))

mean_across_interval_df1<-tapply(df1$steps,df1$interval,mean,na.rm=TRUE)
x_val_df1<-as.numeric(names(mean_across_interval_df1))
plot(x_val_df1,mean_across_interval_df1,type="l", xlab = "interval", ylab = "mean steps", ylim = c(0,250))
title(main = "Mean steps across each interval on Weekdays")

mean_across_interval_df2<-tapply(df2$steps,df2$interval,mean,na.rm=TRUE)
x_val_df2<-as.numeric(names(mean_across_interval_df2))
plot(x_val_df2,mean_across_interval_df2,type="l", xlab = "interval", ylab = "mean steps", ylim = c(0,250))
title(main = "Mean steps across each interval on Weekends")

#####################################################################################################