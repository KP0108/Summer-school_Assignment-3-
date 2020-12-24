Jan_19$date <- as.POSIXct(paste(Jan_19$Date, Jan_19$Time), format="%m/%d/%Y %H:%M:%S")


install.packages("readr")

library(readr)

Jan <- read_csv("Workshop dataset/Jan.csv")

Feb <- read_csv("Workshop dataset/Feb.csv")

Mar <- read_csv("Workshop dataset/Mar.csv")

Apr <- read_csv("Workshop dataset/Apr.csv")

May <- read_csv("Workshop dataset/May.csv")

#Jan <- Jan[,-1]

r

#Mar <- read_csv("H/Mar.csv")Jan <- read_csv("H/Jan.csv")

Yearly_data <- rbind(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)


Yearly_data$Time <- as.POSIXct(strptime(Yearly_data$Time, format = "%m/%d/%Y %H:%M"))


#Mar$Time <- as.POSIXct(strptime(Mar$Time, format = "%m/%d/%Y %H:%M"))

#Feb <- Feb[,(-1)]


install.packages("dplyr")

library(dplyr)

Yearly_data <- Yearly_data %>%
  mutate(Hour= format(Yearly_data$Time,"%H"),
         minute= format(Yearly_data$Time,"%M"),
         Day = as.numeric(format(Yearly_data$Time,"%d")),
         Month = as.numeric(format(Yearly_data$Time,"%m")),
         DayOfWeek = weekdays(Yearly_data$Time,abbreviate = FALSE))



weekday1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')



Yearly_data <- Yearly_data %>%
  
  mutate(DayOfWeek = weekdays(Yearly_data$Time,abbreviate = FALSE),
         
         Weekday_weekend=factor((DayOfWeek %in% weekday1),
                                
                                levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')))


#Yearly_data <- Yearly_data[c(1,45:50, 2:44)]  


library(dplyr)

install.packages("lubridate")
library(lubridate)

Complete_days <- Yearly_data %>%
  mutate(Day = floor_date(Time, unit = "day")) %>%
  group_by(Day) %>%
  mutate(nObservation = n()) %>%
  filter(nObservation == max(nObservation))


dfNew <- Complete_days[Complete_days$nObservation == 1440,]    


write.csv(dfNew, 'new_data.csv')


#write.csv(hourly_data_all, 'old hourly data.csv')

getwd()


## Average_hourly data
Average_hourly_data <- X1_min_processed_data %>%
  
  group_by(Month, Hour) %>%
  
  summarise(hourly_mean=mean(P_tot)*60,
            hourly_mean_motion=mean(M_tot)*60)

#Average_hourly_data$seq <- rep(seq(from=0, to=23, by=1),12)
Average_hourly_data$Month <- as.factor(Average_hourly_data$Month)

#install.packages("ggplot2")

library(ggplot2)


ggplot(Average_hourly_data, aes(x= Hour, y= hourly_mean, color= Month)) + geom_line() + labs(x = "Time of day", y= "Average energy consumption (Wh)")+ ggtitle("Average hourly energy consumption")
ggplot(Average_hourly_data, aes(x= Hour, y= hourly_mean_motion, color= Month)) + geom_line() + labs(x = "Time of day", y= "Average movements detected")


summary(Average_hourly_data$hourly_mean)


## daytype
hourly_daytp_power_data <- X1_min_processed_data %>%
  
  group_by(DayOfWeek, Hour) %>%
  
  summarise(hourly_mean1=mean(P_tot)*60)

#monthly_data$seq <- seq(from=1, to=12, by=1)

hourly_daytp_power_data$DayOfWeek <- as.factor(hourly_daytp_power_data$DayOfWeek)

hourly_daytp_power_data$DayOfWeek <- ordered(hourly_daytp_power_data$DayOfWeek, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                                                         "Friday", "Saturday", "Sunday"))

hourly_daytp_power_data <- hourly_daytp_power_data[order(hourly_daytp_power_data$DayOfWeek), ]

library(ggplot2)

ggplot(hourly_daytp_power_data, aes(x= Hour, y= hourly_mean1, color= DayOfWeek)) + geom_line() + labs(x = "Time of day", y= "Average energy consumption (Wh)")+ ggtitle("Average hourly energy consumption")


## hourly data
hourly_data <- X1_min_processed_data %>%
    group_by(Day, Hour, Month, DayOfWeek, Weekday_weekend) %>%
    summarise(Total_power = sum(P_tot),
            Total_motion = sum(M_tot),
            Average_CO2 = mean(Avg_CO2),
            Average_Temp = mean(Avg_Temp),
            Average_RH = mean(Avg_RH))


hourly_data$Month <- as.factor(hourly_data$Month)

hourly_data <- hourly_data[order(hourly_data$Month), ]

hourly_data[c(8:10)] <- round(hourly_data[c(8:10)], digits = 0)

#hourly_data$Day <- as.POSIXct(strptime(hourly_data$Day, format = "%d-%m-%Y"))

hourly_data$daynum <- yday(hourly_data$Day)


hourly_data$daynum <- as.factor(hourly_data$daynum)

hourly_data$Hour <- as.integer(hourly_data$Hour)

library(ggplot2)
ggplot(hourly_data, aes(x= Hour, y=Total_power, color= daynum)) + geom_line() + labs(x = "Time of day", y= "Total energy consumption (Wh)")+ ggtitle("Total energy consumption") + theme(legend.position = "none")

#boxplot(Total_power~Month, data=hourly_data, main="Daily energy consumption-Minami 182", font.main=3, cex.main=1.2, xlab="Month", ylab="Total energy consumption(wh)", font.lab=3, col="yellow",outline=F)


#daily data

daily_data <- X1_min_processed_data %>%
  
  group_by(Month, Day) %>%
  
  summarise(daily_total=sum(P_tot))

daily_data$Month <- as.factor(daily_data$Month)

boxplot(daily_total~Month, data=daily_data, main="Daily energy consumption", font.main=3, cex.main=1.2, xlab="Month", ylab="Total energy consumption(wh)", font.lab=3, col="yellow", outline=F)

#monthly data

monthly_data <- X1_min_processed_data %>%
  
  group_by(Month) %>%
  
  summarise(month_total=sum(P_tot))

monthly_data$seq <- seq(from=1, to=12, by=1)

#season analysis

Func.season <- function(month){
  
  Winter <- c("1", "2","12")
  
  Spring <- c("3", "4", "5")
  
  Summer <- c("6","7","8")
  
  Autumn <- c("9","10","11")
  
  season_division <- c("Winter","Spring", "Summer", "Autumn")
  
  x <- vector()
  
  
  
  for(i in 1:length(month)){
    
    if(month[i] %in% Winter){
      
      x[i] = season_division[1]
      
    }else if(month[i] %in% Spring){
      
      x[i] = season_division[2]
      
    }else if(month[i] %in% Summer){
      
      x[i] = season_division[3]
      
    }else{
      
      x[i] = season_division[4]
      
    }
    
  }
  
  
  
  return(x)
  
}



X1_min_processed_data$Season <- Func.season(X1_min_processed_data$Month)


Winter <- X1_min_processed_data %>%
  
  filter(Season=="Winter")

Summer <- X1_min_processed_data %>%
  
  filter(Season=="Summer")

Autumn <- X1_min_processed_data %>%
  
  filter(Season=="Autumn")

Spring <- X1_min_processed_data %>%
  
  filter(Season=="Spring")



## Winter_Average_hourly data
Winter_Average_hourly_data <- Winter %>%
  
  group_by(Hour) %>%
  
  summarise(hourly_mean=mean(P_tot)*60,
            hourly_mean_motion=mean(M_tot)*60)


## Summer_Average_hourly data
Summer_Average_hourly_data <- Summer %>%
  
  group_by(Hour) %>%
  
  summarise(hourly_mean=mean(P_tot)*60,
            hourly_mean_motion=mean(M_tot)*60)



## Spring_Average_hourly data
Spring_Average_hourly_data <- Spring %>%
  
  group_by(Hour) %>%
  
  summarise(hourly_mean=mean(P_tot)*60,
            hourly_mean_motion=mean(M_tot)*60)


## Autumn_Average_hourly data
Autumn_Average_hourly_data <- Autumn %>%
  
  group_by(Hour) %>%
  
  summarise(hourly_mean=mean(P_tot)*60,
            hourly_mean_motion=mean(M_tot)*60)

plot(Autumn_Average_hourly_data$hourly_mean_motion, type='l', col='blue', ylab='Average movements detected', xlab='Hours', lwd=3, cex.axis=1, cex.lab=1)
#,xaxt='n')
#axis(1,c(0,2,4,6,8,10,12,14,16,18,20,22,24))
lines(Summer_Average_hourly_data$hourly_mean_motion, type='l', col='orange', lwd=3)
lines(Winter_Average_hourly_data$hourly_mean_motion, type='l', col='green', lwd=3)
lines(Spring_Average_hourly_data$hourly_mean_motion, type='l', col='red', lwd=3)
legend(
  "topleft", 
  col = c('Blue', 'Orange', 'Green', 'Red', 'Black'),
  legend = c("Autumn", "Summer", "Winter", "Spring"), lty = 1, bty = 'n', cex = 1, lwd=3)




with(Winter_Average_hourly_data, plot(Hour, hourly_mean_motion, type='l', col='red'))

par(new=TRUE)

with(Winter_Average_hourly_data, plot(Hour, hourly_mean, type='l', lwd=2, pch=16, axes=F,xlab=NA, ylab=NA, cex=1.2, col="green"))

axis(side=4)
mtext(side=4, line=3, 'hourly_mean')
legend("topleft",
       legend=c(expression(M_tot), expression(P_Tot)),
       lty=c(1,1), pch=c(NA, NA), col=c("red", "green"))

#hourly data new try

#X1_min_processed_data$Day <- as.POSIXct(strptime(X1_min_processed_data$Day, format = "%d-%m-%Y",tz = 'Europe/Paris'))

hourly_data_method_2 <- X1_min_processed_data %>%
  
  group_by(Day, Hour) %>%
  
  summarise(P1=sum(P_1), P2=sum(P_2),P3=sum(P_3), P4=sum(P_4), P5=sum(P_5),
            P6=sum(P_6), P7=sum(P_7), P8=sum(P_8),P9=sum(P_9),
            P10=sum(P_10),P11=sum(P_11),P12=sum(P_12),P13=sum(P_13),
            P14=sum(P_14),P15=sum(P_15),
            M1=sum(M_1),M2=sum(M_2),M3=sum(M_3),M4=sum(M_4),
            M5=sum(M_5),M6=sum(M_6),M7=sum(M_7),M8=sum(M_8),
            M9=sum(M_9),M10=sum(M_10),M11=sum(M_11),M12=sum(M_12),M13=sum(M_13),M14=sum(M_14),
            CO2_LR = mean(CO2_1), CO2_BR1 = mean(CO2_2), CO2_BR2=mean(CO2_3), CO2_BR3 =mean(CO2_4),
            temp_LR=mean(T_1), temp_BR1 = mean(T_2), temp_BR2=mean(T_3),temp_BR3=mean(T_4),
            RH_LR=mean(RH_1),RH_BR1=mean(RH_2), RH_BR2=mean(RH_3), RH_BR3=mean(RH_4))

hourly_data_method_2_tot <- hourly_data_method_2 %>%
  mutate(P_tot = P1+P2+P3+P4+P5+P6+P7+P8+P9+P10+P11+P12+P13+P14+P15,
         M_tot = M1+M2+M3+M4+M5+M6+M7+M8+M9+M10+M11+M12+M13+M14)


hourly_data_method_2_tot[c(32:43)] <-  round(hourly_data_method_2_tot[c(32:43)], digits = 0)


library('lubridate')

hourly_data_method_2_tot$Day_name <- weekdays(hourly_data_method_2_tot$Day)

weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')


hourly_data_method_2_tot$wDay <- factor((weekdays(hourly_data_method_2_tot$Day) %in% weekdays1),
                               
                               levels=c(FALSE,TRUE), labels=c('weekend','weekday'))

hourly_data_method_2_tot <- hourly_data_method_2_tot[c(1,2,46,47,3:45)]


hourly_profile_method_2 <- hourly_data_method_2_tot %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot))

hourly_profile_method_2[c(2,3)] <- round(hourly_profile_method_2[c(2,3)], digits = 0)

with(hourly_profile_method_2, plot(Hour, M_tot_wd, type='l', col='red'))

par(new=TRUE)

with(hourly_profile_method_2, plot(Hour, P_tot_wd, type='l', pch=16, axes=F,xlab=NA, ylab=NA, cex=1.2))


hourly_profile_method_2_weekdays <- filter(hourly_data_method_2_tot, wDay == 'weekday')
hourly_profile_method_2_weekend <- filter(hourly_data_method_2_tot, wDay == 'weekend')

#Weekday analysis

hourly_profile_method_2_weekdays_analysis <- hourly_profile_method_2_weekdays %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot))


hourly_profile_method_2_weekdays_analysis[c(2,3)] <- round(hourly_profile_method_2_weekdays_analysis[c(2,3)], digits = 0)

with(hourly_profile_method_2_weekdays_analysis, plot(Hour, M_tot_wd, type='l', col='red'))

par(new=TRUE)

with(hourly_profile_method_2_weekdays_analysis, plot(Hour, P_tot_wd, type='l', pch=16, axes=F,xlab=NA, ylab=NA, cex=1.2))
axis(side=4)
mtext(side=4, line=3, 'P_Tot')
legend("topleft",
       legend=c(expression(M_tot), expression(P_Tot)),
       lty=c(1,1), pch=c(NA, 16), col=c("red", "black"))


#Weekend analysis

hourly_profile_method_2_weekend_analysis <- hourly_profile_method_2_weekend %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), M_tot_wd = mean(M_tot))


hourly_profile_method_2_weekend_analysis[c(2,3)] <- round(hourly_profile_method_2_weekend_analysis[c(2,3)], digits = 0)

with(hourly_profile_method_2_weekend_analysis, plot(Hour, M_tot_wd, type='l', col='red'))

par(new=TRUE)

plot(hourly_profile_method_2_weekdays_analysis$Hour, hourly_profile_method_2_weekdays_analysis$P_tot_wd, type = 'l', col='green')
lines(hourly_profile_method_2_weekend_analysis$Hour, hourly_profile_method_2_weekend_analysis$P_tot_wd, type = 'l', col='red')


with(hourly_profile_method_2_weekend_analysis, plot(Hour, P_tot_wd, type='l', pch=16, axes=F,xlab=NA, ylab=NA, cex=1.2))
axis(side=4)
mtext(side=4, line=3, 'P_Tot')
legend("topleft",
       legend=c(expression(M_tot), expression(P_Tot)),
       lty=c(1,0), pch=c(NA, 16), col=c("red", "black"))



#### Occupancy data analysis ####

library('reshape2')

Occupancy_data <- dcast(hourly_data, Day~Hour, value.var = 'Total_motion')

library(RColorBrewer)

library(pheatmap)

colors<-colorRampPalette(rev(brewer.pal(n=7,name="Spectral")))(7)

pheatmap::pheatmap(as.matrix(Occupancy_data[c(2:25)]), treeheight_row = 0, treeheight_col = 0,
                   cluster_rows=F, cluster_cols=F, col=colors, legend = TRUE, legend_labels = 'No.of.occupants', main = 'Occupancy data', xlab = 'Hour', cex=1.2)



library(cluster)
library(clusterCrit)

library('dtwclust')

library('TSclust')

library(data.table)

Occupancy_data_matrix <- as.matrix(Occupancy_data[c(2:25)])

clusterings_wd_Type_1_c1 <- lapply(c(2:9), function(x)
  pam((as.matrix(Occupancy_data_matrix)), x))


DB_values_wd_Type_1_c1 <- sapply(seq_along(clusterings_wd_Type_1_c1), function(x) 
  intCriteria(Occupancy_data_matrix, as.integer(clusterings_wd_Type_1_c1[[x]]$clustering),
              c("Dunn")))



ggplot(data.table(Clusters = 2:9, Dunn_index = unlist(DB_values_wd_Type_1_c1)),
       aes(Clusters, Dunn_index)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_bw() + ggtitle("CVI")


dtw_cluster_wd_type_1_c1 = tsclust(Occupancy_data_matrix, type="partitional",k=3, preproc = zscore,seed=110,
                                   distance="sbd",centroid = "shape",trace=T, na.rm=TRUE)

plot(dtw_cluster_wd_type_1_c1)

plot(dtw_cluster_wd_type_1_c1, type = "series", clus = 1L)
plot(dtw_cluster_wd_type_1_c1, type = "centroids", clus = 3L)


### FILTERING MOTION == 0 data ####

microbenchmark(Nonzero <- hourly_data %>% filter(Total_motion == 0),
               times = 24)

#### FILTERING DAYS WITH CONTINUOUS MOTION 0 DATA #####

#Nonzero$Hour <- as.numeric(Nonzero$Hour)

contzero <- Nonzero[(1+(s<-c(0,cumsum(1-(diff(Nonzero$Hour)==1)))))%in%which(table(s)>=23),]

Days_with_Zero_Motion <- (unique(contzero$Day))

### REMOVING CONTINUOUS ZERO MOTION DAYS WITH THE MAIN DATA i.e. hourly_data_112 ####

#dates <- as.POSIXct(c("2016-01-05",  "2016-01-06",  "2016-01-07",  "2016-03-31", "2016-05-17", "2016-05-18", "2016-06-13", "2016-06-14",
                    #  "2016-06-15", "2016-06-16", "2016-06-20", "2016-07-12", "2016-07-13", "2016-07-14", "2016-07-15", "2016-07-18",
                     # "2016-07-19" ,"2016-07-20", "2016-07-21", "2016-07-22", "2016-08-11", "2016-08-12"))


A_182 <- hourly_data

A_182_Nonzerodays <- A_182[!as.Date(A_182$Day) %in% as.Date(c("2016-02-21", "2016-02-22", "2016-02-26", "2016-02-27", "2016-06-05", "2016-06-19", "2016-06-20",
                                                              "2016-07-03", "2016-07-17", "2016-07-27", "2016-07-28", "2016-07-29", "2016-07-30", "2016-07-31",
                                                              "2016-08-06", "2016-08-09", "2016-08-10", "2016-08-11", "2016-08-12", "2016-08-13", "2016-08-14",
                                                              "2016-08-15", "2016-08-18", "2016-08-19", "2016-08-25", "2016-08-26", "2016-08-27", "2016-10-20",
                                                              "2016-10-22", "2016-10-23", "2016-10-24", "2016-10-31", "2016-12-20", "2016-12-21", "2016-12-22",
                                                              "2016-12-23", "2016-12-24", "2016-12-25", "2016-12-28", "2016-12-29", "2016-12-30")), ]

#plugpower consumption profile when there is no occupants in the apartment

No_occ_hourly_profile <- A_182_Nonzerodays_1 %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(Total_power), M_tot_wd = mean(Total_motion))

#plugpower consumption profile when there is occupant present in the apartment

Occ_hourly_profile <- A_182_Nonzerodays %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(Total_power), M_tot_wd = mean(Total_motion))

plot(x=Occ_hourly_profile$Hour, y=Occ_hourly_profile$P_tot_wd, type="l", ylim=c(0,600))
lines(x=No_occ_hourly_profile$Hour, y=No_occ_hourly_profile$P_tot_wd, type = 'l', col='red')


with(No_occ_hourly_profile, plot(Hour, P_tot_wd, type='l', col='red'))

par(new=TRUE)

with(Occ_hourly_profile, plot(Hour, P_tot_wd, type='l', lwd=2, pch=16, axes=F,xlab=NA, ylab=NA, cex=1.2, col="green"))


Occupancy_data_NZ <- dcast(A_182_Nonzerodays, Day~Hour, value.var = 'Total_motion')

Occupancy_data_NZ_matrix <- as.matrix(Occupancy_data_NZ[c(2:25)])

clusterings_wd_Type_1_c1 <- lapply(c(2:9), function(x)
  pam((as.matrix(Occupancy_data_NZ_matrix)), x))


DB_values_wd_Type_1_c1 <- sapply(seq_along(clusterings_wd_Type_1_c1), function(x) 
  intCriteria(Occupancy_data_NZ_matrix, as.integer(clusterings_wd_Type_1_c1[[x]]$clustering),
              c("Dunn")))



ggplot(data.table(Clusters = 2:9, Dunn_index = unlist(DB_values_wd_Type_1_c1)),
       aes(Clusters, Dunn_index)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_bw() + ggtitle("CVI")


dtw_cluster_wd_type_1_c1 = tsclust(Occupancy_data_NZ_matrix, type="partitional",k=2, preproc = zscore,seed=100,
                                   distance="sbd",centroid = "shape",trace=T, na.rm=TRUE)

plot(dtw_cluster_wd_type_1_c1)

plot(dtw_cluster_wd_type_1_c1, type = "series", clus = 1L)
plot(dtw_cluster_wd_type_1_c1, type = "centroids", clus = 1L)


Occupancy_data_NZ$cluster <- dtw_cluster_wd_type_1_c1@cluster

cluster_1 <- subset(Occupancy_data_NZ, Occupancy_data_NZ$cluster==1)

#cluster_1 <- cluster_1[-c(46, 51, 52), ] #(Removing occupant movements with 60 and 120)

cluster_2 <- subset(Occupancy_data_NZ, Occupancy_data_NZ$cluster==2)

pheatmap::pheatmap(as.matrix(cluster_2[c(2:25)]), treeheight_row = 0, treeheight_col = 0, cluster_rows=F, cluster_cols=F, col=colors, main = 'Cluster 2', cex=1.2)


a <- rep(Occupancy_data_NZ$cluster, each=24)

A_182_Nonzerodays$cluster <- a

cluster_1_OD <- subset(A_182_Nonzerodays, A_182_Nonzerodays$cluster==1)

cluster_2_OD <- subset(A_182_Nonzerodays, A_182_Nonzerodays$cluster==2)


#M_Cluster_1_mean_profile_hourly_analysis

Cluster_1_mean_profile_type_1 <- cluster_1_OD %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(Total_power), M_tot_wd = mean(Total_motion))

Cluster_1_mean_profile_type_1[c(2,3)] <- round(Cluster_1_mean_profile_type_1[c(2,3)], digits = 0)

with(Cluster_1_mean_profile_type_1, plot(Hour, M_tot_wd, type='l', col='red'))

par(new=TRUE)

with(Cluster_1_mean_profile_type_1, plot(Hour, P_tot_wd, type='l', lwd=2, pch=16, axes=F,xlab=NA, ylab=NA, cex=1.2, col="green"))

axis(side=4)
mtext(side=4, line=3, 'hourly_mean')
legend("topleft",
       legend=c(expression(M_tot), expression(P_Tot)),
       lty=c(1,1), pch=c(NA, NA), col=c("red", "green"))


#M_Cluster_2_mean_profile_hourly_analysis

Cluster_2_mean_profile_type_1 <- cluster_2_OD %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(Total_power), M_tot_wd = mean(Total_motion))

Cluster_2_mean_profile_type_1[c(2,3)] <- round(Cluster_2_mean_profile_type_1[c(2,3)], digits = 0)

with(Cluster_2_mean_profile_type_1, plot(Hour, M_tot_wd, type='l', col='red'))

par(new=TRUE)

with(Cluster_2_mean_profile_type_1, plot(Hour, P_tot_wd, type='l', lwd=2, pch=16, axes=F,xlab=NA, ylab=NA, cex=1.2, col="green"))

axis(side=4)
mtext(side=4, line=3, 'hourly_mean')
legend("topleft",
       legend=c(expression(M_tot), expression(P_Tot)),
       lty=c(1,1), pch=c(NA, 16), col=c("red", "green"))
