FMUC_EV_power_points_02_01_19$DT <- as.POSIXct(as.character(paste(FMUC_EV_power_points_02_01_19$Date, FMUC_EV_power_points_02_01_19$Time)), format="%m/%d/%Y %H:%M:%S")


library(dplyr)

hourlydata <- hourly_dorost_without_errors

hourlydata <- hourlydata %>%
  mutate(Month= format(hourlydata$DT,"%m"))


weekday1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')



hourlydata <- hourlydata %>%
  
  mutate(DayOfWeek = weekdays(hourlydata$DT,abbreviate = FALSE),
         
         Weekday_weekend=factor((DayOfWeek %in% weekday1),
                                
                                levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')))

#FInction season

Func.season <- function(month){
  
  Winter <- c("01", "02","12")
  
  Spring <- c("03", "04", "05")
  
  Summer <- c("06","07","08")
  
  Autumn <- c("09","10","11")
  
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



hourlydata$Season <- Func.season(hourlydata$Month)


hourlydata[c(3:15)] <- round(hourlydata[c(3:15)], digits = 0)


Winter <- filter(hourlydata, Season == 'Winter')

Summer <- filter(hourlydata, Season == 'Summer')

Autumn <- filter(hourlydata, Season == 'Autumn')

Spring <- filter(hourlydata, Season == 'Spring') 


boxplot(Point_3~Month, data = hourlydata, ylab = "kW", outline=F)
abline(v=c(1.5, 4.5, 7.5, 10.5), lty =3, lwd=5, col="red", )
boxplot(Point_3~Season, data = hourlydata, outline=F, ylab = "kW")


hourlydata$Month <- as.factor(hourlydata$Month)

hourlydata$Hour <- as.integer(hourlydata$Hour)


library(ggplot2)
ggplot(monthly_data, aes(x= Hour, y=Point_3_month, color= Month)) + geom_line() + labs(x = "Time of day", y= "Average energy consumption (Wh)")+ ggtitle("Minami 182, Average hourly energy consumption")

library(dplyr)
monthly_data <- hourlydata %>%
  
  group_by(Month, Hour) %>%
  
  summarise(Point_3_month=mean(Point_3))


## daytype
Daily_data <- hourlydata %>%
  
  group_by(DayOfWeek, Hour) %>%
  
  summarise(Point_3_month=mean(Point_3))

Daily_data$Month <- as.factor(Daily_data$DayOfWeek)

Daily_data$Hour <- as.integer(Daily_data$Hour)


ggplot(Daily_data, aes(x= Hour, y=Point_3_month, color= DayOfWeek)) + ylim(500,1800)+ geom_line(size=1) + theme_bw()+labs(x = "Time of day", y= "kW")+ ggtitle("Mechanical room_Daily load_For the entire 2019")


Daylabs <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
             "Friday", "Saturday", "Sunday")

Daily_data <- Daily_data[order(Daily_data$DayOfWeek),]

boxplot(Point_3_month~DayOfWeek, data = Daily_data, ylab = "kW", outline=F)

library(dplyr)


#Winter daily data
Daily_data_Winter <- Winter %>%
  
  group_by(DayOfWeek, Hour) %>%
  
  summarise(Point_3=mean(Point_3))

Daily_data_Winter$DayOfWeek <- factor(Daily_data_Winter$DayOfWeek, levels = Daylabs)

boxplot(Point_3~DayOfWeek, data = Daily_data_Winter, ylab = "kW", outline=F, main='Winter', ylim=c(500,1600))



#Summer daily data
Daily_data_Summer <- Summer %>%
  
  group_by(DayOfWeek, Hour) %>%
  
  summarise(Point_3=mean(Point_3))

Daily_data_Summer$Hour <- as.integer(Daily_data_Summer$Hour)

Daily_data_Summer$DayOfWeek <- factor(Daily_data_Summer$DayOfWeek, levels = Daylabs)

boxplot(Point_3~DayOfWeek, data = Daily_data_Summer, ylab = "kW", outline=F, main='Summer',ylim=c(500,1600))

#Spring daily data
Daily_data_Spring <- Spring %>%
  
  group_by(DayOfWeek, Hour) %>%
  
  summarise(Point_3=mean(Point_3))

Daily_data_Spring$Hour <- as.integer(Daily_data_Spring$Hour)

Daily_data_Spring$DayOfWeek <- factor(Daily_data_Spring$DayOfWeek, levels = Daylabs)

boxplot(Point_3~DayOfWeek, data = Daily_data_Spring, ylab = "kW", outline=F, main='Spring',ylim=c(500,1600))


#Autumn daily data
Daily_data_Autumn <- Autumn %>%
  
  group_by(DayOfWeek, Hour) %>%
  
  summarise(Point_3=mean(Point_3))

Daily_data_Autumn$Hour <- as.integer(Daily_data_Autumn$Hour)

Daily_data_Autumn$DayOfWeek <- factor(Daily_data_Autumn$DayOfWeek, levels = Daylabs)

boxplot(Point_3~DayOfWeek, data = Daily_data_Autumn, ylab = "kW", outline=F, main='Autumn',ylim=c(500,1600))



library(ggplot2)

par(mfrow = c(2,2))

ggplot(Daily_data_Winter, aes(x= Hour, y=Point_3, color= DayOfWeek)) + ylim(0,1800)+ geom_line(size=1) + theme_bw()+labs(x = "Time of day", y= "kW")+ ggtitle("Winter - Daily load profile")+theme(legend.position = "none") 


ggplot(Daily_data_Summer, aes(x= Hour, y=Point_3, color= DayOfWeek)) + ylim(500,1800)+ geom_line(size=1) + theme_bw()+labs(x = "Time of day", y= "kW")+ ggtitle("Summer - Daily load profile")

ggplot(Daily_data_Spring, aes(x= Hour, y=Point_3, color= DayOfWeek)) + ylim(500,1800)+ geom_line(size=1) + theme_bw()+labs(x = "Time of day", y= "kW")+ ggtitle("Spring - Daily load profile")


ggplot(Daily_data_Autumn, aes(x= Hour, y=Point_3, color= DayOfWeek)) + ylim(500,1800)+ geom_line(size=1) + theme_bw()+labs(x = "Time of day", y= "kW")+ ggtitle("Autumn - Daily load profile")



cols <- c("DayOfWeek", "Point_3")

summary(Daily_data_Autumn$DayOfWeek)


#Winter daily data_WEEKDY%WEEKEND
Daily_data_Winter_WE <- Winter %>%
  
  group_by(Weekday_weekend, Hour) %>%
  
  summarise(Point_3=mean(Point_3))

Daily_data_Winter_WE$Hour <- as.integer(Daily_data_Winter_WE$Hour)
#Daily_data_Winter$DayOfWeek <- factor(Daily_data_Winter$DayOfWeek, levels = Daylabs)

#boxplot(Point_3~DayOfWeek, data = Daily_data_Winter, ylab = "kW", outline=F, main='Winter', ylim=c(500,1600))



#Summer daily data
Daily_data_Summer_WE <- Summer %>%
  
  group_by(Weekday_weekend, Hour) %>%
  
  summarise(Point_3=mean(Point_3))

Daily_data_Summer_WE$Hour <- as.integer(Daily_data_Summer_WE$Hour)

#Daily_data_Summer$DayOfWeek <- factor(Daily_data_Summer$DayOfWeek, levels = Daylabs)

#boxplot(Point_3~DayOfWeek, data = Daily_data_Summer, ylab = "kW", outline=F, main='Summer',ylim=c(500,1600))

#Spring daily data
Daily_data_Spring_WE <- Spring %>%
  
  group_by(Weekday_weekend, Hour) %>%
  
  summarise(Point_3=mean(Point_3))

Daily_data_Spring_WE$Hour <- as.integer(Daily_data_Spring_WE$Hour)

#Daily_data_Spring$DayOfWeek <- factor(Daily_data_Spring$DayOfWeek, levels = Daylabs)

#boxplot(Point_3~DayOfWeek, data = Daily_data_Spring, ylab = "kW", outline=F, main='Spring',ylim=c(500,1600))


#Autumn daily data
Daily_data_Autumn_WE <- Autumn %>%
  
  group_by(Weekday_weekend, Hour) %>%
  
  summarise(Point_3=mean(Point_3))

Daily_data_Autumn_WE$Hour <- as.integer(Daily_data_Autumn_WE$Hour)

#Daily_data_Autumn$DayOfWeek <- factor(Daily_data_Autumn$DayOfWeek, levels = Daylabs)

#boxplot(Point_3~DayOfWeek, data = Daily_data_Autumn, ylab = "kW", outline=F, main='Autumn',ylim=c(500,1600))



library(ggplot2)

#par(mfrow = c(2,2))

ggplot(Daily_data_Winter_WE, aes(x= Hour, y=Point_3, color= Weekday_weekend)) + ylim(0,1800)+ geom_line(size=1) + theme_bw()+labs(x = "Time of day", y= "kW")+ 
  ggtitle("Winter - Weekly load profile")+theme(legend.position = "none")+theme(axis.text=element_text(size=14))


ggplot(Daily_data_Summer_WE, aes(x= Hour, y=Point_3, color= Weekday_weekend)) + ylim(0,1800)+ geom_line(size=1) + theme_bw()+labs(x = "Time of day", y= "kW")+ 
  ggtitle("Summer - Weekly load profile")+theme(legend.position = "none")+theme(axis.text=element_text(size=14))

ggplot(Daily_data_Spring_WE, aes(x= Hour, y=Point_3, color= Weekday_weekend)) + ylim(0,1800)+ geom_line(size=1) + theme_bw()+labs(x = "Time of day", y= "kW")+ 
  ggtitle("Spring - Weekly load profile")+theme(legend.position = "none")+theme(axis.text=element_text(size=14))


ggplot(Daily_data_Autumn_WE, aes(x= Hour, y=Point_3, color= Weekday_weekend)) + ylim(0,1800)+ geom_line(size=1) + theme_bw()+labs(x = "Time of day", y= "kW")+ 
  ggtitle("Autumn - Weekly load profile")+theme(axis.text=element_text(size=16))






#Winter daily data_Point_4_WEEKDAY&WEEKEND
P4_Daily_data_Winter_WE <- Winter %>%
  
  group_by(Weekday_weekend, Hour) %>%
  
  summarise(Point_4=mean(Point_4))

P4_Daily_data_Winter_WE$Hour <- as.integer(P4_Daily_data_Winter_WE$Hour)
#Daily_data_Winter$DayOfWeek <- factor(Daily_data_Winter$DayOfWeek, levels = Daylabs)

#boxplot(Point_3~DayOfWeek, data = Daily_data_Winter, ylab = "kW", outline=F, main='Winter', ylim=c(500,1600))



#Summer daily data
P4_Daily_data_Summer_WE <- Summer %>%
  
  group_by(Weekday_weekend, Hour) %>%
  
  summarise(Point_4=mean(Point_4))

P4_Daily_data_Summer_WE$Hour <- as.integer(P4_Daily_data_Summer_WE$Hour)

#Daily_data_Summer$DayOfWeek <- factor(Daily_data_Summer$DayOfWeek, levels = Daylabs)

#boxplot(Point_3~DayOfWeek, data = Daily_data_Summer, ylab = "kW", outline=F, main='Summer',ylim=c(500,1600))

#Spring daily data
P4_Daily_data_Spring_WE <- Spring %>%
  
  group_by(Weekday_weekend, Hour) %>%
  
  summarise(Point_4=mean(Point_4))

P4_Daily_data_Spring_WE$Hour <- as.integer(P4_Daily_data_Spring_WE$Hour)

#Daily_data_Spring$DayOfWeek <- factor(Daily_data_Spring$DayOfWeek, levels = Daylabs)

#boxplot(Point_3~DayOfWeek, data = Daily_data_Spring, ylab = "kW", outline=F, main='Spring',ylim=c(500,1600))


#Autumn daily data
P4_Daily_data_Autumn_WE <- Autumn %>%
  
  group_by(Weekday_weekend, Hour) %>%
  
  summarise(Point_4=mean(Point_4))

P4_Daily_data_Autumn_WE$Hour <- as.integer(P4_Daily_data_Autumn_WE$Hour)

#Daily_data_Autumn$DayOfWeek <- factor(Daily_data_Autumn$DayOfWeek, levels = Daylabs)

#boxplot(Point_3~DayOfWeek, data = Daily_data_Autumn, ylab = "kW", outline=F, main='Autumn',ylim=c(500,1600))

write.csv(P4_Daily_data_Autumn_WE, 'Point4autumn.csv')
write.csv(P4_Daily_data_Winter_WE, 'Point4winter.csv')
write.csv(P4_Daily_data_Spring_WE, 'Point4spring.csv')
write.csv(P4_Daily_data_Summer_WE, 'Point4summer.csv')



library(ggplot2)

#par(mfrow = c(2,2))

ggplot(P4_Daily_data_Winter_WE, aes(x= Hour, y=Point_4, color= Weekday_weekend)) + ylim(0,1000)+ geom_line(size=1) + theme_bw()+labs(x = "Time of day", y= "kW")+ 
  ggtitle("Winter - Weekly load profile")+theme(legend.position = "none")+theme(axis.text=element_text(size=14))


ggplot(P4_Daily_data_Summer_WE, aes(x= Hour, y=Point_4, color= Weekday_weekend)) + ylim(0,1000)+ geom_line(size=1) + theme_bw()+labs(x = "Time of day", y= "kW")+ 
  ggtitle("Summer - Weekly load profile")+theme(legend.position = "none")+theme(axis.text=element_text(size=14))

ggplot(P4_Daily_data_Spring_WE, aes(x= Hour, y=Point_4, color= Weekday_weekend)) + ylim(0,1000)+ geom_line(size=1) + theme_bw()+labs(x = "Time of day", y= "kW")+ 
  ggtitle("Spring - Weekly load profile")+theme(legend.position = "none")+theme(axis.text=element_text(size=14))


ggplot(P4_Daily_data_Autumn_WE, aes(x= Hour, y=Point_4, color= Weekday_weekend)) + ylim(0,1000)+ geom_line(size=1) + theme_bw()+labs(x = "Time of day", y= "kW")+ 
  ggtitle("Autumn - Weekly load profile")+theme(axis.text=element_text(size=16))










#Winter daily data_Point_5_WEEKDAY&WEEKEND
P5_Daily_data_Winter_WE <- Winter %>%
  
  group_by(Weekday_weekend, Hour) %>%
  
  summarise(Point_5=mean(Point_5))

P5_Daily_data_Winter_WE$Hour <- as.integer(P5_Daily_data_Winter_WE$Hour)
#Daily_data_Winter$DayOfWeek <- factor(Daily_data_Winter$DayOfWeek, levels = Daylabs)

#boxplot(Point_3~DayOfWeek, data = Daily_data_Winter, ylab = "kW", outline=F, main='Winter', ylim=c(500,1600))



#Summer daily data
P5_Daily_data_Summer_WE <- Summer %>%
  
  group_by(Weekday_weekend, Hour) %>%
  
  summarise(Point_5=mean(Point_5))

P5_Daily_data_Summer_WE$Hour <- as.integer(P5_Daily_data_Summer_WE$Hour)

#Daily_data_Summer$DayOfWeek <- factor(Daily_data_Summer$DayOfWeek, levels = Daylabs)

#boxplot(Point_3~DayOfWeek, data = Daily_data_Summer, ylab = "kW", outline=F, main='Summer',ylim=c(500,1600))

#Spring daily data
P5_Daily_data_Spring_WE <- Spring %>%
  
  group_by(Weekday_weekend, Hour) %>%
  
  summarise(Point_5=mean(Point_5))

P5_Daily_data_Spring_WE$Hour <- as.integer(P5_Daily_data_Spring_WE$Hour)

#Daily_data_Spring$DayOfWeek <- factor(Daily_data_Spring$DayOfWeek, levels = Daylabs)

#boxplot(Point_3~DayOfWeek, data = Daily_data_Spring, ylab = "kW", outline=F, main='Spring',ylim=c(500,1600))


#Autumn daily data
P5_Daily_data_Autumn_WE <- Autumn %>%
  
  group_by(Weekday_weekend, Hour) %>%
  
  summarise(Point_5=mean(Point_5))

P5_Daily_data_Autumn_WE$Hour <- as.integer(P5_Daily_data_Autumn_WE$Hour)

#Daily_data_Autumn$DayOfWeek <- factor(Daily_data_Autumn$DayOfWeek, levels = Daylabs)

#boxplot(Point_3~DayOfWeek, data = Daily_data_Autumn, ylab = "kW", outline=F, main='Autumn',ylim=c(500,1600))

write.csv(P5_Daily_data_Autumn_WE, 'Point5autumn.csv')
write.csv(P5_Daily_data_Winter_WE, 'Point5winter.csv')
write.csv(P5_Daily_data_Spring_WE, 'Point5spring.csv')
write.csv(P5_Daily_data_Summer_WE, 'Point5summer.csv')



library(ggplot2)

#par(mfrow = c(2,2))

ggplot(P5_Daily_data_Winter_WE, aes(x= Hour, y=Point_5, color= Weekday_weekend)) + ylim(0,1000)+ geom_line(size=1) + theme_bw()+labs(x = "Time of day", y= "kW")+ 
  ggtitle("Winter - Weekly load profile")+theme(legend.position = "none")+theme(axis.text=element_text(size=14))


ggplot(P5_Daily_data_Summer_WE, aes(x= Hour, y=Point_5, color= Weekday_weekend)) + ylim(0,1000)+ geom_line(size=1) + theme_bw()+labs(x = "Time of day", y= "kW")+ 
  ggtitle("Summer - Weekly load profile")+theme(legend.position = "none")+theme(axis.text=element_text(size=14))

ggplot(P5_Daily_data_Spring_WE, aes(x= Hour, y=Point_5, color= Weekday_weekend)) + ylim(0,1000)+ geom_line(size=1) + theme_bw()+labs(x = "Time of day", y= "kW")+ 
  ggtitle("Spring - Weekly load profile")+theme(legend.position = "none")+theme(axis.text=element_text(size=14))


ggplot(P5_Daily_data_Autumn_WE, aes(x= Hour, y=Point_5, color= Weekday_weekend)) + ylim(0,1000)+ geom_line(size=1) + theme_bw()+labs(x = "Time of day", y= "kW")+ 
  ggtitle("Autumn - Weekly load profile")+theme(axis.text=element_text(size=16))


