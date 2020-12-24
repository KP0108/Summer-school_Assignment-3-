# create three temporal features: Hour, day, DayOfWeek, Weekday_Weekend #

library(dplyr)

minami182$Time <- strptime(minami182$Time, format = "%m/%d/%Y %H:%M")

                   

weekday1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')



minami182_new <- minami182 %>%
  
  mutate(Hour= format(minami182$Time,"%H"),
         
         Day = as.numeric(format(minami182$Time,"%d")),
         
         DayOfWeek = weekdays(minami182$Time,abbreviate = FALSE),
         
         Weekday_weekend=factor((DayOfWeek %in% weekday1),
                                
                                levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')))

write.csv(minami182_new, "minami182_new.csv")


# create temporal features: Month #
library(dplyr)

Minami182_10min_Processed$Time <- strptime(Minami182_10min_Processed$Time, format = "%m/%d/%Y %H:%M")

minami182_month <- Minami182_10min_Processed %>%
  mutate(Month = format(Minami182_10min_Processed$Time, "%m"))

write.csv(minami182_month, "minami182_month.csv")

Func.Day <- function(time){
  night <- c("0","1","2","3","4","5","22","23")
  morning <- c("6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21")
  day_division <- c("Off.Peak","Peak")
  x <- vector()
  for(i in 1:length(time)){
    if(time[i] %in% night){
      x[i]= day_division[1]
    }else{ x[i]= day_division[2]}
  }
  return(x)
}


Minami182_10min_Processed_2_$Day.period <- Func.Day(Minami182_10min_Processed_2_$Hour)

write.csv(Minami182_10min_Processed_2_, "Minami182_10min_Processed_2_.csv")
