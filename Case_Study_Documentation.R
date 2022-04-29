library("readxl")
library("ggplot2")
library("dygraphs")
setwd("D:/Case Study")
paths2013 <- vector()
paths2014 <- vector()
paths2015 <- vector()
paths2016 <- vector()
paths2017 <- vector()
paths2018 <- vector()
paths2019 <- vector()
paths2013 <- c(paths2013, paste(getwd(),"/Yearly_Data/2013/Quarterly/Divvy_Trips_2013.xlsx", sep = "")) 
paths2014 <- c(paths2014, paste(getwd(),"/Yearly_Data/2014/Quarterly/Divvy_Trips_2014_Q1Q2.xlsx", sep = ""))
paths2014 <- c(paths2014, paste(getwd(),"/Yearly_Data/2014/Quarterly/Divvy_Trips_2014-Q3-07.xlsx", sep = ""))
paths2014 <- c(paths2014, paste(getwd(),"/Yearly_Data/2014/Quarterly/Divvy_Trips_2014-Q3-0809.xlsx", sep = ""))
paths2014 <- c(paths2014, paste(getwd(),"/Yearly_Data/2014/Quarterly/Divvy_Trips_2014-Q4.xlsx", sep = ""))
paths2015 <- c(paths2015, paste(getwd(),"/Yearly_Data/2015/Quarterly/Divvy_Trips_2015_07.xlsx", sep = ""))
paths2015 <- c(paths2015, paste(getwd(),"/Yearly_Data/2015/Quarterly/Divvy_Trips_2015_07.xlsx", sep = ""))
paths2015 <- c(paths2015, paste(getwd(),"/Yearly_Data/2015/Quarterly/Divvy_Trips_2015_07.xlsx", sep = ""))
paths2015 <- c(paths2015, paste(getwd(),"/Yearly_Data/2015/Quarterly/Divvy_Trips_2015_07.xlsx", sep = ""))
paths2015 <- c(paths2015, paste(getwd(),"/Yearly_Data/2015/Quarterly/Divvy_Trips_2015_07.xlsx", sep = ""))
paths2015 <- c(paths2015, paste(getwd(),"/Yearly_Data/2015/Quarterly/Divvy_Trips_2015_07.xlsx", sep = ""))
paths2016 <- c(paths2016, paste(getwd(),"/Yearly_Data/2016/Quarterly/Divvy_Trips_2016_04.xlsx", sep = ""))
paths2016 <- c(paths2016, paste(getwd(),"/Yearly_Data/2016/Quarterly/Divvy_Trips_2016_05.xlsx", sep = ""))
paths2016 <- c(paths2016, paste(getwd(),"/Yearly_Data/2016/Quarterly/Divvy_Trips_2016_06.xlsx", sep = ""))
paths2016 <- c(paths2016, paste(getwd(),"/Yearly_Data/2016/Quarterly/Divvy_Trips_2016_Q1.xlsx", sep = ""))
paths2016 <- c(paths2016, paste(getwd(),"/Yearly_Data/2016/Quarterly/Divvy_Trips_2016_Q3.xlsx", sep = ""))
paths2016 <- c(paths2016, paste(getwd(),"/Yearly_Data/2016/Quarterly/Divvy_Trips_2016_Q4.xlsx", sep = ""))
paths2017 <- c(paths2017, paste(getwd(),"/Yearly_Data/2017/Quarterly/Divvy_Trips_2017_Q1.xlsx", sep = ""))
paths2017 <- c(paths2017, paste(getwd(),"/Yearly_Data/2017/Quarterly/Divvy_Trips_2017_Q2.xlsx", sep = ""))
paths2017 <- c(paths2017, paste(getwd(),"/Yearly_Data/2017/Quarterly/Divvy_Trips_2017_Q3.xlsx", sep = ""))
paths2017 <- c(paths2017, paste(getwd(),"/Yearly_Data/2017/Quarterly/Divvy_Trips_2017_Q4.xlsx", sep = ""))
paths2018 <- c(paths2018, paste(getwd(),"/Yearly_Data/2018/Quarterly/Divvy_Trips_2018_Q1.xlsx", sep = ""))
paths2018 <- c(paths2018, paste(getwd(),"/Yearly_Data/2018/Quarterly/Divvy_Trips_2018_Q2.xlsx", sep = ""))
paths2018 <- c(paths2018, paste(getwd(),"/Yearly_Data/2018/Quarterly/Divvy_Trips_2018_Q3.xlsx", sep = ""))
paths2018 <- c(paths2018, paste(getwd(),"/Yearly_Data/2018/Quarterly/Divvy_Trips_2018_Q4.xlsx", sep = ""))
paths2019 <- c(paths2019, paste(getwd(),"/Yearly_Data/2019/Quarterly/Divvy_Trips_2019_Q1.xlsx", sep = ""))
paths2019 <- c(paths2019, paste(getwd(),"/Yearly_Data/2019/Quarterly/Divvy_Trips_2019_Q2.xlsx", sep = ""))
paths2019 <- c(paths2019, paste(getwd(),"/Yearly_Data/2019/Quarterly/Divvy_Trips_2019_Q3.xlsx", sep = ""))
paths2019 <- c(paths2019, paste(getwd(),"/Yearly_Data/2019/Quarterly/Divvy_Trips_2019_Q4.xlsx", sep = ""))

AllPaths <- c(paths2019)
Trips <- list()
for(i in AllPaths)
{
  for(Mypath in i)
  { 
    Trips <- rbind(Trips,read_excel(Mypath, col_types = c("guess","guess","guess","guess","guess","guess","guess","guess","guess","guess","guess","numeric")))
  }
}

Trips$beginTime <- format(as.POSIXct(
  Trips$start_time),format = "%H:%M:%S")
Trips$startDate <- as.Date(Trips$start_time)
Trips$Endtime <- format(as.POSIXct(
  Trips$end_time),format = "%H:%M:%S")
Trips$EndDate <- as.Date(Trips$end_time)

UserTypeCol <- Trips$usertype
Subs <- 0
Customers <- 0

for(i in UserTypeCol)
  if(i == "Subscriber")
    Subs <- Subs + 1 else
      Customers <- Customers + 1

Trips[!is.na(Trips$gender),]

View(Trips)
Gender_Demographic <- ggplot(data = Trips) +
                        geom_bar(mapping = aes(x = gender, fill = usertype), stat = "count")

ggplot(data = Trips, aes(trip_id, tripduration)) +
        geom_smooth()

pie(c(Subs,Customers),label = c(paste("Subscribers = ", round(Subs*100/(Subs + Customers), 2), "%"), paste("Customers = ", round(Customers*100/(Subs + Customers), 2), "%")))


