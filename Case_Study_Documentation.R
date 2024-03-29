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
    Trips <- rbind(Trips,read_excel(Mypath, col_types = c("guess","date","date","guess","guess","guess","guess","guess","guess","guess","guess","numeric")))
  }
}

UserTypeCol <- Trips$usertype
Subs <- 0
Customers <- 0

for(i in UserTypeCol)
  if(i == "Subscriber")
    Subs <- Subs + 1 else
      Customers <- Customers + 1

Trips[!is.na(Trips$gender),]

View(Trips)

ggplot(data = Trips, aes(gender)) +
  geom_area(mapping = aes(fill = ..count..), stat = "count")

pie(c(Subs,Customers),label = c(paste("Subscribers = ", round(Subs*100/(Subs + Customers), 2), "%"), paste("Customers = ", round(Customers*100/(Subs + Customers), 2), "%")))

Trip_Duration <- ggplot(data = Trips, aes(trip_id, tripduration)) +
  geom_line()

Trips$date <- as.Date(Trips$start_time)
Trips$month <- format(as.Date(Trips$start_time), "%m")
Trips$day <- format(as.Date(Trips$start_time), "%A")

ggplot(data = Trips) + 
  geom_bar(mapping = aes(x = day, fill = day), stat = "count") +
    coord_flip() +    
      facet_wrap(~usertype)

xy2 <- as.data.frame(table(Trips$month))
xy2$Var1 <- factor(xy2$Var1, levels = xy2$Var1[order(xy2$Freq)])

ggplot(xy2,aes(x=Var1, y = Freq, fill = Freq)) +
    scale_fill_gradient(low = "yellow", high = "red") +
      geom_bar(width=0.7, stat = "identity") +
        coord_flip()

Sub_Percentage <- c(59.14, 77.41, 80, 81.69, 77.88)
Years <- c(2015, 2016, 2017, 2018, 2019)

ggplot(mapping = aes(x=Years, y=Sub_Percentage)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
    geom_line(color="#69b3a2", size=2) +
      geom_point(size=3, color="#69b3a2")

ggplot(data = Trips) +
  geom_bar(mapping = aes(x = month, fill = month), stat = "count")
  

ggplot(data = Trips) +
  geom_bar(mapping = aes(x = trip))

