#Q1a.
library(ggplot2)
dumemp <- read.delim('dublin employment trends.txt',sep=':',header=T)
View(dumemp)
dumemp <- dumemp[,c(1:3)]
qplot(data=dumemp,x=Time,y=Employment,geom='line',main = 'Dublin Employment Trends Per Sector: 2006-2016',xlab = 'Quarterly figures',ylab = 'Trend') + facet_grid(.~Sector)

#Q1b.
dubprp <- read.delim('dublin property trends.txt',header = T,sep = '\t')
View(dubprp)
dubprp <- dubprp[,c(1:3)]
qplot(data=dubprp,x=Time,y=Trend,color=Category,geom = 'line',main = 'Dublin Property Trends: 2007-2016')

#Q2a.
install.packages("devtools")
library(devtools)
install_github("brian-bot/velib")
library(velib)
setApiKey("565e837c1dcf4059e76f83013a21df36235d9c26")

cs <- velibRestGET('contracts')
idx <- sapply(cs, function(x){x$commercial_name=="CityCycle"})
velibContract <- cs[[26]]

stns <- velibRestGET(paste0("stations?contract=", velibContract$name))
stnsNew <- lapply(stns,function(x){
  x$last_update <- as.POSIXct(x$last_update/1000,tz="GMT", origin="1970-01-01")
  x$position <- paste0(x$position, collapse=":")
  return(as.data.frame(x))
})
View(stnsNew)

df <- do.call(rbind,stnsNew)
str(df)

for (i in 3:5){
  df[,i] <- as.character(df[,i])
}

df$contract_name <- factor(df$contract_name)

View(df)
str(df)

head(df)
tail(df)
ncol(df)
nrow(df)
dim(df)
nrow(na.omit(df))/nrow(df)*100

summary(df)

#Q2b.

library(ggplot2)

qplot(data=df, bike_stands,ylab = "Count",main = "Actual number of bike stands") + theme(plot.title = element_text(size = 20, face = "bold"))+theme(axis.title.y = element_text(colour="grey20",size=15,face="bold"),axis.title.x = element_text(colour="grey20",size=15,face="bold"))

qplot(data=df,available_bike_stands,ylab = "Count",main="Available number of bike stands" ) + theme(plot.title = element_text(size = 20, face = "bold"))+theme(axis.title.y = element_text(colour="grey20",size=15,face="bold"),axis.title.x = element_text(colour="grey20",size=15,face="bold"))

qplot(data=df,available_bikes,ylab = "Count",main="Available number of bikes") + theme(plot.title = element_text(size = 20, face = "bold"))+theme(axis.title.y = element_text(colour="grey20",size=15,face="bold"),axis.title.x = element_text(colour="grey20",size=15,face="bold"))

viz2b <- qplot(data=df,x=df$bike_stands,y=df$available_bikes,size=df$available_bike_stands,color=I('blue'),geom = 'point',main='Analysis of availability of bikes and bike stands') + xlab('Actual number of bike stands') +ylab('Available number of bikes')
viz2b <- viz2b + scale_color_discrete("Available Bike Stand") +theme(plot.title = element_text(size = 20, face = "bold"))+theme(axis.title.y = element_text(colour="grey20",size=15,face="bold"),axis.title.x = element_text(colour="grey20",size=15,face="bold"))
viz2b

#Q3a.
unzip("DublinBus.zip", exdir = './DublinBus')
list.files()
agency <- read_csv('agency.txt')
routes <- read_csv('routes.txt')
calendar <- read_csv('calendar.txt')
calendar_dates <- read_csv('calendar_dates.txt')
routes <- read_csv('routes.txt')
shapes <- read_csv('shapes.txt')
stop_times <- read_csv('stop_times.txt')
stops <- read_csv('stops.txt')
transfers <- read_csv('transfers.txt')
trips <- read_csv('trips.txt')

dataset <- inner_join(agency,routes,by='agency_id') %>% inner_join(trips, by = 'route_id') %>% inner_join(stop_times,by='trip_id') %>% inner_join(stops,by = 'stop_id') %>% inner_join(calendar_dates,by = 'service_id') %>% inner_join(calendar,by='service_id')
View(dataset)

#Total number of Busroutes available in Dublin

Total_routes <-length(unique(dataset$route_short_name))
Total_routes

#Number of routes for outbound travel and inbound travel within Dublin

Outbound_routes <- length(dataset$route_short_name[dataset$direction_id == '0'])
Outbound_routes
Inbound_routes <- length(dataset$route_short_name[dataset$direction_id == '1'])
Inbound_routes

#Dublin Bus route having maximum number of stops

Max_stops_routes <- dataset$route_short_name[dataset$stop_sequence == max(dataset$stop_sequence)]

#Average distance that Dublin bus routes cover

Avg_distance <- mean(dataset$shape_dist_traveled)
Avg_distance

#Stop_headsigns while travelling in outbound and inbound directions

stophd_out <-  dataset$stop_headsign[dataset$direction_id == '0']
stophd_out <- unique(stophd_out)

stophd_In <- dataset$stop_headsign[dataset$direction_id == '1']
stophd_In <- unique(stophd_In)

#Q3b.

join <- inner_join(routes,trips,by='route_id')  %>% inner_join(stop_times,by = 'trip_id')  %>% inner_join(stops,by='stop_id')

Route_fwd <- join %>% filter(route_short_name == '84X') %>% filter(direction_id == '1') %>% filter(stop_name == 'Dawson Street')
View(Route_fwd)

Route_rev <- join %>% filter(route_short_name == '84X') %>% filter(direction_id == '0') %>% filter(stop_name == 'Stillorgan Road')
View(Route_rev)

Route_84X <- rbind(Route_fwd,Route_rev)
View(Route_84X)

viz3b <- qplot(data = Route_84X,x=stop_name,y=format(Route_84X$arrival_time, format='%H:M'),color=stop_headsign,main='84X Route details',ylab = 'Scheduled arrival time',xlab = 'Heading direction',color = c('red','green')) + geom_point(aes(size=I(0.005))) + theme(axis.title.y = element_text(colour="grey20",size=15,face="bold"),axis.title.x = element_text(colour="grey20",size=15,face="bold"))
viz3b <- viz3b +theme(plot.title = element_text(size = 20, face = "bold"))+theme(axis.title.y = element_text(colour="grey20",size=15,face="bold"),axis.title.x = element_text(colour="grey20",size=15,face="bold")) + ggsave("123.pdf",height=6)
viz3b 

#Q4.
install.packages("readODS")
library("readODS")

data_list_north <- list()
data_list_south <- list()

for(i in 1:52){
  
  data_list_temp <- read.ods('pedestrianfootfall2013 (3).ods',sheet = i)
  camera_indices <- which(data_list_temp$A == "Time")[1:2]
  data_list_north[[i]] <- data_list_temp[camera_indices[1]: (camera_indices[1]+24),]
  
  data_list_north[[i]] <- data_list_north[[i]][-1,]  
  
  data_list_north[[i]] <- as.matrix(data_list_north[[i]][,-1])
  
  mode(data_list_north[[i]]) <- "numeric"
  
  data_list_north[[i]] <- colMeans(data_list_north[[i]])
  
  names(data_list_north[[i]]) <- c('Mon_In','Mon_Out','Tue_In','Tue_Out','Wed_In','Wed_Out','Thu_In','Thu_Out',
                                    'Fri_In','Fri_Out','Sat_In','Sat_Out','Sun_In','Sun_Out')
  
  
  data_list_south[[i]] <- data_list_temp[camera_indices[2]: (camera_indices[2]+24),]
  
  data_list_south[[i]] <- data_list_south[[i]][-1,]
  
  data_list_south[[i]] <- as.matrix(data_list_south[[i]][,-1])
  
  mode(data_list_south[[i]]) <- "numeric"
  
  data_list_south[[i]] <- colMeans(data_list_south[[i]])
  
  names(data_list_south[[i]]) <- c('Mon_In','Mon_Out','Tue_In','Tue_Out','Wed_In','Wed_Out','Thu_In','Thu_Out',
                                    'Fri_In','Fri_Out','Sat_In','Sat_Out','Sun_In','Sun_Out')
  
}

temp_daily_north <- unlist(data_list_north)
temp_daily_north <- temp_daily_north[!is.na(temp_daily_north)]
temp_daily_north
Mon_in_north <- temp_daily_north[seq(1,length(temp_daily_north),by = 14)]
Mon_in_north

Mon_in_north_mean <- mean(Mon_in_north)

temp_daily_south <- unlist(data_list_south)
temp_daily_south <- temp_daily_south[!is.na(temp_daily_south)]
temp_daily_south
Mon_in_south <- temp_daily_south[seq(1,length(temp_daily_south),by = 14)]
Mon_in_south

Mon_in_south_mean <- mean(Mon_in_south)

Prop <- (Mon_in_north_mean/Mon_in_south_mean)

#Q4b.

#scraped 2012 O'Conell street data
library(readODS)
data2012 <- read.ods('pedestrianfootfall2012.ods',sheet = 8)
View(data2012)
OCS_2012 <- data2012[c(7:30),]
names(OCS_2012) <- c('Time','Mon_In_week8','Mon_Out_week8','Tue_In_week8','Tue_Out_week8','Wed_In_week8','Wed_Out_week8','Thu_In_week8','Thu_Out_week8',
                                 'Fri_In_week8','Fri_Out_week8','Sat_In_week8','Sat_Out_week8','Sun_In_week8','Sun_Out_week8')

rownames(OCS_2012) <- OCS_2012$Time
OCS_2012 <- OCS_2012[,-1]
View(OCS_2012)
for( i in 1:14){
  OCS_2012[,i] <- as.numeric(OCS_2012[,i])
}
  
OCS2012_sum <- colSums(OCS_2012)
SumInOCS2012 <- OCS2012_sum[grep(".*_In",names(OCS2012_sum))]
SumOutOCS2012 <- OCS2012_sum[grep(".*_Out",names(OCS2012_sum))]
InOutratio_OCS2012 <- (SumInOCS2012/SumOutOCS2012)
InOutratio_OCS2012

#from 2013 data O'Conell street previous data
data2013 <- read.ods('pedestrianfootfall2013 (3).ods',sheet = 8)
View(data2013)
OCS_2013 <- data2013[c(7:30),]
names(OCS_2013) <- c('Time','Mon_In_week8','Mon_Out_week8','Tue_In_week8','Tue_Out_week8','Wed_In_week8','Wed_Out_week8','Thu_In_week8','Thu_Out_week8',
                     'Fri_In_week8','Fri_Out_week8','Sat_In_week8','Sat_Out_week8','Sun_In_week8','Sun_Out_week8')

rownames(OCS_2013) <- OCS_2013$Time
OCS_2013 <- OCS_2013[,-1]
View(OCS_2013)
for( i in 1:14){
  OCS_2013[,i] <- as.numeric(OCS_2013[,i])
}

OCS2013_sum <- colSums(OCS_2013)
SumInOCS2013 <- OCS2013_sum[grep(".*_In",names(OCS2013_sum))]
SumOutOCS2013 <- OCS2013_sum[grep(".*_Out",names(OCS2013_sum))]
InOutratio_OCS_2013 <- (SumInOCS2013/SumOutOCS2013)
InOutratio_OCS_2013

Time <- rownames(OCS_2012)

OCSfootfall <- data.frame(InOutratio_OCS2012,InOutratio_OCS_2013)

weekdays = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
rownames(OCSfootfall) <- weekdays

viz4b <- qplot(data = OCSfootfall,x=InOutratio_OCS2012,y=InOutratio_OCS_2013,color=rownames(OCSfootfall),main = "O'Conell street Week8 InOut footfall ratio 2012 v/s 2013") 
viz4b <- viz4b + geom_point(size=4)+theme(plot.title = element_text(size = 20, face = "bold"))+theme(axis.title.y = element_text(colour="grey20",size=15,face="bold"),axis.title.x = element_text(colour="grey20",size=15,face="bold"))
viz4b

#Q5.
library(lubridate)
hist <- read_csv('history_database')
View(hist)
colnames(hist) <- "TC"

library(tidyr)
hist <- separate(data=hist,col = TC,into = c("Time","Commands"),sep = "\\:")
View(hist)

hist$Time <- as.numeric(hist$Time)
hist$Time <- as_datetime(hist$Time/1000)

Proj_hist <- hist[c(6893:9162),]
View(Proj_hist)

Date_Proj_hist <- data.frame(as.Date(Proj_hist$Time),Proj_hist$Commands)
colnames(Date_Proj_hist) <- c("Date","Commands")
View(Date_Proj_hist)

#Dates on which I worked on R assignment
Plot <- data.frame(Date_Proj_hist %>% group_by(Date) %>% summarise(Length_date = length(Date)))

viz <- ggplot(data=Plot,aes(x=Date,y=Length_date, fill = Date)) + ggtitle('Dates on which I worked on R assignment') + xlab('Date_2018') +ylab('Number of commands executed')+theme(axis.title.y = element_text(colour="grey20",size=15,face="bold"),axis.title.x = element_text(colour="grey20",size=15,face="bold"))
viz <- viz + geom_col(position='dodge') +theme(axis.title.y = element_text(colour="grey20",size=15,face="bold"),axis.title.x = element_text(colour="grey20",size=15,face="bold"))
viz <- viz + theme(plot.title = element_text(size = 20, face = "bold"))
viz

#Datewise working analysis of assignment questions
First_Q <- data.frame(Date_Proj_hist[grep("dublin employment trends",Date_Proj_hist$Commands),])
First_Q <- unique(First_Q$Date)
First_Q
cnt_First_Q <- length(First_Q)
cnt_First_Q

Second_Q <- data.frame(Date_Proj_hist[grep("bike|velib",Date_Proj_hist$Commands),])
Second_Q <- unique(Second_Q$Date)
Second_Q
cnt_Second_Q <- length(Second_Q)
cnt_Second_Q

Third_Q <- data.frame(Date_Proj_hist[grep("routes",Date_Proj_hist$Commands),])
Third_Q <- unique(Third_Q$Date)
Third_Q
cnt_Third_Q <- length(Third_Q)
cnt_Third_Q

Fourth_Q <- data.frame(Date_Proj_hist[grep("footfall",Date_Proj_hist$Commands),])
Fourth_Q <- unique(Fourth_Q$Date)
Fourth_Q
cnt_Fourth_Q <- length(Fourth_Q)
cnt_Fourth_Q

Fifth_Q <- data.frame(Date_Proj_hist[grep("history",Date_Proj_hist$Commands),])
Fifth_Q <- unique(Fifth_Q$Date)
Fifth_Q
cnt_Fifth_Q <- length(Fifth_Q)
cnt_Fifth_Q

#query which took more time to complete
Days_count <- c(cnt_First_Q,cnt_Second_Q,cnt_Third_Q,cnt_Fourth_Q,cnt_Fifth_Q)
names(Days_count) <- c("First Question","Second Question","Third Question","Fourth Question","Fifth Question")
Days_count <- data.frame(Days_count)
colnames(Days_count) <- "Days"

viz5b <- ggplot(data=Days_count,aes(x=rownames(Days_count),y=Days)) + geom_point(size = 5) +ggtitle('Days taken to complete v/s Question') + xlab('Question_ID')
viz5b <- viz5b + theme(axis.title.y = element_text(colour="grey20",size=15,face="bold"),axis.title.x = element_text(colour="grey20",size=15,face="bold"))
viz5b <- viz5b + theme(plot.title = element_text(size = 20, face = "bold"))
viz5b      
