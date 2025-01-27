# import raw data and sampling

library(data.table)
set.seed(100615)
data1 <- fread("input/aggregate/agg_match_stats_1.csv")
data1.backup <- data1
data1 <- data1[sample(1:dim(data1)[1],size = 100000,replace = F),]
data2 <- fread("input/deaths/kill_match_stats_final_1.csv")
data2.backup <- data2
data2 <- data2[sample(1:dim(data2)[1],size = 100000,replace = F),]

# data1 : meta information about the match (rank/damage/...), each observation matches a player
# data2 : death information about fighting (gun/death/location/...), each observation matched a player
# the raw data is too large (about 10^7 obs), take a random sample (10^5 obs)


# preprocessing

data1 <- na.omit(data1)
data2 <- na.omit(data2)
data2 <- data2[-which(data2$map==''),]
data1 <- data1[,-4]

match_id <- data.frame(table(data1$match_id))
id1 <- data.frame(table(data1$player_name))
match_id2 <- data.frame(table(data2$match_id))
id_victim <- data.frame(table(data2$victim_name))
length(match_id$Var1)
length(id1$Var1)
length(match_id2$Var1)
length(id_victim$Var1)

# time sequency

data1$day <- substr(data1$date,1,10)
data1$time <- substr(data1$date,12,19)
data1 <- data1[,-1]

miramar <- data2[map == 'MIRAMAR']
erangel <- data2[map == 'ERANGEL']

# data2 contains 74151 ERANGEL death records and 15949 MIRAMAR death records



lastkill <- data2[killer_placement==1 & victim_placement==2]



data1$day <- as.Date(data1$day)
day <- data.frame(table(data1$day))

library(ggplot2)
library(scales)
day$day <- as.Date(day$Var1)
p1 <- ggplot(data = day, aes(x = day,y = Freq,group = 1))
p1 + geom_line(linetype="dotted") + labs(title = '', x = 'Date',y = 'Game Heat') + scale_x_date(breaks = date_breaks("15 days")) + geom_point()


# Periodic decomposition
day_ts <- ts(day$Freq)
library(TTR)
smo1 <- SMA(day_ts, n=7)
som2 <- data.frame(smo1)
som2 <- cbind(som2,day$Var1)
colnames(som2) <- c('freq','date')
som2$date <- as.Date(som2$date)
p1 <- ggplot(data = som2, aes(x = date, y = freq))
p1 + geom_line(linetype="dotted") + labs(title = '', x = 'Date',y = 'Game Heat') + scale_x_date(breaks = date_breaks("15 days")) + geom_point()


zhongshu <- function(x)
{
  return(as.numeric(names(table(x))[table(x) == max(table(x))]))
}
range(data2$time)
zhongshu(data2$time)
ggplot(data = data2, aes(x=time)) + geom_density(colour='grey',fill='#87CEFA') + labs(x = 'Death time[s]', y = 'Probability')


# try 2 maps
range(erangel$time)
zhongshu(erangel$time)
range(miramar$time)
zhongshu(miramar$time)

ggplot(data = erangel, aes(x=time)) +
  geom_density(colour='grey',fill='#66CDAA') + labs(title='ERANGEL',x = 'Death time[s]', y = 'Probability') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = miramar, aes(x=time)) + geom_density(colour='grey',fill='#F0E68C') +
  labs(title='MIRAMAR',x = 'Death time[s]', y = 'Probability') +
  theme(plot.title = element_text(hjust = 0.5))

# 120s 128s max


# choose 60s since first death
# erangel 61-121s
# miramar 62-122s
era_landing <- erangel[which(erangel$time <= 121),]
mir_landing <- miramar[which(miramar$time <= 122),]
era_landing[,c(4:5,11:12)] <- era_landing[,c(4:5,11:12)]*4096/800000
mir_landing[,c(4:5,11:12)] <- mir_landing[,c(4:5,11:12)]*1000/800000

library(jpeg)
library(grid)
erangel.map <- readJPEG("erangel.jpg")
miramar.map <- readJPEG("miramar.jpg")

dim(era_landing)[1]
dim(mir_landing)[1]

ggplot(aes(x = killer_position_x,y=killer_position_y),data = era_landing) ++
  annotation_custom(rasterGrob(erangel.map, width=unit(1,"npc"), height=unit(1,"npc"))) +
  ylim(4096,0) + xlim(0,4096) +
  guides(alpha=F) +
  geom_point(colour = "lightgreen",size = 0.1) +
  ?(aes(fill = ..level..,alpha = ..level..),geom = "polygon",bins = 50) +
  scale_fill_continuous(high = "red",low = "green") +
  labs(title = "Parachute Landing hotspot map of Erangel") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(aes(x = killer_position_x,y=killer_position_y),data = mir_landing) +
  annotation_custom(rasterGrob(miramar.map, width=unit(1,"npc"), height=unit(1,"npc"))) +
  ylim(1000,0) + xlim(0,1000) +
  guides(alpha=F) +
  geom_point(colour = "lightgreen",size = 0.75) +
  stat_density_2d(aes(fill = ..level..,alpha = ..level..),geom = "polygon",bins = 100) +
  scale_fill_continuous(high = "red",low = "green") +
  labs(title = "Parachute Landing hotspot map of Miramar") +
  theme(plot.title = element_text(hjust = 0.5))

weapon <- data.frame(table(data2$killed_by))
guns <- c('P18C','P1911','P92','R1895','R45','Win94','S1897','S686','S12K','UMP9','Micro UZI','Vector','Tommy Gun','AKM','M416','SCAR-L','M16A4','AUG','Groza','M249','DP-28','VSS','Mini 14','SKS','Mk14','Kar98k','M24','AWM')

gun <- weapon[weapon$Var1 %in% guns,]
gun$Var1 <- factor(gun$Var1)
gun$percent <- gun$Freq/sum(gun$Freq)
gun <- gun[order(gun$Freq, decreasing = T),]
rownames(gun) <- 1:dim(gun)[1]
colnames(gun)[1] <- "gun"

library(RColorBrewer)
getPalette <- colorRampPalette(brewer.pal(12,"Set3"))
ggplot(data = gun[1:15,],mapping = aes(x=gun,y=Freq,fill=gun)) +
  geom_bar(stat = "identity") + scale_fill_manual(values = getPalette(15)) +
  geom_text(aes(label = Freq,vjust = -0.4)) + scale_x_discrete(limits = gun$gun[1:15]) +
  labs(title = "PUBG top fifteen firearms") +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle=25)) +
  guides(fill=FALSE)

# delete the 0 placement from data2
data3 <- data2[which(data2$victim_position_x!=0&data2$victim_position_y!=0&data2$killer_position_x!=0&data2$killer_position_y!=0),]
data3$distance <- sqrt((data3$killer_position_x-data3$victim_position_x)^2 + (data3$victim_position_y-data3$killer_position_y)^2)

data4 <- subset(data3, data3$killed_by %in% gun$gun[1:15])

# guntype
data4$guntype[data4$killed_by %in% c('AKM','M416','SCAR-L','M16A4')] <- 'Rifle'
data4$guntype[data4$killed_by %in% c('P18C','P1911','P92','R1895','R45')] <- 'Pistol'
data4$guntype[data4$killed_by %in% c('S1897','S686','S12K')] <- 'Shotgun'
data4$guntype[data4$killed_by %in% c('Kar98k','M24','AWM')] <- 'Sniper rifle1'
data4$guntype[data4$killed_by %in% c('Mini 14','SKS')] <- 'Sniper rifle2'
data4$guntype[is.na(data4$guntype)] <- 'Submachine gun'

library(ggplot2)
library(ggthemes)

ggplot(data = data4, aes(guntype, distance,fill = killed_by)) + geom_boxplot()

# seperate data4
data4.era <- data4[data4$map == "ERANGEL",]
data4.mir <- data4[data4$map == "MIRAMAR",]
data4.era[,c(4,5,11,12)] <- data4.era[,c(4,5,11,12)]*4096/800000
data4.mir[,c(4,5,11,12)] <- data4.mir[,c(4,5,11,12)]*1000/800000

as.era <- data4.era[data4.era$guntype == 'Rifle',]
sub.era <- data4.era[data4.era$guntype == 'Submachine gun',]
snip1.era <- data4.era[data4.era$guntype == 'Sniper rifle1',]
snip2.era <- data4.era[data4.era$guntype == 'Sniper rifle2',]
shot.era <- data4.era[data4.era$guntype == 'Shotgun',]
pistol.era <- data4.era[data4.era$guntype == 'Pistol',]

as.mir <- data4.mir[data4.mir$guntype == 'Rifle',]
sub.mir <- data4.mir[data4.mir$guntype == 'Submachine gun',]
snip1.mir <- data4.mir[data4.mir$guntype == 'Sniper rifle1',]
snip2.mir <- data4.mir[data4.mir$guntype == 'Sniper rifle2',]
shot.mir <- data4.mir[data4.mir$guntype == 'Shotgun',]
pistol.mir <- data4.mir[data4.mir$guntype == 'Pistol',]


ggplot(aes(x = killer_position_x,y=killer_position_y),data = shot.era) +
  annotation_custom(rasterGrob(erangel.map, width=unit(1,"npc"), height=unit(1,"npc"))) +
  ylim(4096,0) + xlim(0,4096) +
  guides(alpha=F) +
  stat_density_2d(aes(fill = ..level..,alpha = ..level..),geom = "polygon",bins = 50) +
  scale_fill_continuous(high = "red",low = "white") +
  labs(title = "Shotgun Usage Hotspot - Erangel") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(aes(x = killer_position_x,y=killer_position_y),data = snip1.era) +
  annotation_custom(rasterGrob(erangel.map, width=unit(1,"npc"), height=unit(1,"npc"))) +
  ylim(4096,0) + xlim(0,4096) +
  guides(alpha=F) +
  stat_density_2d(aes(fill = ..level..,alpha = ..level..),geom = "polygon",bins = 30) +
  scale_fill_continuous(high = "red",low = "white") +
  labs(title = "Sniper Rifle Usage Hotspot - Erangel") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(aes(x = killer_position_x,y=killer_position_y),data = pistol.mir) +
  annotation_custom(rasterGrob(miramar.map, width=unit(1,"npc"), height=unit(1,"npc"))) +
  ylim(1000,0) + xlim(0,1000) +
  guides(alpha=F) +
  stat_density_2d(aes(fill = ..level..,alpha = ..level..),geom = "polygon",bins = 50) +
  scale_fill_continuous(high = "red",low = "white") +
  labs(title = "Pistol Usage Hotspot - Miramar") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(aes(x = killer_position_x,y=killer_position_y),data = snip1.mir) +
  annotation_custom(rasterGrob(miramar.map, width=unit(1,"npc"), height=unit(1,"npc"))) +
  ylim(1000,0) + xlim(0,1000) +
  guides(alpha=F) +
  stat_density_2d(aes(fill = ..level..,alpha = ..level..),geom = "polygon",bins = 30) +
  scale_fill_continuous(high = "red",low = "white") +
  labs(title = "Sniper Rifle Usage Hotspot - Miramar") +
  theme(plot.title = element_text(hjust = 0.5))
