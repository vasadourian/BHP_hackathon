```{r, echo=FALSE, message=FALSE, warning=FALSE}

library(tidyverse)
library(ggplot2)
library(xts)
library(smooth)
library(splitstackshape)
library(caret)
library(RColorBrewer)
library(TTR)
library(gridExtra)

setwd("/home/vicken/Desktop/Challenge 2 Final Data")

df.1 <- read.table("Hackathon_DataSet_OctApr_Part1.txt", sep="\t", header=TRUE)
df.2 <- read.table("Hackathon_DataSet_OctApr_Part2.txt", sep="\t", header=TRUE)

df.total <- merge(df.1, df.2, all=TRUE, by='Id')

df.total$TimeStamp.x <- strptime(df.total$TimeStamp.x, format="%m/%d/%Y %I:%M:%S %p")

df.total <- df.total[-2]


prime_list <- c(
	"TimeStamp.x",
	"X05.PT.34101.04_H1_Manifold_Pressure..Psi.",
	"X05.TT.34101.04_H1_Manifold_Temperature..DegF." ,
	"X05.PT.29101.02_C1_Manifold_Pressure..Psi.",
	"X05.TT.29101.02_C1_Manifold_Temperature..DegF.",
	"X05.TT.33101.03_G1_Manifold_Temperature..DegF." ,
	"X05.PT.33101.03_G1_Manifold_Pressure..Psi.",
	"X05.PT.28201.01_B2_Manifold_Pressure..Psi." , 
	"X05.TT.28201.01_B2_Manifold_Temperature..DegF.",
	"X20.PT.10007.01.PV_Flowline_From_Drill_Center_C..PSIG.",   
	"X20.TT.10205.PV_Subsea_Flowline_Test_Sep..Deg.F.",      
	"X20.PT.20008.01.PV_Flowline_From_Drill_Centers_B.G..PSIG.",   
	"X20.TT.20105.PV_Train_2_Subsea_Flowline_Launcher..Deg.F.", 
	"X21.LT.10618.PV_Prod_Sep_2nd_Stg_Interface....",   
	"X21.LT.10620.PV_Prod_Sep_2nd_Stg_Interface....",   
	"X21.LY.10620.OUT_2nd_Stg_Hydrocyclone_Wtr_Out...."
	)

prime_list2 <- c(
	"X05.PT.34101.04_H1_Manifold_Pressure..Psi.",
	"X05.TT.34101.04_H1_Manifold_Temperature..DegF." ,
	"X05.PT.29101.02_C1_Manifold_Pressure..Psi.",
	"X05.TT.29101.02_C1_Manifold_Temperature..DegF.",
	"X05.TT.33101.03_G1_Manifold_Temperature..DegF." ,
	"X05.PT.33101.03_G1_Manifold_Pressure..Psi.",
	"X05.PT.28201.01_B2_Manifold_Pressure..Psi." , 
	"X05.TT.28201.01_B2_Manifold_Temperature..DegF.",
	"upsetClass"
	)

prime_list3 <- c(
	"X05.PT.34101.04_H1_Manifold_Pressure..Psi.",
	"X05.TT.34101.04_H1_Manifold_Temperature..DegF." ,
	"X05.PT.29101.02_C1_Manifold_Pressure..Psi.",
	"X05.TT.29101.02_C1_Manifold_Temperature..DegF.",
	"X05.TT.33101.03_G1_Manifold_Temperature..DegF." ,
	"X05.PT.33101.03_G1_Manifold_Pressure..Psi.",
	"X05.PT.28201.01_B2_Manifold_Pressure..Psi." , 
	"X05.TT.28201.01_B2_Manifold_Temperature..DegF.",
	"upsetNum"
	)

prime_list4 <- c(
	"X05.PT.34101.04_H1_Manifold_Pressure..Psi.",
	"X05.TT.34101.04_H1_Manifold_Temperature..DegF." ,
	"X05.PT.29101.02_C1_Manifold_Pressure..Psi.",
	"X05.TT.29101.02_C1_Manifold_Temperature..DegF.",
	"X05.TT.33101.03_G1_Manifold_Temperature..DegF." ,
	"X05.PT.33101.03_G1_Manifold_Pressure..Psi.",
	"X05.PT.28201.01_B2_Manifold_Pressure..Psi." , 
	"X05.TT.28201.01_B2_Manifold_Temperature..DegF.",
	"X20.PT.10007.01.PV_Flowline_From_Drill_Center_C..PSIG.",
	"X20.TT.10205.PV_Subsea_Flowline_Test_Sep..Deg.F.",
	"X20.PT.10008.01.PV_Flowline_From_Drill_Center_C..PSIG.",
	"X20.TT.10105.PV_Subsea_Flowline_To_Train_1..Deg.F.",
	"X20.PT.20008.01.PV_Flowline_From_Drill_Centers_B.G..PSIG.",
	"X20.TT.20105.PV_Train_2_Subsea_Flowline_Launcher..Deg.F.",
	"upsetClass"
	)




df.total$upsetNum <- NA
df.total$upsetClass <- NA

df.total$upsetNum[17000:20500] <- 1
df.total$upsetNum[60000:67000] <- 1
df.total$upsetNum[78690:79000] <- 1
df.total$upsetNum[110000:114000] <- 1
df.total$upsetNum[287000:292000] <- 1
df.total$upsetNum[410000:411000] <- 1

df.total$upsetNum[is.na(df.total$upsetNum)] <- 0
df.total$upsetClass[is.na(df.total$upsetClass)] <- 'N'
df.total$upsetClass <- ifelse(df.total$upsetNum == 1, 'Y', 'N')


df.upset <- subset(df.total, upsetNum == 1)
df.upset.expanded <- df.upset[rep(seq_len(nrow(df.upset)), 20), ]

df.total.expanded <- rbind(df.total, df.upset.expanded)

df.total.expanded.prime <- df.total.expanded[, prime_list2]
df.total.expanded.prime2 <- df.total.expanded[, prime_list3]

df.total.expanded.prime4 <- df.total.expanded[, prime_list4]

#ggplot(df.total, aes(x=TimeStamp.x, y=X21.LT.10618.PV_Prod_Sep_2nd_Stg_Interface....)) +
#	geom_line()

# str(df.total)

#"X21.LT.10618.PV_Prod_Sep_2nd_Stg_Interface...."                          
#"X21.LT.10620.PV_Prod_Sep_2nd_Stg_Interface...."   

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
##INTERFACE PLOTS
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# OCT 26 event plot

oct_26_2016.interface.plot <- ggplot(df.total[17000:23000,], aes(x=TimeStamp.x)) +
     geom_line(aes(y=X21.LT.10618.PV_Prod_Sep_2nd_Stg_Interface...., col="LT10618"), size=.1) + 
     geom_line(aes(y=X21.LT.10620.PV_Prod_Sep_2nd_Stg_Interface...., col="LT10620"), size=.1) +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     scale_colour_manual("",
     					breaks = c("LT10618", "LT10620"),
     					values = c("red", "darkblue")) +
     ggtitle("Oct 26 2016 Upset Event") + 
     ylab("interface") + 
     xlab("date")


nov_11_2016.interface.plot <- ggplot(df.total[63000:67000,], aes(x=TimeStamp.x)) +
     geom_line(aes(y=X21.LT.10618.PV_Prod_Sep_2nd_Stg_Interface...., col="LT10618"), size=.1) + 
     geom_line(aes(y=X21.LT.10620.PV_Prod_Sep_2nd_Stg_Interface...., col="LT10620"), size=.1) +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     scale_colour_manual("",
     					breaks = c("LT10618", "LT10620"),
     					values = c("red", "darkblue")) +
     ggtitle("Nov 11 2016 Upset Event") + 
     ylab("interface") + 
     xlab("date")


nov_16_2016.interface.plot <- ggplot(df.total[77000:82000,], aes(x=TimeStamp.x)) +
     geom_line(aes(y=X21.LT.10618.PV_Prod_Sep_2nd_Stg_Interface...., col="LT10618"), size=.1) + 
     geom_line(aes(y=X21.LT.10620.PV_Prod_Sep_2nd_Stg_Interface...., col="LT10620"), size=.1) +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     scale_colour_manual("",
     					breaks = c("LT10618", "LT10620"),
     					values = c("red", "darkblue")) +
     ggtitle("Nov 16 2016 Upset Event") + 
     ylab("interface") + 
     xlab("date")


nov_27_2016.interface.plot <- ggplot(df.total[109000:113000,], aes(x=TimeStamp.x)) +
     geom_line(aes(y=X21.LT.10618.PV_Prod_Sep_2nd_Stg_Interface...., col="LT10618"), size=.1) + 
     geom_line(aes(y=X21.LT.10620.PV_Prod_Sep_2nd_Stg_Interface...., col="LT10620"), size=.1) +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     scale_colour_manual("",
     					breaks = c("LT10618", "LT10620"),
     					values = c("red", "darkblue")) +
     ggtitle("Nov 27 2016 Upset Event") + 
     ylab("interface") + 
     xlab("date")

jan_28_2017.interface.plot <- ggplot(df.total[287500:291500,], aes(x=TimeStamp.x)) +
     geom_line(aes(y=X21.LT.10618.PV_Prod_Sep_2nd_Stg_Interface...., col="LT10618"), size=.1) + 
     geom_line(aes(y=X21.LT.10620.PV_Prod_Sep_2nd_Stg_Interface...., col="LT10620"), size=.1) +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     scale_colour_manual("",
     					breaks = c("LT10618", "LT10620"),
     					values = c("red", "darkblue")) +
     ggtitle("Jan 28 2017 Upset Event") + 
     ylab("interface") + 
     xlab("date")


mar_12_2017.interface.plot <- ggplot(df.total[410000:414000,], aes(x=TimeStamp.x)) +
     geom_line(aes(y=X21.LT.10618.PV_Prod_Sep_2nd_Stg_Interface...., col="LT10618"), size=.1) + 
     geom_line(aes(y=X21.LT.10620.PV_Prod_Sep_2nd_Stg_Interface...., col="LT10620"), size=.1) +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     scale_colour_manual("",
     					breaks = c("LT10618", "LT10620"),
     					values = c("red", "darkblue")) +
     ggtitle("Mar 12 2017 Upset Event") + 
     ylab("interface") + 
     xlab("date")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# P2 - H PLOTS
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#05-TT-34101-04_H1_Manifold_Temperature
#05-PT-34101-04_H1_Manifold_Pressure

#"X05.TT.34101.04_H1_Manifold_Temperature..DegF."                          
#"X05.PT.34101.04_H1_Manifold_Pressure..Psi." 

oct_26_2016.34101_04.plot <- ggplot(df.total[17000:23000,], aes(x=TimeStamp.x)) +
     geom_line(aes(y=X05.TT.34101.04_H1_Manifold_Temperature..DegF., col="34101-04-T"), size=.1) + 
     geom_line(aes(y=X05.PT.34101.04_H1_Manifold_Pressure..Psi., col="34101-04-P"), size=.1) +
     geom_vline(aes(xintercept=df.total$TimeStamp.x[18331]), col="green") +
     geom_text(aes(x=df.total$TimeStamp.x[18700], label="Upset Event", y=1500), angle=90, color="red", text=element_text(size=3)) +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     scale_colour_manual("",
     					breaks = c("34101-04-T", "34101-04-P"),
     					values = c("red", "darkblue")) +
     ggtitle("Oct 26 2016 Upset Event") + 
     ylab("manifold") + 
     xlab("date")


nov_11_2016.34101_04.plot <- ggplot(df.total[63000:67000,], aes(x=TimeStamp.x)) +
     geom_line(aes(y=X05.TT.34101.04_H1_Manifold_Temperature..DegF., col="34101-04-T"), size=.1) + 
     geom_line(aes(y=X05.PT.34101.04_H1_Manifold_Pressure..Psi., col="34101-04-P"), size=.1) +
     geom_vline(aes(xintercept=df.total$TimeStamp.x[64441]), col="green") +
     geom_text(aes(x=df.total$TimeStamp.x[65000], label="Upset Event", y=500), angle=90, color="red", text=element_text(size=3)) +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     scale_colour_manual("",
     					breaks = c("34101-04-T", "34101-04-P"),
     					values = c("red", "darkblue")) +
     ggtitle("Nov 11 2016 Upset Event") + 
     ylab("manifold") + 
     xlab("date")


nov_16_2016.34101_04.plot <- ggplot(df.total[77000:82000,], aes(x=TimeStamp.x)) +
     geom_line(aes(y=X05.TT.34101.04_H1_Manifold_Temperature..DegF., col="34101-04-T"), size=.1) + 
     geom_line(aes(y=X05.PT.34101.04_H1_Manifold_Pressure..Psi., col="34101-04-P"), size=.1) +
     geom_vline(aes(xintercept=df.total$TimeStamp.x[78691]), col="green") +
     geom_text(aes(x=df.total$TimeStamp.x[79300], label="Upset Event", y=4000), angle=90, color="red", text=element_text(size=3)) +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     scale_colour_manual("",
     					breaks = c("34101-04-T", "34101-04-P"),
     					values = c("red", "darkblue")) +
     ggtitle("Nov 16 2016 Upset Event") + 
     ylab("manifold") + 
     xlab("date")


nov_27_2016.34101_04.plot <- ggplot(df.total[109000:113000,], aes(x=TimeStamp.x)) +
     geom_line(aes(y=X05.TT.34101.04_H1_Manifold_Temperature..DegF., col="34101-04-T"), size=.1) + 
     geom_line(aes(y=X05.PT.34101.04_H1_Manifold_Pressure..Psi., col="34101-04-P"), size=.1) +
     geom_vline(aes(xintercept=df.total$TimeStamp.x[110521]), col="green") +
     geom_text(aes(x=df.total$TimeStamp.x[111000], label="Upset Event", y=1000), angle=90, color="red", text=element_text(size=3)) +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     scale_colour_manual("",
     					breaks = c("34101-04-T", "34101-04-P"),
     					values = c("red", "darkblue")) +
     ggtitle("Nov 27 2016 Upset Event") + 
     ylab("manifold") + 
     xlab("date")

jan_28_2017.34101_04.plot <- ggplot(df.total[287500:291500,], aes(x=TimeStamp.x)) +
     geom_line(aes(y=X05.TT.34101.04_H1_Manifold_Temperature..DegF., col="34101-04-T"), size=.1) + 
     geom_line(aes(y=X05.PT.34101.04_H1_Manifold_Pressure..Psi., col="34101-04-P"), size=.1) +
     geom_vline(aes(xintercept=df.total$TimeStamp.x[288991]), col="green") +
     geom_text(aes(x=df.total$TimeStamp.x[289300], label="Upset Event", y=500), angle=90, color="red", text=element_text(size=3)) +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     scale_colour_manual("",
     					breaks = c("34101-04-T", "34101-04-P"),
     					values = c("red", "darkblue")) +
     ggtitle("Jan 28 2017 Upset Event") + 
     ylab("manifold") + 
     xlab("date")


mar_12_2017.34101_04.plot <- ggplot(df.total[410000:414000,], aes(x=TimeStamp.x)) +
     geom_line(aes(y=X05.TT.34101.04_H1_Manifold_Temperature..DegF., col="34101-04-T"), size=.1) + 
     geom_line(aes(y=X05.PT.34101.04_H1_Manifold_Pressure..Psi., col="34101-04-P"), size=.1) +
     geom_vline(aes(xintercept=df.total$TimeStamp.x[412681]), col="green") +
     geom_text(aes(x=df.total$TimeStamp.x[413000], label="Upset Event", y=500), angle=90, color="red", text=element_text(size=3)) +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     scale_colour_manual("",
     					breaks = c("34101-04-T", "34101-04-P"),
     					values = c("red", "darkblue")) +
     ggtitle("Mar 12 2017 Upset Event") + 
     ylab("manifold") + 
     xlab("date")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# P2 - C PLOTS
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#X05.PT.29101.02_C1_Manifold_Pressure..Psi.
#X05.TT.29101.02_C1_Manifold_Temperature..DegF.
   

oct_26_2016.29101_02.plot <- ggplot(df.total[17000:23000,], aes(x=TimeStamp.x)) +
     geom_line(aes(y=X05.TT.29101.02_C1_Manifold_Temperature..DegF., col="29101-02-T"), size=.1) + 
     geom_line(aes(y=X05.PT.34101.04_H1_Manifold_Pressure..Psi., col="29101-02-P"), size=.1) +
     geom_vline(aes(xintercept=df.total$TimeStamp.x[18331]), col="green") +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     scale_colour_manual("",
                              breaks = c("29101-02-T", "29101-02-P"),
                              values = c("red", "darkblue")) +
     ggtitle("Oct 26 2016 Upset Event") + 
     ylab("manifold") + 
     xlab("date")


nov_11_2016.29101_02.plot <- ggplot(df.total[63000:67000,], aes(x=TimeStamp.x)) +
     geom_line(aes(y=X05.TT.29101.02_C1_Manifold_Temperature..DegF., col="29101-02-T"), size=.1) + 
     geom_line(aes(y=X05.PT.34101.04_H1_Manifold_Pressure..Psi., col="29101-02-P"), size=.1) +
     geom_vline(aes(xintercept=df.total$TimeStamp.x[64441]), col="green") +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     scale_colour_manual("",
                              breaks = c("29101-02-T", "29101-02-P"),
                              values = c("red", "darkblue")) +
     ggtitle("Nov 11 2016 Upset Event") + 
     ylab("manifold") + 
     xlab("date")


nov_16_2016.29101_02.plot <- ggplot(df.total[77000:82000,], aes(x=TimeStamp.x)) +
     geom_line(aes(y=X05.TT.29101.02_C1_Manifold_Temperature..DegF., col="29101-02-T"), size=.1) + 
     geom_line(aes(y=X05.PT.34101.04_H1_Manifold_Pressure..Psi., col="29101-02-P"), size=.1) +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     scale_colour_manual("",
                              breaks = c("29101-02-T", "29101-02-P"),
                              values = c("red", "darkblue")) +
     ggtitle("Nov 16 2016 Upset Event") + 
     ylab("manifold") + 
     xlab("date")


nov_27_2016.29101_02.plot <- ggplot(df.total[109000:113000,], aes(x=TimeStamp.x)) +
     geom_line(aes(y=X05.TT.29101.02_C1_Manifold_Temperature..DegF., col="29101-02-T"), size=.1) + 
     geom_line(aes(y=X05.PT.34101.04_H1_Manifold_Pressure..Psi., col="29101-02-P"), size=.1) +
     geom_vline(aes(xintercept=df.total$TimeStamp.x[110521]), col="green") +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     scale_colour_manual("",
                              breaks = c("29101-02-T", "29101-02-P"),
                              values = c("red", "darkblue")) +
     ggtitle("Nov 27 2016 Upset Event") + 
     ylab("manifold") + 
     xlab("date")

jan_28_2017.29101_02.plot <- ggplot(df.total[287500:291500,], aes(x=TimeStamp.x)) +
     geom_line(aes(y=X05.TT.29101.02_C1_Manifold_Temperature..DegF., col="29101-02-T"), size=.1) + 
     geom_line(aes(y=X05.PT.34101.04_H1_Manifold_Pressure..Psi., col="29101-02-P"), size=.1) +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     scale_colour_manual("",
                              breaks = c("29101-02-T", "29101-02-P"),
                              values = c("red", "darkblue")) +
     ggtitle("Jan 28 2017 Upset Event") + 
     ylab("manifold") + 
     xlab("date")


mar_12_2017.29101_02.plot <- ggplot(df.total[410000:414000,], aes(x=TimeStamp.x)) +
     geom_line(aes(y=X05.TT.29101.02_C1_Manifold_Temperature..DegF., col="29101-02-T"), size=.1) + 
     geom_line(aes(y=X05.PT.34101.04_H1_Manifold_Pressure..Psi., col="29101-02-P"), size=.1) +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     scale_colour_manual("",
                              breaks = c("29101-02-T", "29101-02-P"),
                              values = c("red", "darkblue")) +
     ggtitle("Mar 12 2017 Upset Event") + 
     ylab("manifold") + 
     xlab("date")




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Clustering
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

station_h.df <- data.frame(df.total$X05.TT.34101.04_H1_Manifold_Temperature..DegF., df.total$X05.PT.34101.04_H1_Manifold_Pressure..Psi.)
station_h.df <- station_h.df %>% filter(df.total.X05.PT.34101.04_H1_Manifold_Pressure..Psi. < 4000)

station_h.kmeans <- kmeans(station_h.df, centers=2, nstart=40, iter.max=10)

station_h.cluster.plot <- plot(station_h.df, col=station_h.kmeans$cluster)


wss <- 0

for (i in 1:15) {
  km.out <- kmeans(station_h.df, centers = i, nstart = 40, iter.max = 50)
  wss[i] <- km.out$tot.withinss}

roc.plot <- plot(1:15, wss, type = "b", 
     			xlab = "Number of Clusters", 
     			ylab = "Within groups sum of squares")

df.total.extra <- df.total
df.total.extra <- df.total.extra %>% filter(X05.PT.34101.04_H1_Manifold_Pressure..Psi. < 4000)

myColors <- brewer.pal(5,"Set1")
names(myColors) <- levels(df.total$upsetClass)
colScale <- scale_colour_manual(name = "upsetClass",values = myColors)
station_h.upset.plot <- ggplot(df.total, aes(x=X05.TT.34101.04_H1_Manifold_Temperature..DegF., y=X05.PT.34101.04_H1_Manifold_Pressure..Psi., color=upsetClass)) + 
	geom_point(alpha=.3) + 
	colScale +
	ylim(400, 2600) + xlim(40,110)


df.total.copy <- df.total
df.total.copy$TimeStamp.x <- as.POSIXct(df.total.copy$TimeStamp.x, format = "%Y-%m-%d %I:%M:%S")

p <- df.total.copy %>%
	plot_ly(
	x = ~TimeStamp.x,
	y= ~X05.PT.34101.04_H1_Manifold_Pressure..Psi.,
	type='scatter',
	mode='markers') %>%
	layout(
		xaxis = list(
			type='log'
			)
		)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Outlier removal FT
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#X21.FT.40518.03_Gross_Volume_Flow_Rate_.Coriolis...bbl.d.
#X21.FQI.10518.01.NetRate.PV..BPD.

#ggplot(df.total, aes(x=TimeStamp.x, y=X21.FT.40518.03_Gross_Volume_Flow_Rate_.Coriolis...bbl.d.)) + geom_line()



# P2
#"X20.PT.10007.01.PV_Flowline_From_Drill_Center_C..PSIG."
#"X20.TT.10205.PV_Subsea_Flowline_Test_Sep..Deg.F."     

time_series_list1 <- c(
	"X05.TT.34101.04_H1_Manifold_Temperature..DegF.",
	"X05.PT.34101.04_H1_Manifold_Pressure..Psi.",
	"X05.PT.29101.02_C1_Manifold_Pressure..Psi.",
	"X05.TT.29101.02_C1_Manifold_Temperature..DegF.",
	"X05.TT.33101.03_G1_Manifold_Temperature..DegF.",
	"X05.PT.33101.03_G1_Manifold_Pressure..Psi.",
	"X05.PT.28201.01_B2_Manifold_Pressure..Psi.", 
	"X05.TT.28201.01_B2_Manifold_Temperature..DegF.",
	"X21.LT.10618.PV_Prod_Sep_2nd_Stg_Interface....",
	"X21.LT.10620.PV_Prod_Sep_2nd_Stg_Interface....",
	"X20.PT.10007.01.PV_Flowline_From_Drill_Center_C..PSIG.",
	"X20.TT.10205.PV_Subsea_Flowline_Test_Sep..Deg.F.",
	
	 "X20.PT.10007.01.PV_Flowline_From_Drill_Center_C..PSIG.",
	 "X20.TT.10205.PV_Subsea_Flowline_Test_Sep..Deg.F." 
	  )

ts.total1 <- xts(df.total[, time_series_list1], order.by = df.total[,"TimeStamp.x"])



time_series_list2 <- c(
	"X21.LT.10618.PV_Prod_Sep_2nd_Stg_Interface....",
	"X21.LT.10620.PV_Prod_Sep_2nd_Stg_Interface....",
	"X05.PT.29101.02_C1_Manifold_Pressure..Psi.",
	"X05.TT.29101.02_C1_Manifold_Temperature..DegF.",
	"X20.PT.10007.01.PV_Flowline_From_Drill_Center_C..PSIG.",
	"X20.TT.10205.PV_Subsea_Flowline_Test_Sep..Deg.F.",
	"X05.TT.33101.03_G1_Manifold_Temperature..DegF.",
	"X05.PT.33101.03_G1_Manifold_Pressure..Psi.",
	"X05.PT.28201.01_B2_Manifold_Pressure..Psi.", 
	"X05.TT.28201.01_B2_Manifold_Temperature..DegF."
	 )

ts.total2 <- xts(df.total[, time_series_list2], order.by = df.total[,"TimeStamp.x"])


time_series_list3 <- c(
	"X05.TT.34101.04_H1_Manifold_Temperature..DegF.",
	"X05.PT.34101.04_H1_Manifold_Pressure..Psi.",
	"X05.PT.29101.02_C1_Manifold_Pressure..Psi.",
	"X05.TT.29101.02_C1_Manifold_Temperature..DegF.",
	"X05.TT.33101.03_G1_Manifold_Temperature..DegF.",
	"X05.PT.33101.03_G1_Manifold_Pressure..Psi.",
	"X05.PT.28201.01_B2_Manifold_Pressure..Psi.", 
	"X05.TT.28201.01_B2_Manifold_Temperature..DegF."
)

ts.total3 <- xts(df.total[, time_series_list3], order.by = df.total[,"TimeStamp.x"])


ts.total3.1 <- ts.total3[1:46000]

X05.TT.34101.04_H1_Manifold_Temperature..DegF.MA <- sma(ts.total3.1$X05.TT.34101.04_H1_Manifold_Temperature..DegF., h=240)
X05.PT.34101.04_H1_Manifold_Pressure..Psi.MA <- sma(ts.total3.1$X05.PT.34101.04_H1_Manifold_Pressure..Psi., h=240)
X05.TT.29101.02_C1_Manifold_Temperature..DegF.MA <- sma(ts.total3.1$X05.TT.29101.02_C1_Manifold_Temperature..DegF., h=240)
X05.PT.29101.02_C1_Manifold_Pressure..Psi.MA <- sma(ts.total3.1$X05.PT.29101.02_C1_Manifold_Pressure..Psi., h=240)
X05.TT.33101.03_G1_Manifold_Temperature..DegF.MA <- sma(ts.total3.1$X05.TT.33101.03_G1_Manifold_Temperature..DegF., h=240)
X05.PT.33101.03_G1_Manifold_Pressure..Psi.MA <- sma(ts.total3.1$X05.PT.33101.03_G1_Manifold_Pressure..Psi., h=240)
X05.TT.28201.01_B2_Manifold_Temperature..DegF.MA <- sma(ts.total3.1$X05.TT.28201.01_B2_Manifold_Temperature..DegF., h=240)
X05.PT.28201.01_B2_Manifold_Pressure..Psi.MA <- sma(ts.total3.1$X05.PT.28201.01_B2_Manifold_Pressure..Psi., h=240)




# create plots for the time series
# multiple geom lines, with a legend
#ggplot(ts.total2, aes(x=Index, y=X21.LT.10618.PV_Prod_Sep_2nd_Stg_Interface....)) + geom_point()






c <- sma(df.oct_event$X05.PT.28201.01_B2_Manifold_Pressure..Psi., h=240)
df.oct_event$moving_average <- c$fitted
ggplot(df.oct_event, aes(x=TimeStamp.x, y=d)) + geom_line()

#"X21.LT.10618.PV_Prod_Sep_2nd_Stg_Interface...."                          
#"X21.LT.10620.PV_Prod_Sep_2nd_Stg_Interface...."

lt_10618_ma <- sma(df.oct_event$X21.LT.10618.PV_Prod_Sep_2nd_Stg_Interface...., h=240)
df.oct_event$lt_10618_maf <- lt_10618_ma$fitted


ggplot(df.oct_event, aes(x=TimeStamp.x)) + 
	geom_line(aes(y=lt_10618_maf))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Hourly Dataframe
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# "X05.PT.34101.04_H1_Manifold_Pressure..Psi."   
# "X05.TT.34101.04_H1_Manifold_Temperature..DegF." 
# "X05.PT.29101.02_C1_Manifold_Pressure..Psi."
# "X05.TT.29101.02_C1_Manifold_Temperature..DegF."
# "X05.TT.33101.03_G1_Manifold_Temperature..DegF." 
# "X05.PT.33101.03_G1_Manifold_Pressure..Psi."
# "X05.PT.28201.01_B2_Manifold_Pressure..Psi."  
# "X05.TT.28201.01_B2_Manifold_Temperature..DegF."
# "X20.PT.10007.01.PV_Flowline_From_Drill_Center_C..PSIG."   
# "X20.TT.10205.PV_Subsea_Flowline_Test_Sep..Deg.F."      
# "X20.PT.20008.01.PV_Flowline_From_Drill_Centers_B.G..PSIG."   
# "X20.TT.20105.PV_Train_2_Subsea_Flowline_Launcher..Deg.F."  



# "X21.LT.10618.PV_Prod_Sep_2nd_Stg_Interface...."   
# "X21.LT.10620.PV_Prod_Sep_2nd_Stg_Interface...."   
# "X21.LY.10620.OUT_2nd_Stg_Hydrocyclone_Wtr_Out...."  




hourly_df_list <- c(
	"TimeStamp.x",
	"X05.PT.34101.04_H1_Manifold_Pressure..Psi.",
	"X05.TT.34101.04_H1_Manifold_Temperature..DegF." ,
	"X05.PT.29101.02_C1_Manifold_Pressure..Psi.",
	"X05.TT.29101.02_C1_Manifold_Temperature..DegF.",
	"X05.TT.33101.03_G1_Manifold_Temperature..DegF." ,
	"X05.PT.33101.03_G1_Manifold_Pressure..Psi.",
	"X05.PT.28201.01_B2_Manifold_Pressure..Psi." , 
	"X05.TT.28201.01_B2_Manifold_Temperature..DegF.",
	"X20.PT.10007.01.PV_Flowline_From_Drill_Center_C..PSIG.",   
	"X20.TT.10205.PV_Subsea_Flowline_Test_Sep..Deg.F.",      
	"X20.PT.20008.01.PV_Flowline_From_Drill_Centers_B.G..PSIG.",   
	"X20.TT.20105.PV_Train_2_Subsea_Flowline_Launcher..Deg.F.", 
	"X21.LT.10618.PV_Prod_Sep_2nd_Stg_Interface....",   
	"X21.LT.10620.PV_Prod_Sep_2nd_Stg_Interface....",   
	"X21.LY.10620.OUT_2nd_Stg_Hydrocyclone_Wtr_Out...."
	)

df.total.hourly.source <- df.total[, hourly_df_list]


# Building a dataframe of half hour means

df.total.hourly <- aggregate(df.total.hourly, 
               list(hour=cut(df.total.hourly$TimeStamp.x, breaks="hour")),
               mean, na.rm=TRUE)

df.total.hourly$avg.flow.rate <- mean(df.total.hourly$X21.LT.10618.PV_Prod_Sep_2nd_Stg_Interface....,   
	df.total.hourly$X21.LT.10620.PV_Prod_Sep_2nd_Stg_Interface....)



df.total$X05.TT.34101.04_H1_Manifold_Temperature..DegF.MA <- SMA(df.total$X05.TT.34101.04_H1_Manifold_Temperature..DegF., h=240)
df.total$X05.PT.34101.04_H1_Manifold_Pressure..Psi.MA <- SMA(df.total$X05.PT.34101.04_H1_Manifold_Pressure..Psi., h=240)
df.total$X05.TT.29101.02_C1_Manifold_Temperature..DegF.MA <- SMA(df.total$X05.TT.29101.02_C1_Manifold_Temperature..DegF., h=240)
df.total$X05.PT.29101.02_C1_Manifold_Pressure..Psi.MA <- SMA(df.total$X05.PT.29101.02_C1_Manifold_Pressure..Psi., h=240)
df.total$X05.TT.33101.03_G1_Manifold_Temperature..DegF.MA <- SMA(df.total$X05.TT.33101.03_G1_Manifold_Temperature..DegF., h=240)
df.total$X05.PT.33101.03_G1_Manifold_Pressure..Psi.MA <- SMA(df.total$X05.PT.33101.03_G1_Manifold_Pressure..Psi., h=240)
df.total$X05.TT.28201.01_B2_Manifold_Temperature..DegF.MA <- SMA(df.total$X05.TT.28201.01_B2_Manifold_Temperature..DegF., h=240)
df.total$X05.PT.28201.01_B2_Manifold_Pressure..Psi.MA <- SMA(df.total$X05.PT.28201.01_B2_Manifold_Pressure..Psi., h=240)





upset_list3 <- as.Date(upset_list2, format = "%Y-%m-%d")
unique(upset_list3)


df.total$test_sma3 <- SMA(df.total$X05.TT.33101.03_G1_Manifold_Temperature..DegF., n=240)

ggplot(df.total, aes(x=TimeStamp.x, y=test_sma3)) + geom_line() + scale_x_datetime(date_minor_breaks = "3 days")

df.total$test_sma4 <- SMA(df.total$X05.TT.34101.04_H1_Manifold_Temperature..DegF., n=240)

# oct 26 - 8:45 AM 17000:20500
# NOV 11 - 8 AM 60000:67000
# Nov 16 - 6:45 AM 78690:79000
# Nov 27 6 AM 110000:114000
# Jan 28 7:15 AM 287000:292000
# Mar 12 7 AM 410000:411000

# which(df.total$TimeStamp.x == '2016-10-26 06:45:00')
# ggplot(df.total[17000:19000,], aes(x=TimeStamp.x, y=X05.PT.34101.04_H1_Manifold_Pressure..Psi.)) + geom_line() 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Random Forest
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



#RF.model <- train(
#  upsetClass ~ .,
#  tuneLength = 2,
#  data = df.total.expanded.prime, method = "ranger",
#  trControl = trainControl(method = "cv", number = 3, verboseIter = TRUE)
#)


#logisticControl <- trainControl(
#  method = "cv",
#  number = 10,
#  summaryFunction = twoClassSummary,
#  classProbs = TRUE,
#  verboseIter = TRUE
#)

#logistic.model <- train(upsetNum ~ ., method="glm", df.total.expanded.prime2, trControl = logisticControl)

#logistic.model <- train(upsetClass ~ ., 
#	method="glm", df.total.expanded.prime, trControl = logisticControl)

#logistic.model4 <- train(upsetClass ~ ., 
#	method="glm", df.total.expanded.prime, trControl = logisticControl)


```
df.total.expanded.prime$predictions <- predict(logistic.model, newdata=df.total.expanded.prime)


df.total.expanded.prime$correct <- ifelse(df.total.expanded.prime$upsetClass == df.total.expanded.prime$predictions, 'Y', 'N')

ggplot(df.total.expanded.prime, aes(x=X05.TT.34101.04_H1_Manifold_Temperature..DegF., y=X05.PT.34101.04_H1_Manifold_Pressure..Psi., col=correct )) + 
     geom_point(alpha = .1) + ylim(400,2600) + xlim(40,110)











```{r, echo=FALSE, warning=FALSE, message=FALSE, fig.width=15}
grid.arrange(oct_26_2016.interface.plot, nov_11_2016.interface.plot,
	nov_16_2016.interface.plot, nov_27_2016.interface.plot,
	jan_28_2017.interface.plot, mar_12_2017.interface.plot,
	ncol=2
	)
```

```{r, echo=FALSE, warning=FALSE, message=FALSE, fig.width=15}
grid.arrange(oct_26_2016.34101_04.plot, nov_11_2016.34101_04.plot,
	nov_16_2016.34101_04.plot, nov_27_2016.34101_04.plot, 
	jan_28_2017.34101_04.plot, mar_12_2017.34101_04.plot,
	ncol=2)

```

```{r, echo=FALSE, warning=FALSE, message=FALSE, fig.width=15}

box1.40518.03 <- ggplot(df.total[df.total$upsetClass == 'Y',], aes(x=X21.FT.40518.03_Gross_Volume_Flow_Rate_.Coriolis...bbl.d., y=X21.FT.40518.03_Gross_Volume_Flow_Rate_.Coriolis...bbl.d.)) + 
	geom_boxplot(col="red")  + 
	ggtitle("Flow rate of FT-40518-03 during upset events") +
	xlab("FT-40518-03") + 
	ylab("FT-40518-03")

box2.40518.03 <- ggplot(df.total[df.total$upsetClass == 'N',], aes(x=X21.FT.40518.03_Gross_Volume_Flow_Rate_.Coriolis...bbl.d., y=X21.FT.40518.03_Gross_Volume_Flow_Rate_.Coriolis...bbl.d.)) + 
	geom_boxplot(col="turquoise")  + 
	ggtitle("Flow rate of FT-40518-03 during normal activity") +
	xlab("FT-40518-03") + 
	ylab("FT-40518-03")

upset_summary.40518.03 <- summary(df.total$X21.FT.40518.03_Gross_Volume_Flow_Rate_.Coriolis...bbl.d.[df.total$upsetClass=='Y'])

normal_summary.40518.03 <- summary(df.total$X21.FT.40518.03_Gross_Volume_Flow_Rate_.Coriolis...bbl.d.[df.total$upsetClass=='N'])

grid.arrange(box1.40518.03, box2.40518.03, ncol=2)


box1.10518.01 <- ggplot(df.total[df.total$upsetClass == 'Y',], aes(x=X21.FQI.10518.01.NetRate.PV..BPD., y=X21.FQI.10518.01.NetRate.PV..BPD.)) + 
	geom_boxplot(col="red")  + 
	ggtitle("Flow rate of FQI-10518-01 during upset events") +
	xlab("FQI-10518-01") + 
	ylab("FQI-10518-01")

box2.10518.01 <- ggplot(df.total[df.total$upsetClass == 'N',], aes(x=X21.FQI.10518.01.NetRate.PV..BPD., y=X21.FQI.10518.01.NetRate.PV..BPD.)) + 
	geom_boxplot(col="turquoise")  + 
	ggtitle("Flow rate of FQI-10518-01 during normal activity") +
	xlab("FQI-10518-01") + 
	ylab("FQI-10518-01")

upset_summary.10518.01 <- summary(df.total$X21.FQI.10518.01.NetRate.PV..BPD.[df.total$upsetClass=='Y'])

normal_summary.10518.01 <- summary(df.total$X21.FQI.10518.01.NetRate.PV..BPD.[df.total$upsetClass=='N'])

grid.arrange(box1.10518.01, box2.10518.01, ncol=2)

```

```{r, echo=FALSE, warning=FALSE, message=FALSE, fig.width=15}
roc.plot
```


```{r, echo=FALSE, warning=FALSE, message=FALSE, fig.width=15}
station_h.cluster.plot 
```

```{r, echo=FALSE, warning=FALSE, message=FALSE, fig.width=15}
station_h.upset.plot
```
