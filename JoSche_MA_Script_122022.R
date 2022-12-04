library(tidyverse, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(xts, warn.conflicts = FALSE)
library(forecastML, warn.conflicts = FALSE)
library(imputeTS, warn.conflicts = FALSE)
library(zoo, warn.conflicts = FALSE)
library(forecast, warn.conflicts = FALSE)
library(gridExtra, warn.conflicts = FALSE)
library(lfstat, warn.conflicts = FALSE)
library(grid, warn.conflicts = FALSE)
library(FlowScreen, warn.conflicts = FALSE)
library(tidyquant, warn.conflicts = FALSE)
library(data.table, warn.conflicts = FALSE)
library(highfrequency, warn.conflicts = FALSE)
library(randomForest, warn.conflicts = FALSE)
library(randomForestExplainer, warn.conflicts = FALSE)
library(plotmo, warn.conflicts = FALSE)
library(reshape2, warn.conflicts = FALSE)
library(descomponer, warn.conflicts = FALSE)

Sys.setenv(TZ='GMT')
rm(list = ls())

#### directories
in.dir = "./inputs/"
out.dir =  "./outputs/"

#### load data frames
ACC <- read.csv("./inputs/ACC_Data_08092022.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)
HND <- read.csv("./inputs/HND_Data_08092022.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)
NNC <- read.csv("./inputs/NNC_Data_08092022.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)
SBH <- read.csv("./inputs/SBH_Data_08092022.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)
SMC <- read.csv("./inputs/SMC_Data_08092022.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)
TMR <- read.csv("./inputs/TMR_Data_08092022.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)
WEB <- read.csv("./inputs/WEB_Data_08092022.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)

#### as POSIXct
ACC$datetime <- ymd_hms(ACC$datetime, tz = "GMT")
HND$datetime <- ymd_hms(HND$datetime, tz = "GMT")
NNC$datetime <- ymd_hms(NNC$datetime, tz = "GMT")
SBH$datetime <- ymd_hms(SBH$datetime, tz = "GMT")
SMC$datetime <- ymd_hms(SMC$datetime, tz = "GMT")
TMR$datetime <- ymd_hms(TMR$datetime, tz = "GMT")
WEB$datetime <- ymd_hms(WEB$datetime, tz = "GMT")
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#                            XTS objects for daily aggregated data                         #
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
ACCX <- xts(ACC, order.by = as.POSIXct(ACC$datetime, format = "%y-%m-%d %H:%M:%S", tzone = "GMT"), unique = TRUE)
HNDX <- xts(HND, order.by = as.POSIXct(HND$datetime, format = "%y-%m-%d %H:%M:%S", tzone = "GMT"), unique = TRUE)
NNCX <- xts(NNC, order.by = as.POSIXct(NNC$datetime, format = "%y-%m-%d %H:%M:%S", tzone = "GMT"), unique = TRUE)
SBHX <- xts(SBH, order.by = as.POSIXct(SBH$datetime, format = "%y-%m-%d %H:%M:%S", tzone = "GMT"), unique = TRUE)
SMCX <- xts(SMC, order.by = as.POSIXct(SMC$datetime, format = "%y-%m-%d %H:%M:%S", tzone = "GMT"), unique = TRUE)
TMRX <- xts(TMR, order.by = as.POSIXct(TMR$datetime, format = "%y-%m-%d %H:%M:%S", tzone = "GMT"), unique = TRUE)
WEBX <- xts(WEB, order.by = as.POSIXct(WEB$datetime, format = "%y-%m-%d %H:%M:%S", tzone = "GMT"), unique = TRUE)

ACCX$datetime = NULL
HNDX$datetime = NULL
NNCX$datetime = NULL
SBHX$datetime = NULL
SMCX$datetime = NULL
TMRX$datetime = NULL
WEBX$datetime = NULL

ACCXEP <- endpoints(ACCX, "days")
HNDXEP <- endpoints(HNDX, "days")
NNCXEP <- endpoints(NNCX, "days")
SBHXEP <- endpoints(SBHX, "days")
SMCXEP <- endpoints(SMCX, "days")
TMRXEP <- endpoints(TMRX, "days")
WEBXEP <- endpoints(WEBX, "days")

ACCXDM <- period.apply(ACCX, INDEX = ACCXEP, FUN = mean)
HNDXDM <- period.apply(HNDX, INDEX = HNDXEP, FUN = mean)
NNCXDM <- period.apply(NNCX, INDEX = NNCXEP, FUN = mean)
SBHXDM <- period.apply(SBHX, INDEX = SBHXEP, FUN = mean)
SMCXDM <- period.apply(SMCX, INDEX = SMCXEP, FUN = mean)
TMRXDM <- period.apply(TMRX, INDEX = TMRXEP, FUN = mean)
WEBXDM <- period.apply(WEBX, INDEX = WEBXEP, FUN = mean)

#### filling date gaps within the ACC Daily XTS
ACCFillGaps <- fortify.zoo(ACCXDM)
names(ACCFillGaps)[1] <- "datetime"
ACCFillGaps <- fill_gaps(ACCFillGaps, date_col = 1, frequency = "days")
ACCFillGaps1 <- na_seadec(ACCFillGaps, algorithm = "ma", find_frequency = TRUE, maxgap = 1)
ACCX <- xts(ACCFillGaps1, order.by = as.POSIXct(ACCFillGaps1$datetime, format = "%y-%m-%d", tzone = "GMT"), unique = TRUE) 
ACCX$datetime = NULL
storage.mode(ACCX) <- "numeric" # from here on I Use ACCX instead of ACCXDM

#### create data frames from daily XTS
ACCD <- fortify.zoo(ACCX)
names(ACCD)[1] <- "datetime"
HNDD <- fortify.zoo(HNDXDM)
names(HNDD)[1] <- "datetime"
NNCD <- fortify.zoo(NNCXDM)
names(NNCD)[1] <- "datetime"
SBHD <- fortify.zoo(SBHXDM)
names(SBHD)[1] <- "datetime"
SMCD <- fortify.zoo(SMCXDM)
names(SMCD)[1] <- "datetime"
TMRD <- fortify.zoo(TMRXDM)
names(TMRD)[1] <- "datetime"
WEBD <- fortify.zoo(WEBXDM)
names(WEBD)[1] <- "datetime"


#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#                                       FIGURE 9                                           #
#                             low-flow objects and lf stats                                #
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#### subset Q
LFOACC <- subset(ACCX, select = c(Q_m3.s))
LFOHND <- subset(HNDXDM, select = c(Q_m3.s))
LFONNC <- subset(NNCXDM, select = c(Q_m3.s))
LFOSBH <- subset(SBHXDM, select = c(Q_m3.s))
LFOSMC <- subset(SMCXDM, select = c(Q_cfs))
LFOTMR <- subset(TMRXDM, select = c(Q_cfs))
LFOWEB <- subset(WEBXDM, select = c(Q_m3.s))

#### create lf objects
LFOACC1 <- as.lfobj(ACCX$Q_m3.s, hyearstart = 10)
LFOHND1 <- as.lfobj(HNDXDM$Q_m3.s, hyearstart = 10)
LFONNC1 <- as.lfobj(NNCXDM$Q_m3.s, hyearstart = 10)
LFOSBH1 <- as.lfobj(SBHXDM$Q_m3.s, hyearstart = 10)
LFOSMC1 <- as.lfobj(SMCXDM$Q_cfs, hyearstart = 10)
LFOTMR1 <- as.lfobj(TMRXDM$Q_cfs, hyearstart = 10)
LFOWEB1 <- as.lfobj(WEBXDM$Q_m3.s, hyearstart = 10)

#### set flow units (this is just to supress warnings and does not impact scaling etc.)
flowunit(LFOACC1)<-"m^3/s"
flowunit(LFOHND1)<-"m^3/s"
flowunit(LFONNC1)<-"m^3/s"
flowunit(LFOSBH1)<-"m^3/s"
flowunit(LFOSMC1)<-"m^3/s"
flowunit(LFOTMR1)<-"m^3/s"
flowunit(LFOWEB1)<-"l/s"

#### hydrographs
plot.new()
par(mfrow = c(7, 1), mar = c(2, 4, 4, 2))
hydrograph(LFOACC1, amin = TRUE, pch = 20, lwd = 1.4, main = "Hydrograph ACC")
hydrograph(LFOHND1, amin = TRUE, pch = 20, lwd = 1.4, main = "Hydrograph HND")
hydrograph(LFONNC1, amin = TRUE, pch = 20, lwd = 1.4, main = "Hydrograph NNC")
hydrograph(LFOSBH1, amin = TRUE, pch = 20, lwd = 1.4, main = "Hydrograph SBH")
hydrograph(LFOSMC1, amin = TRUE, pch = 20, lwd = 1.4, main = "Hydrograph SMC")
hydrograph(LFOTMR1, amin = TRUE, pch = 20, lwd = 1.4, main = "Hydrograph TMR")
hydrograph(LFOWEB1, amin = TRUE, pch = 20, lwd = 1.4, main = "Hydrograph WEB")

#### BFI
BFI(LFOACC1, year = "any", breakdays = NULL, yearly = FALSE)
BFI(LFOHND1, year = "any", breakdays = NULL, yearly = FALSE)
BFI(LFONNC1, year = "any", breakdays = NULL, yearly = FALSE)
BFI(LFOSBH1, year = "any", breakdays = NULL, yearly = FALSE)
BFI(LFOSMC1, year = "any", breakdays = NULL, yearly = FALSE)
BFI(LFOTMR1, year = "any", breakdays = NULL, yearly = FALSE)
BFI(LFOWEB1, year = "any", breakdays = NULL, yearly = FALSE)

#### mean flow across study period
meanflow(LFOACC1, year = "any", monthly = FALSE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
meanflow(LFOHND1, year = "any", monthly = FALSE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
meanflow(LFONNC1, year = "any", monthly = FALSE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
meanflow(LFOSBH1, year = "any", monthly = FALSE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
meanflow(LFOSMC1, year = "any", monthly = FALSE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
meanflow(LFOTMR1, year = "any", monthly = FALSE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
meanflow(LFOWEB1, year = "any", monthly = FALSE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)

#### final plot (FIGURE 9)
#### baseflow (export as PNG with w = 850 and h = 1100 for intended result)
plot.new()
par(mfrow = c(7, 1), mar = c(3, 3, 2, 1), oma = c(1, 2, 1, 2))
BFACC <- bfplot(LFOACC1, year = "any", col = "steel blue", bfcol = "black", ylog = FALSE)
BFHND <- bfplot(LFOHND1, year = "any", col = "steel blue", bfcol = "black", ylog = FALSE)
BFNNC <- bfplot(LFONNC1, year = "any", col = "steel blue", bfcol = "black", ylog = FALSE)
BFSBH <- bfplot(LFOSBH1, year = "any", col = "steel blue", bfcol = "black", ylog = FALSE)
BFSMC <- bfplot(LFOSMC1, year = "any", col = "steel blue", bfcol = "black", ylog = FALSE)
BFTMR <- bfplot(LFOTMR1, year = "any", col = "steel blue", bfcol = "black", ylog = FALSE)
BFWEB <- bfplot(LFOWEB1, year = "any", col = "steel blue", bfcol = "black", ylog = FALSE)
par(new=TRUE)
mtext("(A) ACC: 2016-2021, Q³/s, BFI = 0.50, Qmean = 0.38", side = 3, line = 73, las = 1, cex = 0.8, adj = 0)
par(new=TRUE)
mtext("(B) HND: 2016-2020, Q³/s, BFI = 0.80, Qmean = 1.01", side = 3, line = 60.8, las = 1, cex = 0.8, adj = 0)
par(new=TRUE)
mtext("(C) NNC: 2017-2018, Q³/s, BFI = 0.74, Qmean = 2.31", side = 3, line = 48.6, las = 1, cex = 0.8, adj = 0)
par(new=TRUE)
mtext("(D) SBH: 2016-2020, Q³/s, BFI = 0.66, Qmean = 0.61", side = 3, line = 36.6, las = 1, cex = 0.8, adj = 0)
par(new=TRUE)
mtext("(E) SMC: 2016-2020, Q cf/s, BFI = 0.87, Qmean = 1.82", side = 3, line = 24.6, las = 1, cex = 0.8, adj = 0)
par(new=TRUE)
mtext("(F) TMR: 2015-2017, Q cf/s, BFI = 0.82, Qmean = 32.39", side = 3, line = 12.5, las = 1, cex = 0.8, adj = 0)
par(new=TRUE)
mtext("(G) WEB: 2014-2015, l/s, BFI = 0.51, Qmean = 5.96", side = 3, line = 0.2, las = 1, cex = 0.8, adj = 0)

#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#                                       FIGURE 10                                          #                        
#                          Seasonal plots for Q, NO3, TP, T2M                              #
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#monthly means
ACCMM <- meanflow(LFOACC1, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
HNDMM <- meanflow(LFOHND1, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
NNCMM <- meanflow(LFONNC1, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
SBHMM <- meanflow(LFOSBH1, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
SMCMM <- meanflow(LFOSMC1, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
TMRMM <- meanflow(LFOTMR1, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
WEBMM <- meanflow(LFOWEB1, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)

#plotting
plot.new()
ACCMMP <- ggplot(ACCMM, aes(month, flow))+
  geom_col(width = 0.5, fill = "steel blue")+
  labs(title = "ACC (2016 - 2021)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("Q m"^3*"/s"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
HNDMMP <- ggplot(HNDMM, aes(month, flow))+
  geom_col(width = 0.5, fill = "steel blue")+
  labs(title = "HND (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("Q m"^3*"/s"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
NNCMMP <- ggplot(NNCMM, aes(month, flow))+
  geom_col(width = 0.5, fill = "steel blue")+
  labs(title = "NNC (2017 - 2018)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("Q m"^3*"/s"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
SBHMMP <- ggplot(SBHMM, aes(month, flow))+
  geom_col(width = 0.5, fill = "steel blue")+
  labs(title = "SBH (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("Q m"^3*"/s"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
SMCMMP <- ggplot(SMCMM, aes(month, flow))+
  geom_col(width = 0.5, fill = "steel blue")+
  labs(title = "SMC (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("Q m"^3*"/s"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
TMRMMP <- ggplot(TMRMM, aes(month, flow))+
  geom_col(width = 0.5, fill = "steel blue")+
  labs(title = "TMR (2015 - 2017)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("Q cf/s"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
WEBMMP <- ggplot(WEBMM, aes(month, flow))+
  geom_col(width = 0.5, fill = "steel blue")+
  labs(title = "WEB (2014 - 2015)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("Q l/s"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))

Qarrange <- grid.arrange(ACCMMP, HNDMMP, NNCMMP, SBHMMP, SMCMMP, TMRMMP, WEBMMP, ncol = 2,
                         layout_matrix = cbind(c(1, 3, 5, 7), c(2, 4, 6, NA)), top = textGrob("(A)", gp=gpar(fontsize=14), x = 0, hjust = 0))

#///////////////////////////////////////////////////////////////////////////////////////////
#                             Seasonal plots for NO3                                       #
#///////////////////////////////////////////////////////////////////////////////////////////

LFOACC1N <- as.lfobj(ACCX$No3_mg.l, hyearstart = 10)
LFOHND1N <- as.lfobj(HNDXDM$No3_mg.l, hyearstart = 10)
LFONNC1N <- as.lfobj(NNCXDM$No3_mg.l, hyearstart = 10)
LFOSBH1N <- as.lfobj(SBHXDM$No3_mg.l, hyearstart = 10)
LFOSMC1N <- as.lfobj(SMCXDM$No3_mg.l, hyearstart = 10)
LFOTMR1N <- as.lfobj(TMRXDM$No3_mg.l, hyearstart = 10)
LFOWEB1N <- as.lfobj(WEBXDM$No3_mg.l, hyearstart = 10)

ACCMMN <- meanflow(LFOACC1N, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
HNDMMN <- meanflow(LFOHND1N, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
NNCMMN <- meanflow(LFONNC1N, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
SBHMMN <- meanflow(LFOSBH1N, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
SMCMMN <- meanflow(LFOSMC1N, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
TMRMMN <- meanflow(LFOTMR1N, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
WEBMMN <- meanflow(LFOWEB1N, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)

ACCMMPN <- ggplot(ACCMMN, aes(month, flow))+
  geom_col(width = 0.5, fill = "#CD9600")+
  labs(title = "ACC (2016 - 2021)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("NO"[3]*"- mg/l"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
HNDMMPN <- ggplot(HNDMMN, aes(month, flow))+
  geom_col(width = 0.5, fill = "#CD9600")+
  labs(title = "HND (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("NO"[3]*"- mg/l"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
NNCMMPN <- ggplot(NNCMMN, aes(month, flow))+
  geom_col(width = 0.5, fill = "#CD9600")+
  labs(title = "NNC (2017 - 2018)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("NO"[3]*"- mg/l"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
SBHMMPN <- ggplot(SBHMMN, aes(month, flow))+
  geom_col(width = 0.5, fill = "#CD9600")+
  labs(title = "SBH (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("NO"[3]*"- mg/l"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
SMCMMPN <- ggplot(SMCMMN, aes(month, flow))+
  geom_col(width = 0.5, fill = "#CD9600")+
  labs(title = "SMC (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("NO"[3]*"- mg/l"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
TMRMMPN <- ggplot(TMRMMN, aes(month, flow))+
  geom_col(width = 0.5, fill = "#CD9600")+
  labs(title = "TMR (2015 - 2017)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("NO"[3]*"- mg/l"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
WEBMMPN <- ggplot(WEBMMN, aes(month, flow))+
  geom_col(width = 0.5, fill = "#CD9600")+
  labs(title = "WEB (2014 - 2015)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("NO"[3]*"- mg/l"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))

Narrange <- grid.arrange(ACCMMPN, HNDMMPN, NNCMMPN, SBHMMPN, SMCMMPN, TMRMMPN, WEBMMPN, ncol = 2,
                         layout_matrix = cbind(c(1, 3, 5, 7), c(2, 4, 6, NA)), top = textGrob("(B)", gp=gpar(fontsize=14), x = 0, hjust = 0))


#///////////////////////////////////////////////////////////////////////////////////////////
#                      Seasonal plots for precipitation*1000 for mm                       #
#///////////////////////////////////////////////////////////////////////////////////////////

LFOACC1TP <- as.lfobj(ACCX$TP*1000, hyearstart = 10)
LFOHND1TP <- as.lfobj(HNDXDM$TP*1000, hyearstart = 10)
LFONNC1TP <- as.lfobj(NNCXDM$TP*1000, hyearstart = 10)
LFOSBH1TP <- as.lfobj(SBHXDM$TP*1000, hyearstart = 10)
LFOSMC1TP <- as.lfobj(SMCXDM$TP*1000, hyearstart = 10)
LFOTMR1TP <- as.lfobj(TMRXDM$TP*1000, hyearstart = 10)
LFOWEB1TP <- as.lfobj(WEBXDM$TP*1000, hyearstart = 10)

ACCMMTP <- meanflow(LFOACC1TP, year = "2021", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
HNDMMTP <- meanflow(LFOHND1TP, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
NNCMMTP <- meanflow(LFONNC1TP, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
SBHMMTP <- meanflow(LFOSBH1TP, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
SMCMMTP <- meanflow(LFOSMC1TP, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
TMRMMTP <- meanflow(LFOTMR1TP, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
WEBMMTP <- meanflow(LFOWEB1TP, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)

View(ACCMMTP)
sum(ACCMMTP)
#### yearly total precipitation (including snow)
sum(ACCMMTP$flow)
sum(HNDMMTP$flow)
sum(NNCMMTP$flow)
sum(SBHMMTP$flow)
sum(SMCMMTP$flow)
sum(TMRMMTP$flow)
sum(WEBMMTP$flow)

ACCMMPTP <- ggplot(ACCMMTP, aes(month, flow))+
  geom_col(width = 0.5, fill = "light blue")+
  labs(title = "ACC (2016 - 2021)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("TP mm"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
HNDMMPTP <- ggplot(HNDMMTP, aes(month, flow))+
  geom_col(width = 0.5, fill = "light blue")+
  labs(title = "HND (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("TP mm"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
NNCMMPTP <- ggplot(NNCMMTP, aes(month, flow))+
  geom_col(width = 0.5, fill = "light blue")+
  labs(title = "NNC (2017 - 2018)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("TP mm"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
SBHMMPTP <- ggplot(SBHMMTP, aes(month, flow))+
  geom_col(width = 0.5, fill = "light blue")+
  labs(title = "SBH (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("TP mm"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
SMCMMPTP <- ggplot(SMCMMTP, aes(month, flow))+
  geom_col(width = 0.5, fill = "light blue")+
  labs(title = "SMC (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("TP mm"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
TMRMMPTP <- ggplot(TMRMMTP, aes(month, flow))+
  geom_col(width = 0.5, fill = "light blue")+
  labs(title = "TMR (2015 - 2017)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("TP mm"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
WEBMMPTP <- ggplot(WEBMMTP, aes(month, flow))+
  geom_col(width = 0.5, fill = "light blue")+
  labs(title = "WEB (2014 - 2015)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("TP mm"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))

TParrange <- grid.arrange(ACCMMPTP, HNDMMPTP, NNCMMPTP, SBHMMPTP, SMCMMPTP, TMRMMPTP, WEBMMPTP, ncol = 2,
                          layout_matrix = cbind(c(1, 3, 5, 7), c(2, 4, 6, NA)), top = textGrob("(C)", gp=gpar(fontsize=14), x = 0, hjust = 0))

#///////////////////////////////////////////////////////////////////////////////////////////
#                             Seasonal plots for temperature                              #
#///////////////////////////////////////////////////////////////////////////////////////////

LFOACC1T2M <- as.lfobj(ACCX$T2M, hyearstart = 10)
LFOHND1T2M <- as.lfobj(HNDXDM$T2M, hyearstart = 10)
LFONNC1T2M <- as.lfobj(NNCXDM$T2M, hyearstart = 10)
LFOSBH1T2M <- as.lfobj(SBHXDM$T2M, hyearstart = 10)
LFOSMC1T2M <- as.lfobj(SMCXDM$T2M, hyearstart = 10)
LFOTMR1T2M <- as.lfobj(TMRXDM$T2M, hyearstart = 10)
LFOWEB1T2M <- as.lfobj(WEBXDM$T2M, hyearstart = 10)

ACCMMT2M <- meanflow(LFOACC1T2M, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
HNDMMT2M <- meanflow(LFOHND1T2M, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
NNCMMT2M <- meanflow(LFONNC1T2M, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
SBHMMT2M <- meanflow(LFOSBH1T2M, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
SMCMMT2M <- meanflow(LFOSMC1T2M, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
TMRMMT2M <- meanflow(LFOTMR1T2M, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
WEBMMT2M <- meanflow(LFOWEB1T2M, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)

summary(ACCMMT2M$flow)
summary(HNDMMT2M$flow)
summary(NNCMMT2M$flow)
summary(SBHMMT2M$flow)
summary(SMCMMT2M$flow)
summary(TMRMMT2M$flow)
summary(WEBMMT2M$flow)

ACCMMPT2M <- ggplot(ACCMMT2M, aes(month, flow))+
  geom_col(width = 0.5, fill = "#F8766D")+
  labs(title = "ACC (2016 - 2021)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("T2M °C"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))+
  geom_hline(yintercept = 14.848)
HNDMMPT2M <- ggplot(HNDMMT2M, aes(month, flow))+
  geom_col(width = 0.5, fill = "#F8766D")+
  labs(title = "HND (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("T2M °C"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))+
  geom_hline(yintercept = 9.8043)
NNCMMPT2M <- ggplot(NNCMMT2M, aes(month, flow))+
  geom_col(width = 0.5, fill = "#F8766D")+
  labs(title = "NNC (2017 - 2018)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("T2M °C"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))+
  geom_hline(yintercept = 14.5395)
SBHMMPT2M <- ggplot(SBHMMT2M, aes(month, flow))+
  geom_col(width = 0.5, fill = "#F8766D")+
  labs(title = "SBH (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("T2M °C"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))+
  geom_hline(yintercept = 9.964)
SMCMMPT2M <- ggplot(SMCMMT2M, aes(month, flow))+
  geom_col(width = 0.5, fill = "#F8766D")+
  labs(title = "SMC (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("T2M °C"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))+
  geom_hline(yintercept = 12.9506)
TMRMMPT2M <- ggplot(TMRMMT2M, aes(month, flow))+
  geom_col(width = 0.5, fill = "#F8766D")+
  labs(title = "TMR (2015 - 2017)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("T2M °C"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))+
  geom_hline(yintercept = 7.95756)
WEBMMPT2M <- ggplot(WEBMMT2M, aes(month, flow))+
  geom_col(width = 0.5, fill = "#F8766D")+
  labs(title = "WEB (2014 - 2015)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("T2M °C"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))+
  geom_hline(yintercept = 9.564)

T2Marrange <- grid.arrange(ACCMMPT2M, HNDMMPT2M, NNCMMPT2M, SBHMMPT2M, SMCMMPT2M, TMRMMPT2M, WEBMMPT2M, ncol = 2,
                           layout_matrix = cbind(c(1, 3, 5, 7), c(2, 4, 6, NA)), top = textGrob("(D)", gp=gpar(fontsize=14), x = 0, hjust = 0))


#### combine plots for Q, NO3, TP and T2M (FIGURE 10)
grid.arrange(Qarrange, Narrange, TParrange, T2Marrange, ncol = 2, top = "")

#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#                     Setting up data for C-Q Patterns and Boxplots                        #
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#### renaming WEB discharge column (m³/s is actually l/s but that did not matter so far)
names(WEB)[names(WEB) == "Q_m3.s"] <- "Q_l.s"

#### setting up Qnormal subsets for all study areas
ACCnormal <- ACC[ACC$Q_m3.s >= "0.1667862" & ACC$Q_m3.s <= "0.5918221",]
HNDnormal <- HND[HND$Q_m3.s >= "0.428" & HND$Q_m3.s <= "1.62",]
NNCnormal <- NNC[NNC$Q_m3.s >= "1.633174" & NNC$Q_m3.s <= "3.015744",]
SBHnormal <- SBH[SBH$Q_m3.s >= "0.184" & SBH$Q_m3.s <= "0.905",]
SMCnormal <- SMC[SMC$Q_cfs >= "1.37" & SMC$Q_cfs <= "2.2",]
TMRnormal <- TMR[TMR$Q_cfs >= "25.1" & TMR$Q_cfs <= "37.625",] 
WEBnormal <- WEB[WEB$Q_l.s >= "0.9" & WEB$Q_l.s <= "10.25",]

#### setting up Qlow subsets
quantile(ACC$Q_m3.s, probs = 0.3) #Q70
quantile(ACC$Q_m3.s, probs = 0.2) #Q80
quantile(ACC$Q_m3.s, probs = 0.10) #Q90
quantile(ACC$Q_m3.s, probs = 0.01) #Q99
quantile(ACC$Q_m3.s, probs = 0.8) #Q20

ACC70 <- ACC[ACC$Q_m3.s <= "0.1667862",]
ACC80 <- ACC[ACC$Q_m3.s <= "0.124311",]
ACC90 <- ACC[ACC$Q_m3.s <= "0.07022578",]
ACC99 <- ACC[ACC$Q_m3.s <= "0.03426338",]
ACC20 <- ACC[ACC$Q_m3.s >= "0.5918221",]

quantile(HND$Q_m3.s, probs = 0.3)
quantile(HND$Q_m3.s, probs = 0.2)
quantile(HND$Q_m3.s, probs = 0.10)
quantile(HND$Q_m3.s, probs = 0.01)
quantile(HND$Q_m3.s, probs = 0.8)

HND70 <- HND[HND$Q_m3.s <= "0.428",]
HND80 <- HND[HND$Q_m3.s <= "0.313",]
HND90 <- HND[HND$Q_m3.s <= "0.214",]
HND99 <- HND[HND$Q_m3.s <= "0.097",]
HND20 <- HND[HND$Q_m3.s >= "1.62",]

quantile(NNC$Q_m3.s, probs = 0.3)
quantile(NNC$Q_m3.s, probs = 0.2)
quantile(NNC$Q_m3.s, probs = 0.10)
quantile(NNC$Q_m3.s, probs = 0.01)
quantile(NNC$Q_m3.s, probs = 0.8)

NNC70 <- NNC[NNC$Q_m3.s <= "1.633174",]
NNC80 <- NNC[NNC$Q_m3.s <= "1.4031",]
NNC90 <- NNC[NNC$Q_m3.s <= "0.9719758",]
NNC99 <- NNC[NNC$Q_m3.s <= "0.5174904",]
NNC20 <- NNC[NNC$Q_m3.s >= "3.015744",]

quantile(SBH$Q_m3.s, probs = 0.3)
quantile(SBH$Q_m3.s, probs = 0.2)
quantile(SBH$Q_m3.s, probs = 0.10)
quantile(SBH$Q_m3.s, probs = 0.01)
quantile(SBH$Q_m3.s, probs = 0.8)

SBH70 <- SBH[SBH$Q_m3.s <= "0.184",]
SBH80 <- SBH[SBH$Q_m3.s <= "0.135",]
SBH90 <- SBH[SBH$Q_m3.s <= "0.11",]
SBH99 <- SBH[SBH$Q_m3.s <= "0.079",]
SBH20 <- SBH[SBH$Q_m3.s >= "0.905",]

quantile(SMC$Q_cfs, probs = 0.3)
quantile(SMC$Q_cfs, probs = 0.2)
quantile(SMC$Q_cfs, probs = 0.10)
quantile(SMC$Q_cfs, probs = 0.01)
quantile(SMC$Q_cfs, probs = 0.8)

SMC70 <- SMC[SMC$Q_cfs <= "1.37",]
SMC80 <- SMC[SMC$Q_cfs <= "1.32",]
SMC90 <- SMC[SMC$Q_cfs <= "1.25",]
SMC99 <- SMC[SMC$Q_cfs <= "1.18",]
SMC20 <- SMC[SMC$Q_cfs >= "2.2",]

quantile(TMR$Q_cfs, probs = 0.3)
quantile(TMR$Q_cfs, probs = 0.2)
quantile(TMR$Q_cfs, probs = 0.10)
quantile(TMR$Q_cfs, probs = 0.01)
quantile(TMR$Q_cfs, probs = 0.8)

TMR70 <- TMR[TMR$Q_cfs <= "25.1",]
TMR80 <- TMR[TMR$Q_cfs <= "24.1443",]
TMR90 <- TMR[TMR$Q_cfs <= "22.825",]
TMR99 <- TMR[TMR$Q_cfs <= "18.8",]
TMR20 <- TMR[TMR$Q_cfs >= "37.625",]

quantile(WEB$Q_l.s, probs = 0.3)
quantile(WEB$Q_l.s, probs = 0.2)
quantile(WEB$Q_l.s, probs = 0.10)
quantile(WEB$Q_l.s, probs = 0.01)
quantile(WEB$Q_l.s, probs = 0.8)

WEB70 <- WEB[WEB$Q_l.s <= "0.9",]
WEB80 <- WEB[WEB$Q_l.s <= "0.6",]
WEB90 <- WEB[WEB$Q_l.s <= "0.2",]
WEB99 <- WEB[WEB$Q_l.s <= "0",]
WEB20 <- WEB[WEB$Q_l.s >= "10.25",]

#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#                                        FIGURE 11                                         #
#                                      C-Q patterns                                        #
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
ACCCQ <- ggplot(data = ACC, aes(log(Q_m3.s), log(No3_mg.l)))+
  geom_point(alpha = 0.6, color = "steelblue", shape = 20, size = 1.2)+
  labs(y = (expression((log)~NO["3-"]*" mg/l")))+
  xlab(bquote((log)~Q_m^3*s))+
  labs(title = "(A)", cex = 0.8)+
  theme(plot.title = element_text(size = 9, vjust = 0.1))+
  theme(axis.text = element_text(size = 7))+
  theme(axis.title = element_text(size = 9))+
  geom_smooth(method = "gam", col = "black", size = 1)+ # gam smoother is chosen here for its faster calculation compared to loess
  theme_bw()
#ACCQ1 <- ggMarginal(ACCQ , type="boxplot", size = 18) #(adds marginal boxplots on the sides if value distribution is of interest (not plotted in original thesis)))

HNDCQ <- ggplot(data = HND, aes(log(Q_m3.s), log(No3_mg.l)))+
  geom_point(alpha = 0.8, colour = "steelblue", shape = 20, size = 1.2)+
  labs(y = (expression((log)~NO["3-"]*" mg/l")))+
  xlab(bquote((log)~Q_m^3*s))+
  labs(title = "(B)")+
  theme(plot.title = element_text(size = 9, vjust = 0.1))+
  theme(axis.text = element_text(size = 7))+
  theme(axis.title = element_text(size = 9))+
  geom_smooth(method = "gam", col = "black", size = 1)+
  theme_bw()
#HNDCQ1 <- ggMarginal(HNDCQ , type="boxplot", size = 18)

WEBCQ <- ggplot(data = WEB, aes(log(Q_l.s), log(No3_mg.l)))+
  geom_point(alpha = 0.8, colour = "steelblue", shape = 20, size = 1.2)+
  labs(y = (expression((log)~NO["3-"]*" mg/l")))+
  xlab(bquote((log)~Q_l/s))+
  labs(title = "(G)")+
  theme(plot.title = element_text(size = 9, vjust = 0.1))+
  theme(axis.text = element_text(size = 7))+
  theme(axis.title = element_text(size = 9))+
  geom_smooth(method = "gam", col = "black", size = 1)+
  theme_bw()
#WEBCQ1 <- ggMarginal(WEBCQ , type="boxplot", size = 18)


SMCCQ <- ggplot(data = SMC, aes(log(Q_cfs), log(No3_mg.l)))+
  geom_point(alpha = 0.8, colour = "steelblue", shape = 20, size = 1.2)+
  labs(y = (expression((log)~NO["3-"]*" mg/l")))+
  xlab(bquote((log)~Q_cf/s))+
  labs(title = "(E)")+
  theme(plot.title = element_text(size = 9, vjust = 0.1))+
  theme(axis.text = element_text(size = 7))+
  theme(axis.title = element_text(size = 9))+
  geom_smooth(method = "gam", col = "black", size = 1)+
  theme_bw()
#SMCCQ1 <- ggMarginal(SMCCQ , type="boxplot", size = 18)

NNCCQ <- ggplot(data = NNC, aes(log(Q_m3.s), log(No3_mg.l)))+
  geom_point(alpha = 0.8, colour = "steelblue", shape = 20, size = 1.2)+
  labs(y = (expression((log)~NO["3-"]*" mg/l")))+
  xlab(bquote((log)~Q_m^3*s))+
  labs(title = "(C)")+
  theme(plot.title = element_text(size = 9, vjust = 0.1))+
  theme(axis.text = element_text(size = 7))+
  theme(axis.title = element_text(size = 9))+
  geom_smooth(method = "gam", col = "black", size = 1)+
  theme_bw()
#NNCCQ1 <- ggMarginal(NNCCQ , type="boxplot", size = 18)

TMRCQ <- ggplot(data = TMR, aes(log(Q_cfs), log(No3_mg.l)))+
  geom_point(alpha = 0.8, colour = "steelblue", shape = 20, size = 1.2)+
  labs(y = (expression((log)~NO["3-"]*" mg/l")))+
  xlab(bquote((log)~Q_cf/s))+
  labs(title = "(F)")+
  theme(plot.title = element_text(size = 9))+
  theme(axis.text = element_text(size = 7))+
  theme(axis.title = element_text(size = 9))+
  geom_smooth(method = "gam", col = "black", size = 1)+
  theme_bw()
#TMRCQ1 <- ggMarginal(TMRCQ , type="boxplot", size = 18)

SBHCQ <- ggplot(data = SBH, aes(log(Q_m3.s), log(No3_mg.l)))+
  geom_point(alpha = 0.8, colour = "steelblue", shape = 20, size = 1.2)+
  labs(y = (expression((log)~NO["3-"]*" mg/l")))+
  xlab(bquote((log)~Q_m^3*s))+
  labs(title = "(D)")+
  theme(plot.title = element_text(size = 9))+
  theme(axis.text = element_text(size = 7))+
  theme(axis.title = element_text(size = 9))+
  geom_smooth(method = "gam", col = "black", size = 1)+
  theme_bw()
#SBHCQ1 <- ggMarginal(SBHCQ , type="boxplot", size = 18)

#### CQ plots Q80
ACCQ80 <- ggplot(data = ACC80, aes(log(Q_m3.s), log(No3_mg.l)))+
  geom_point(alpha = 0.6, colour = "steelblue", shape = 20, size = 1.2)+
  labs(y = (expression((log)~NO["3-"]*" mg/l")))+
  xlab(bquote((log)~Q_m^3*s))+
  labs(title = "(A1)", cex = 0.8)+
  theme(plot.title = element_text(size = 9, vjust = 0.1))+
  theme(axis.text = element_text(size = 7))+
  theme(axis.title = element_text(size = 9))+
  geom_smooth(method = "loess", col = "black", size = 1, n = 1000)+ #loess smoother chosen here for it's higher accuracy (smaller data set)
  theme_bw()
#ACCQ81 <- ggMarginal(ACCQ80 , type="boxplot", size = 18)

HNDCQ80 <- ggplot(data = HND80, aes(log(Q_m3.s), log(No3_mg.l)))+
  geom_point(alpha = 0.8, colour = "steelblue", shape = 20, size = 1.2)+
  labs(y = (expression((log)~NO["3-"]*" mg/l")))+
  xlab(bquote((log)~Q_m^3*s))+
  labs(title = "(B1)")+
  theme(plot.title = element_text(size = 9, vjust = 0.1))+
  theme(axis.text = element_text(size = 7))+
  theme(axis.title = element_text(size = 9))+
  geom_smooth(method = "loess", col = "black", size = 1, n=1000)+
  theme_bw()
#HNDCQ81 <- ggMarginal(HNDCQ80 , type="boxplot", size = 18)

WEBCQ80 <- ggplot(data = WEB80, aes(log(Q_l.s), log(No3_mg.l)))+
  geom_point(alpha = 0.8, colour = "steelblue", shape = 20, size = 1.2)+
  labs(y = (expression((log)~NO["3-"]*" mg/l")))+
  xlab(bquote((log)~Q_l/s))+
  labs(title = "(G1)")+
  theme(plot.title = element_text(size = 9, vjust = 0.1))+
  theme(axis.text = element_text(size = 7))+
  theme(axis.title = element_text(size = 9))+
  geom_smooth(method = "loess", col = "black", size = 1, n = 1000)+
  theme_bw()
#WEBCQ81 <- ggMarginal(WEBCQ80 , type="boxplot", size = 18)

SMCCQ80 <- ggplot(data = SMC80, aes(log(Q_cfs), log(No3_mg.l)))+
  geom_point(alpha = 0.8, colour = "steelblue", shape = 20, size = 1.2)+
  labs(y = (expression((log)~NO["3-"]*" mg/l")))+
  xlab(bquote((log)~Q_cf/s))+
  labs(title = "(E1)")+
  theme(plot.title = element_text(size = 9, vjust = 0.1))+
  theme(axis.text = element_text(size = 7))+
  theme(axis.title = element_text(size = 9))+
  geom_smooth(method = "loess", col = "black", size = 1, n = 1000)+
  theme_bw()
#SMCCQ81 <- ggMarginal(SMCCQ80 , type="boxplot", size = 18)

NNCCQ80 <- ggplot(data = NNC80, aes(log(Q_m3.s), log(No3_mg.l)))+
  geom_point(alpha = 0.8, colour = "steelblue", shape = 20, size = 1.2)+
  labs(y = (expression((log)~NO["3-"]*" mg/l")))+
  xlab(bquote((log)~Q_m^3*s))+
  labs(title = "(C1)")+
  theme(plot.title = element_text(size = 9, vjust = 0.1))+
  theme(axis.text = element_text(size = 7))+
  theme(axis.title = element_text(size = 9))+
  geom_smooth(method = "loess", col = "black", size = 1, n=1000)+
  theme_bw()
#NNCCQ81 <- ggMarginal(NNCCQ80 , type="boxplot", size = 18)

TMRCQ80 <- ggplot(data = TMR80, aes(log(Q_cfs), log(No3_mg.l)))+
  geom_point(alpha = 0.8, colour = "steelblue", shape = 20, size = 1.2)+
  labs(y = (expression((log)~NO["3-"]*" mg/l")))+
  xlab(bquote((log)~Q_cf/s))+
  labs(title = "(F1)")+
  theme(plot.title = element_text(size = 9))+
  theme(axis.text = element_text(size = 7))+
  theme(axis.title = element_text(size = 9))+
  geom_smooth(method = "loess", col = "black", size = 1, n=1000)+
  theme_bw()
#TMRCQ81 <- ggMarginal(TMRCQ80 , type="boxplot", size = 18)

SBHCQ80 <- ggplot(data = SBH80, aes(log(Q_m3.s), log(No3_mg.l)))+
  geom_point(alpha = 0.8, colour = "steelblue", shape = 20, size = 1.2)+
  labs(y = (expression((log)~NO["3-"]*" mg/l")))+
  xlab(bquote((log)~Q_m^3*s))+
  labs(title = "(D1)")+
  theme(plot.title = element_text(size = 9))+
  theme(axis.text = element_text(size = 7))+
  theme(axis.title = element_text(size = 9))+
  geom_smooth(method = "loess", col = "black", size = 1, n=1000)+
  theme_bw()
#SBHCQ81 <- ggMarginal(SBHCQ80 , type="boxplot", size = 18)

#### combining all C-Q plots (this may take several minutes) (FIGURE 11)
grid.arrange(ACCCQ, ACCQ80, HNDCQ, HNDCQ80, NNCCQ, NNCCQ80, SBHCQ, SBHCQ80, SMCCQ, SMCCQ80, TMRCQ, TMRCQ80, WEBCQ, WEBCQ80, ncol = 4, nrow = 4)

#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#                                        FIGURE 12                                         #
#                                        Boxplots                                          #
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#### Boxplots for ACC study area with dotplots as visual aid
ACCBxplts <- ggplot(data = NULL, aes(x = NULL, y = No3_mg.l))+
  theme_bw()+
  scale_color_manual(name = "", 
                     values = c("Q70" = "#003300",
                                "Q80" = "#336633",
                                "Q90"   = "#339933",
                                "Q99" = "000033",
                                "Qnorm" = "#3399FF")) +
  geom_boxplot(data = ACC70, aes(colour = "Q70", alpha(1)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = ACC70, aes(colour = "Q70", alpha(1)), binaxis = "y", dotsize = .2, binwidth = 1/120, stackdir = "center", alpha = 0.005, method = "dotdensity")+
  geom_boxplot(data = ACC80, aes(colour = "Q80", alpha(5)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = ACC80, aes(colour = "Q80", alpha(5)), binaxis = "y", dotsize = .2, binwidth = 1/120, stackdir = "center", alpha = 0.008, method = "dotdensity")+
  geom_boxplot(data = ACC90, aes(colour = "Q90", alpha(6)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = ACC90, aes(colour = "Q90", alpha(6)), binaxis = "y", dotsize = .2, binwidth = 1/110, stackdir = "center", alpha = 0.008, method = "dotdensity")+
  geom_boxplot(data = ACC99, aes(colour = "Q99", alpha(10)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = ACC99, aes(colour = "Q99", alpha(10)), binaxis = "y", dotsize = .2, binwidth = 1/100, stackdir = "center", alpha = 0.03, method = "dotdensity")+
  geom_boxplot(data = ACCnormal, aes(colour = "Qnorm", alpha(7)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = ACCnormal, aes(colour = "Qnorm", alpha(7)), binaxis = "y", dotsize = .2, binwidth = 1/180, stackdir = "center", alpha = 0.005, method = "dotdensity")+
  labs(title = "(A)")+
  labs(y = (expression(NO["3-"]*" mg/l")))+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")+
  xlab("")

#### Boxplots for HND study area with dotplots as visual aid
HNDBxplts <- ggplot(data = NULL, aes(x = NULL, y = No3_mg.l))+
  theme_bw()+
  scale_color_manual(name = "", 
                     values = c("Q70" = "#003300",
                                "Q80" = "#336633",
                                "Q90"   = "#339933",
                                "Q99" = "000033",
                                "Qnorm" = "#3399FF")) +
  geom_boxplot(data = HND70, aes(colour = "Q70", alpha(1)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = HND70, aes(colour = "Q70", alpha(1)), binaxis = "y", dotsize = .2, binwidth = 1/80, stackdir = "center", alpha = 0.005, method = "dotdensity")+
  geom_boxplot(data = HND80, aes(colour = "Q80", alpha(5)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = HND80, aes(colour = "Q80", alpha(5)), binaxis = "y", dotsize = .2, binwidth = 1/70, stackdir = "center", alpha = 0.008, method = "dotdensity")+
  geom_boxplot(data = HND90, aes(colour = "Q90", alpha(6)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = HND90, aes(colour = "Q90", alpha(6)), binaxis = "y", dotsize = .2, binwidth = 1/40, stackdir = "center", alpha = 0.01, method = "dotdensity")+
  geom_boxplot(data = HND99, aes(colour = "Q99", alpha(10)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = HND99, aes(colour = "Q99", alpha(10)), binaxis = "y", dotsize = .2, binwidth = 1/20, stackdir = "center", alpha = 0.06, method = "dotdensity")+
  geom_boxplot(data = HNDnormal, aes(colour = "Qnorm", alpha(7)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = HNDnormal, aes(colour = "Qnorm", alpha(7)), binaxis = "y", dotsize = .2, binwidth = 1/100, stackdir = "center", alpha = 0.005, method = "dotdensity")+
  labs(title = "(B)")+
  labs(y = (expression(NO["3-"]*" mg/l")))+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank())+
  xlab("")

#### Boxplots for NNC study area with dotplots as visual aid
NNCBxplts <- ggplot(data = NULL, aes(x = NULL, y = No3_mg.l))+
  theme_bw()+
  scale_color_manual(name = "", 
                     values = c("Q70" = "#003300",
                                "Q80" = "#336633",
                                "Q90"   = "#339933",
                                "Q99" = "000033",
                                "Qnorm" = "#3399FF")) +
  geom_boxplot(data = NNC70, aes(colour = "Q70", alpha(1)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = NNC70, aes(colour = "Q70", alpha(1)), binaxis = "y", dotsize = .2, binwidth = 1/200, stackdir = "center", alpha = 0.01, method = "dotdensity")+
  geom_boxplot(data = NNC80, aes(colour = "Q80", alpha(5)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = NNC80, aes(colour = "Q80", alpha(5)), binaxis = "y", dotsize = .2, binwidth = 1/200, stackdir = "center", alpha = 0.01, method = "dotdensity")+
  geom_boxplot(data = NNC90, aes(colour = "Q90", alpha(6)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = NNC90, aes(colour = "Q90", alpha(6)), binaxis = "y", dotsize = .2, binwidth = 1/200, stackdir = "center", alpha = 0.01, method = "dotdensity")+
  geom_boxplot(data = NNC99, aes(colour = "Q99", alpha(10)), 
               outlier.alpha = 0.2, lwd = 1, notch = FALSE, notchwidth = 0.1)+
  geom_dotplot(data = NNC99, aes(colour = "Q99", alpha(10)), binaxis = "y", dotsize = .2, binwidth = 1/200, stackdir = "center", alpha = 0.05, method = "dotdensity")+
  geom_boxplot(data = NNCnormal, aes(colour = "Qnorm", alpha(7)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = NNCnormal, aes(colour = "Qnorm", alpha(7)), binaxis = "y", dotsize = .2, binwidth = 1/300, stackdir = "center", alpha = 0.005, method = "dotdensity")+
  labs(title = "(C)")+
  labs(y = (expression(NO["3-"]*" mg/l")))+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")+
  xlab("")

#### Boxplots for SBH study area with dotplots as visual aid
SBHBxplts <- ggplot(data = NULL, aes(x = NULL, y = No3_mg.l))+
  theme_bw()+
  scale_color_manual(name = "", 
                     values = c("Q70" = "#003300",
                                "Q80" = "#336633",
                                "Q90"   = "#339933",
                                "Q99" = "000033",
                                "Qnorm" = "#3399FF")) +
  geom_boxplot(data = SBH70, aes(colour = "Q70", alpha(1)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = SBH70, aes(colour = "Q70", alpha(1)), binaxis = "y", dotsize = .2, binwidth = 1/100, stackdir = "center", alpha = 0.003, method = "dotdensity")+
  geom_boxplot(data = SBH80, aes(colour = "Q80", alpha(5)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = SBH80, aes(colour = "Q80", alpha(5)), binaxis = "y", dotsize = .2, binwidth = 1/100, stackdir = "center", alpha = 0.003, method = "dotdensity")+
  geom_boxplot(data = SBH90, aes(colour = "Q90", alpha(6)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = SBH90, aes(colour = "Q90", alpha(6)), binaxis = "y", dotsize = .2, binwidth = 1/80, stackdir = "center", alpha = 0.003, method = "dotdensity")+
  geom_boxplot(data = SBH99, aes(colour = "Q99", alpha(10)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = SBH99, aes(colour = "Q99", alpha(10)), binaxis = "y", dotsize = .2, binwidth = 1/60, stackdir = "center", alpha = 0.01, method = "dotdensity")+
  geom_boxplot(data = SBHnormal, aes(colour = "Qnorm", alpha(7)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = SBHnormal, aes(colour = "Qnorm", alpha(7)), binaxis = "y", dotsize = .2, binwidth = 1/100, stackdir = "center", alpha = 0.003, method = "dotdensity")+
  labs(title = "(D)")+
  labs(y = (expression(NO["3-"]*" mg/l")))+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank())+
  xlab("")

#### Boxplots for SMC study area with dotplots as visual aid
SMCBxplts <- ggplot(data = NULL, aes(x = NULL, y = No3_mg.l))+
  theme_bw()+
  scale_color_manual(name = "", 
                     values = c("Q70" = "#003300",
                                "Q80" = "#336633",
                                "Q90"   = "#339933",
                                "Q99" = "000033",
                                "Qnorm" = "#3399FF")) +
  geom_boxplot(data = SMC70, aes(colour = "Q70", alpha(1)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = SMC70, aes(colour = "Q70", alpha(1)), binaxis = "y", dotsize = .2, binwidth = 1/100, stackdir = "center", alpha = 0.005, method = "dotdensity")+
  geom_boxplot(data = SMC80, aes(colour = "Q80", alpha(5)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = SMC80, aes(colour = "Q80", alpha(5)), binaxis = "y", dotsize = .2, binwidth = 1/100, stackdir = "center", alpha = 0.005, method = "dotdensity")+
  geom_boxplot(data = SMC90, aes(colour = "Q90", alpha(6)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = SMC90, aes(colour = "Q90", alpha(6)), binaxis = "y", dotsize = .2, binwidth = 1/50, stackdir = "center", alpha = 0.01, method = "dotdensity")+
  geom_boxplot(data = SMC99, aes(colour = "Q99", alpha(10)), 
               outlier.alpha = 0.2, lwd = 1, notch = FALSE, notchwidth = 0.1)+
  geom_dotplot(data = SMC99, aes(colour = "Q99", alpha(10)), binaxis = "y", dotsize = .2, binwidth = 1/20, stackdir = "center", alpha = 0.1, method = "dotdensity")+
  geom_boxplot(data = SMCnormal, aes(colour = "Qnorm", alpha(7)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = SMCnormal, aes(colour = "Qnorm", alpha(7)), binaxis = "y", dotsize = .2, binwidth = 1/180, stackdir = "center", alpha = 0.005, method = "dotdensity")+
  labs(title = "(E)")+
  labs(y = (expression(NO["3-"]*" mg/l")))+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")+
  xlab("")

#### Boxplots for TMR study area with dotplots as visual aid
TMRBxplts <- ggplot(data = NULL, aes(x = NULL, y = No3_mg.l))+
  theme_bw()+
  scale_color_manual(name = "", 
                     values = c("Q70" = "#003300",
                                "Q80" = "#336633",
                                "Q90"   = "#339933",
                                "Q99" = "000033",
                                "Qnorm" = "#3399FF")) +
  geom_boxplot(data = TMR70, aes(colour = "Q70", alpha(1)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = TMR70, aes(colour = "Q70", alpha(1)), binaxis = "y", dotsize = .2, binwidth = 1/110, stackdir = "center", alpha = 0.005, method = "dotdensity")+
  geom_boxplot(data = TMR80, aes(colour = "Q80", alpha(5)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = TMR80, aes(colour = "Q80", alpha(5)), binaxis = "y", dotsize = .2, binwidth = 1/100, stackdir = "center", alpha = 0.005, method = "dotdensity")+
  geom_boxplot(data = TMR90, aes(colour = "Q90", alpha(6)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = TMR90, aes(colour = "Q90", alpha(6)), binaxis = "y", dotsize = .2, binwidth = 1/60, stackdir = "center", alpha = 0.02, method = "dotdensity")+
  geom_boxplot(data = TMR99, aes(colour = "Q99", alpha(10)), 
               outlier.alpha = 0.2, lwd = 1, notch = FALSE, notchwidth = 0.1)+
  geom_dotplot(data = TMR99, aes(colour = "Q99", alpha(10)), binaxis = "y", dotsize = .2, binwidth = 1/20, stackdir = "center", alpha = 0.05, method = "dotdensity")+
  geom_boxplot(data = TMRnormal, aes(colour = "Qnorm", alpha(7)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = TMRnormal, aes(colour = "Qnorm", alpha(7)), binaxis = "y", dotsize = .2, binwidth = 1/120, stackdir = "center", alpha = 0.005, method = "dotdensity")+
  labs(title = "(F)")+
  labs(y = (expression(NO["3-"]*" mg/l")))+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank())+
  xlab("")

#### Boxplots for WEB study area with dotplots as visual aid
WEBBxplts <- ggplot(data = NULL, aes(x = NULL, y = No3_mg.l))+
  theme_bw()+
  scale_color_manual(name = "", 
                     values = c("Q70" = "#003300",
                                "Q80" = "#336633",
                                "Q90"   = "#339933",
                                "Q99" = "000033",
                                "Qnorm" = "#3399FF")) +
  geom_boxplot(data = WEB70, aes(colour = "Q70", alpha(1)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = WEB70, aes(colour = "Q70", alpha(1)), binaxis = "y", dotsize = .2, binwidth = 1/140, stackdir = "center", alpha = 0.01, method = "dotdensity")+
  geom_boxplot(data = WEB80, aes(colour = "Q80", alpha(5)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = WEB80, aes(colour = "Q80", alpha(5)), binaxis = "y", dotsize = .2, binwidth = 1/120, stackdir = "center", alpha = 0.01, method = "dotdensity")+
  geom_boxplot(data = WEB90, aes(colour = "Q90", alpha(6)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = WEB90, aes(colour = "Q90", alpha(6)), binaxis = "y", dotsize = .2, binwidth = 1/100, stackdir = "center", alpha = 0.02, method = "dotdensity")+
  geom_boxplot(data = WEB99, aes(colour = "Q99", alpha(10)), 
               outlier.alpha = 0.2, lwd = 1, notch = FALSE, notchwidth = 0.1)+
  geom_dotplot(data = WEB99, aes(colour = "Q99", alpha(10)), binaxis = "y", dotsize = .2, binwidth = 1/100, stackdir = "center", alpha = 0.03, method = "dotdensity")+
  geom_boxplot(data = WEBnormal, aes(colour = "Qnorm", alpha(7)), 
               outlier.alpha = 0.2, lwd = 1, notch = TRUE, notchwidth = 0.1)+
  geom_dotplot(data = WEBnormal, aes(colour = "Qnorm", alpha(7)), binaxis = "y", dotsize = .2, binwidth = 1/200, stackdir = "center", alpha = 0.005, method = "dotdensity")+
  labs(title = "(G)")+
  labs(y = (expression(NO["3-"]*" mg/l")))+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  xlab("")

#### combining plots (this may take several minutes) (FIGURE 12)
grid.arrange(ACCBxplts, HNDBxplts, NNCBxplts, SBHBxplts, SMCBxplts, TMRBxplts, WEBBxplts, ncol = 2,
             layout_matrix = cbind(c(1, 3, 5, 7), c(2, 4, 6, NA)))



#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#                                        FIGURE 13                                         #
#                                          FDCs                                            #
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#### FDCs with gustard curves (export as PNG with w = 850 and h = 1000 for intended result)
plot.new()
par(mfrow = c(4, 2), 
    title(main = NULL, outer = TRUE, cex.main = 1.2, line = 0), 
    mar = c(2, 4.5, 5, 1), 
    mgp = c(3.6, 1, 0), 
    mai = c(0.3, 0.6, 0.3, 0.1), 
    oma = c(1, 1, 3, 1))
ACCfdcGust <- FlowScreen::FDC(ACC$Q_m3.s, title = NULL, gust = TRUE, normal = FALSE)
HNDfdcGust <- FlowScreen::FDC(HND$Q_m3.s, title = NULL, gust = TRUE, normal = FALSE)
NNCfdcGust <- FlowScreen::FDC(NNC$Q_m3.s, title = NULL, gust = TRUE, normal = FALSE)
SBHfdcGust <- FlowScreen::FDC(SBH$Q_m3.s, title = NULL, gust = TRUE, normal = FALSE)
SMCfdcGust <- FlowScreen::FDC(SMC$Q_cfs, title = NULL, gust = TRUE, normal = FALSE)
TMRfdcGust <- FlowScreen::FDC(TMR$Q_cfs, title = NULL, gust = TRUE, normal = FALSE)
WEBfdcGust <- FlowScreen::FDC(WEB$Q_l.s, title = NULL, gust = TRUE, normal = FALSE)
par(new=TRUE)
mtext("(A)", side = 3, line = 56.5, las = 1, cex = 0.8, adj = 0)
par(new=TRUE)
mtext("(B)", side = 3, line = 56.5, las = 1, cex = 0.8, adj = 1.25)
par(new=TRUE)
mtext("(C)", side = 3, line = 37.6, las = 1, cex = 0.8, adj = 0)
par(new=TRUE)
mtext("(D)", side = 3, line = 37.6, las = 1, cex = 0.8, adj = 1.25)
par(new=TRUE)
mtext("(E)", side = 3, line = 19, las = 1, cex = 0.8, adj = 0)
par(new=TRUE)
mtext("(F)", side = 3, line = 19, las = 1, cex = 0.8, adj = 1.25)
par(new=TRUE)
mtext("(G)", side = 3, line = 0.1, las = 1, cex = 0.8, adj = 0)
par(new=TRUE)
mtext("Proportion of time flow is equalled or exceeded", side = 3, line = 39, las = 1, cex = 0.7, adj = 0.5)
par(new=TRUE)
mtext("Proportion of time flow is equalled or exceeded", side = 3, line = 39, las = 1, cex = 0.7, adj = 4)
par(new=TRUE)
mtext("Proportion of time flow is equalled or exceeded", side = 3, line = 20.3, las = 1, cex =0.7, adj = 0.5)
par(new=TRUE)
mtext("Proportion of time flow is equalled or exceeded", side = 3, line = 20.3, las = 1, cex = 0.7, adj = 4)
par(new=TRUE)
mtext("Proportion of time flow is equalled or exceeded", side = 3, line = 1.3, las = 1, cex = 0.7, adj = 0.5)
par(new=TRUE)
mtext("Proportion of time flow is equalled or exceeded", side = 3, line = 1.3, las = 1, cex = 0.7, adj = 4)
par(new=TRUE)
mtext("Proportion of time flow is equalled or exceeded", side = 3, line = -17.3, las = 1, cex = 0.7, adj = 0.5)



#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#                                 Random Forest Models                                     #
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
###                          Random Forest data frame setup                              ###
#///////////////////////////////////////////////////////////////////////////////////////////
#### create 2-hour aggregates for Qnorm (not enough memory for models at full resolution)
ACCXnormal <- xts(ACCnormal, order.by = as.POSIXct(ACCnormal$datetime, format = "%y-%m-%d %H:%M:%S", tzone = "GMT"), unique = TRUE)
HNDXnormal <- xts(HNDnormal, order.by = as.POSIXct(HNDnormal$datetime, format = "%y-%m-%d %H:%M:%S", tzone = "GMT"), unique = TRUE)
NNCXnormal <- xts(NNCnormal, order.by = as.POSIXct(NNCnormal$datetime, format = "%y-%m-%d %H:%M:%S", tzone = "GMT"), unique = TRUE)
SBHXnormal <- xts(SBHnormal, order.by = as.POSIXct(SBHnormal$datetime, format = "%y-%m-%d %H:%M:%S", tzone = "GMT"), unique = TRUE)
SMCXnormal <- xts(SMCnormal, order.by = as.POSIXct(SMCnormal$datetime, format = "%y-%m-%d %H:%M:%S", tzone = "GMT"), unique = TRUE)
TMRXnormal <- xts(TMRnormal, order.by = as.POSIXct(TMRnormal$datetime, format = "%y-%m-%d %H:%M:%S", tzone = "GMT"), unique = TRUE)
WEBXnormal <- xts(WEBnormal, order.by = as.POSIXct(WEBnormal$datetime, format = "%y-%m-%d %H:%M:%S", tzone = "GMT"), unique = TRUE)

ACCXnormal$datetime = NULL
HNDXnormal$datetime = NULL
NNCXnormal$datetime = NULL
SBHXnormal$datetime = NULL
SMCXnormal$datetime = NULL
TMRXnormal$datetime = NULL
WEBXnormal$datetime = NULL

ACCXEPnormal <- endpoints(ACCXnormal, "hours", 2)
HNDXEPnormal <- endpoints(HNDXnormal, "hours", 2)
NNCXEPnormal <- endpoints(NNCXnormal, "hours", 2)
SBHXEPnormal <- endpoints(SBHXnormal, "hours", 2)
SMCXEPnormal <- endpoints(SMCXnormal, "hours", 2)
TMRXEPnormal <- endpoints(TMRXnormal, "hours", 2)
WEBXEPnormal <- endpoints(WEBXnormal, "hours", 2)

ACCXDMnormal <- period.apply(ACCXnormal, INDEX = ACCXEPnormal, FUN = mean)
HNDXDMnormal <- period.apply(HNDXnormal, INDEX = HNDXEPnormal, FUN = mean)
NNCXDMnormal <- period.apply(NNCXnormal, INDEX = NNCXEPnormal, FUN = mean)
SBHXDMnormal <- period.apply(SBHXnormal, INDEX = SBHXEPnormal, FUN = mean)
SMCXDMnormal <- period.apply(SMCXnormal, INDEX = SMCXEPnormal, FUN = mean)
TMRXDMnormal <- period.apply(TMRXnormal, INDEX = TMRXEPnormal, FUN = mean)
WEBXDMnormal <- period.apply(WEBXnormal, INDEX = WEBXEPnormal, FUN = mean)

ACCDnormal <- fortify.zoo(ACCXDMnormal)
names(ACCDnormal)[1] <- "datetime"
head(ACCDnormal)
HNDDnormal <- fortify.zoo(HNDXDMnormal)
names(HNDDnormal)[1] <- "datetime"
head(HNDDnormal)
NNCDnormal <- fortify.zoo(NNCXDMnormal)
names(NNCDnormal)[1] <- "datetime"
head(NNCDnormal)
SBHDnormal <- fortify.zoo(SBHXDMnormal)
names(SBHDnormal)[1] <- "datetime"
head(SBHDnormal)
SMCDnormal <- fortify.zoo(SMCXDMnormal)
names(SMCDnormal)[1] <- "datetime"
head(SMCDnormal)
TMRDnormal <- fortify.zoo(TMRXDMnormal)
names(TMRDnormal)[1] <- "datetime"
head(TMRDnormal)
WEBDnormal <- fortify.zoo(WEBXDMnormal)
names(WEBDnormal)[1] <- "datetime"
head(WEBDnormal)


#///////////////////////////////////////////////////////////////////////////////////////////////////
###                  RF data (we use Qnorm (2-hour aggregates), Q80, and Q20                     ###
#///////////////////////////////////////////////////////////////////////////////////////////////////
RFModel_ACC <- subset(ACCDnormal, select = c(Q_m3.s,No3_mg.l,T2M,ROSS,VSWL1,VSWL2,VSWL3,VSWL4,SNSR))
RFModel_ACC80 <- subset(ACC80, select = c(Q_m3.s,No3_mg.l,T2M,ROSS,VSWL1,VSWL2,VSWL3,VSWL4,SNSR)) 
RFModel_ACC20 <- subset(ACC20, select = c(Q_m3.s,No3_mg.l,T2M,ROSS,VSWL1,VSWL2,VSWL3,VSWL4,SNSR))

RFModel_HND <- subset(HNDDnormal, select = c(Q_m3.s,No3_mg.l,T2M,ROSS,VSWL1,VSWL2,VSWL3,VSWL4,SNSR))
RFModel_HND80 <- subset(HND80, select = c(Q_m3.s,No3_mg.l,T2M,ROSS,VSWL1,VSWL2,VSWL3,VSWL4,SNSR))
RFModel_HND20 <- subset(HND20, select = c(Q_m3.s,No3_mg.l,T2M,ROSS,VSWL1,VSWL2,VSWL3,VSWL4,SNSR))

RFModel_NNC <- subset(NNCDnormal, select = c(Q_m3.s,No3_mg.l,T2M,ROSS,VSWL1,VSWL2,VSWL3,VSWL4,SNSR))
RFModel_NNC80 <- subset(NNC80, select = c(Q_m3.s,No3_mg.l,T2M,ROSS,VSWL1,VSWL2,VSWL3,VSWL4,SNSR))
RFModel_NNC20 <- subset(NNC20, select = c(Q_m3.s,No3_mg.l,T2M,ROSS,VSWL1,VSWL2,VSWL3,VSWL4,SNSR))

RFModel_SBH <- subset(SBHDnormal, select = c(Q_m3.s,No3_mg.l,T2M,ROSS,VSWL1,VSWL2,VSWL3,VSWL4,SNSR))
RFModel_SBH80 <- subset(SBH80, select = c(Q_m3.s,No3_mg.l,T2M,ROSS,VSWL1,VSWL2,VSWL3,VSWL4,SNSR))
RFModel_SBH20 <- subset(SBH20, select = c(Q_m3.s,No3_mg.l,T2M,ROSS,VSWL1,VSWL2,VSWL3,VSWL4,SNSR))

RFModel_SMC <- subset(SMCDnormal, select = c(Q_cfs,No3_mg.l,T2M,ROSS,VSWL1,VSWL2,VSWL3,VSWL4,SNSR))
RFModel_SMC80 <- subset(SMC80, select = c(Q_cfs,No3_mg.l,T2M,ROSS,VSWL1,VSWL2,VSWL3,VSWL4,SNSR))
RFModel_SMC20 <- subset(SMC20, select = c(Q_cfs,No3_mg.l,T2M,ROSS,VSWL1,VSWL2,VSWL3,VSWL4,SNSR))

RFModel_TMR <- subset(TMRDnormal, select = c(Q_cfs,No3_mg.l,T2M,ROSS,VSWL1,VSWL2,VSWL3,VSWL4,SNSR))
RFModel_TMR80 <- subset(TMR80, select = c(Q_cfs,No3_mg.l,T2M,ROSS,VSWL1,VSWL2,VSWL3,VSWL4,SNSR))
RFModel_TMR20 <- subset(TMR20, select = c(Q_cfs,No3_mg.l,T2M,ROSS,VSWL1,VSWL2,VSWL3,VSWL4,SNSR))

RFModel_WEB <- subset(WEBDnormal, select = c(Q_l.s,No3_mg.l,T2M,ROSS,VSWL1,VSWL2,VSWL3,VSWL4,SNSR))
RFModel_WEB80 <- subset(WEB80, select = c(Q_l.s,No3_mg.l,T2M,ROSS,VSWL1,VSWL2,VSWL3,VSWL4,SNSR))
RFModel_WEB20 <- subset(WEB20, select = c(Q_l.s,No3_mg.l,T2M,ROSS,VSWL1,VSWL2,VSWL3,VSWL4,SNSR))

#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
###                                                            ACC RF                                                                        ###
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#### random forest Qnorm
set.seed(900)
RF_ACC_2All <- randomForest(No3_mg.l ~ ., data = RFModel_ACC, prox = TRUE, importance = TRUE, keep.forest = TRUE, ntree = 250, do.trace = TRUE)
RF_ACC_2All

#### random forest Q80
set.seed(900)
RF_ACC_180 <- randomForest(No3_mg.l ~ ., data = RFModel_ACC80, prox = TRUE, importance = TRUE, keep.forest = TRUE, ntree = 250, do.trace = TRUE)
RF_ACC_180

#### random forest Q20
set.seed(900)
RF_ACC_120 <- randomForest(No3_mg.l ~ ., data = RFModel_ACC20, prox = TRUE, importance = TRUE, keep.forest = TRUE, ntree = 250, do.trace = TRUE)
RF_ACC_120

#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
###                                                            HND RF                                                                        ###
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#### random forest Qnorm
set.seed(900)
RF_HND_2All <- randomForest(No3_mg.l ~ ., data = RFModel_HND, prox = TRUE, importance = TRUE, keep.forest = TRUE, ntree = 250, do.trace = TRUE)
RF_HND_2All

#### random forest Q80
set.seed(900)
RF_HND_180 <- randomForest(No3_mg.l ~ ., data = RFModel_HND80, prox = TRUE, importance = TRUE, keep.forest = TRUE, ntree = 250, do.trace = TRUE)
RF_HND_180

#### random forest Q20
set.seed(900)
RF_HND_120 <- randomForest(No3_mg.l ~ ., data = RFModel_HND20, prox = TRUE, importance = TRUE, keep.forest = TRUE, ntree = 250, do.trace = TRUE)
RF_HND_120

#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
###                                                            NNC RF                                                                        ###
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#### random forest Qnorm
set.seed(900)
RF_NNC_2All <- randomForest(No3_mg.l ~ ., data = RFModel_NNC, prox = TRUE, importance = TRUE, keep.forest = TRUE, ntree = 400, do.trace = TRUE)
RF_NNC_2All

#### random forest Q80
set.seed(900)
RF_NNC_180 <- randomForest(No3_mg.l ~ ., data = RFModel_NNC80, prox = TRUE, importance = TRUE, keep.forest = TRUE, ntree = 400, do.trace = TRUE)
RF_NNC_180

#### random forest Q20
set.seed(900)
RF_NNC_120 <- randomForest(No3_mg.l ~ ., data = RFModel_NNC20, prox = TRUE, importance = TRUE, keep.forest = TRUE, ntree = 400, do.trace = TRUE)
RF_NNC_120

#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
###                                                            SBH RF                                                                        ###
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#### random forest Qnorm
set.seed(900)
RF_SBH_2All <- randomForest(No3_mg.l ~ ., data = RFModel_SBH, prox = TRUE, importance = TRUE, keep.forest = TRUE, ntree = 300, do.trace = TRUE)
RF_SBH_2All

#### random forest Q80
set.seed(900)
RF_SBH_180 <- randomForest(No3_mg.l ~ ., data = RFModel_SBH80, prox = TRUE, importance = TRUE, keep.forest = TRUE, ntree = 300, do.trace = TRUE)
RF_SBH_180

#### random forest Q20
set.seed(900)
RF_SBH_120 <- randomForest(No3_mg.l ~ ., data = RFModel_SBH20, prox = TRUE, importance = TRUE, keep.forest = TRUE, ntree = 300, do.trace = TRUE)
RF_SBH_120

#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
###                                                            SMC RF                                                                        ###
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#### random forest Qnorm
set.seed(900)
RF_SMC_2All <- randomForest(No3_mg.l ~ ., data = RFModel_SMC, prox = TRUE, importance = TRUE, keep.forest = TRUE, ntree = 300, do.trace = TRUE)
RF_SMC_2All

#### random forest Q80
set.seed(900)
RF_SMC_180 <- randomForest(No3_mg.l ~ ., data = RFModel_SMC80, prox = TRUE, importance = TRUE, keep.forest = TRUE, ntree = 300, do.trace = TRUE)
RF_SMC_180

#### random forest Q20
set.seed(900)
RF_SMC_120 <- randomForest(No3_mg.l ~ ., data = RFModel_SMC20, prox = TRUE, importance = TRUE, keep.forest = TRUE, ntree = 300, do.trace = TRUE)
RF_SMC_120

#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
###                                                            TMR RF                                                                        ###
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#### random forest Qnorm
set.seed(900)
RF_TMR_2All <- randomForest(No3_mg.l ~ ., data = RFModel_TMR, prox = TRUE, importance = TRUE, keep.forest = TRUE, ntree = 300, do.trace = TRUE)
RF_TMR_2All

#### random forest Q80
set.seed(900)
RF_TMR_180 <- randomForest(No3_mg.l ~ ., data = RFModel_TMR80, prox = TRUE, importance = TRUE, keep.forest = TRUE, ntree = 300, do.trace = TRUE)
RF_TMR_180

#### random forest Q20
set.seed(900)
RF_TMR_120 <- randomForest(No3_mg.l ~ ., data = RFModel_TMR20, prox = TRUE, importance = TRUE, keep.forest = TRUE, ntree = 300, do.trace = TRUE)
RF_TMR_120

#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
###                                                            WEB RF                                                                        ###
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#### random forest Qnorm
set.seed(900)
RF_WEB_2All <- randomForest(No3_mg.l ~ ., data = RFModel_WEB, prox = TRUE, importance = TRUE, keep.forest = TRUE, ntree = 300, do.trace = TRUE)
RF_WEB_2All

#### random forest Q80
set.seed(900)
RF_WEB_180 <- randomForest(No3_mg.l ~ ., data = RFModel_WEB80, prox = TRUE, importance = TRUE, keep.forest = TRUE, ntree = 400, do.trace = TRUE, na.action = na.omit)
RF_WEB_180

#### random forest Q20
set.seed(900)
RF_WEB_120 <- randomForest(No3_mg.l ~ ., data = RFModel_WEB20, prox = TRUE, importance = TRUE, keep.forest = TRUE, ntree = 400, do.trace = TRUE, na.action = na.omit)
RF_WEB_120

#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#                         FIGURES 15, 18, 21 & 24                        #
#                   Mean minimal depth for all RF models                 #
#                       These will take a few hours                      #
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#### ACC
VIMP_ACC_MDD <- min_depth_distribution(RF_ACC_2All)
ACCVIMP_Qall <- plot_min_depth_distribution(VIMP_ACC_MDD, k = 8, min_no_of_trees = 0, mean_sample = "top_trees", mean_scale = FALSE, mean_round = 1, main = "(A)")
ACCVIMP_Qall

VIMP_ACC180_MDD <- min_depth_distribution(RF_ACC_180)
ACCVIMP_180 <- plot_min_depth_distribution(VIMP_ACC180_MDD, k = 8, min_no_of_trees = 0, mean_sample = "top_trees", mean_scale = FALSE, mean_round = 1, main = "(A1)")
ACCVIMP_180

VIMP_ACC120_MDD <- min_depth_distribution(RF_ACC_120)
ACCVIMP_120 <- plot_min_depth_distribution(VIMP_ACC120_MDD, k = 8, min_no_of_trees = 0, mean_sample = "top_trees", mean_scale = FALSE, mean_round = 1, main = "(A2)")
ACCVIMP_120

#### HND
VIMP_HND_MDD <- min_depth_distribution(RF_HND_2All)
HNDVIMP_Qall <- plot_min_depth_distribution(VIMP_HND_MDD, k = 8, min_no_of_trees = 0, mean_sample = "top_trees", mean_scale = FALSE, mean_round = 1, main = "(B)")
HNDVIMP_Qall

VIMP_HND180_MDD <- min_depth_distribution(RF_HND_180)
HNDVIMP_180 <- plot_min_depth_distribution(VIMP_HND180_MDD, k = 8, min_no_of_trees = 0, mean_sample = "top_trees", mean_scale = FALSE, mean_round = 1, main = "(B1)")
HNDVIMP_180

VIMP_HND120_MDD <- min_depth_distribution(RF_HND_120)
HNDVIMP_120 <- plot_min_depth_distribution(VIMP_HND120_MDD, k = 8, min_no_of_trees = 0, mean_sample = "top_trees", mean_scale = FALSE, mean_round = 1, main = "(B2)")
HNDVIMP_120

#### NNC
VIMP_NNC_MDD <- min_depth_distribution(RF_NNC_2All)
NNCVIMP_Qall <- plot_min_depth_distribution(VIMP_NNC_MDD, k = 7, min_no_of_trees = 0, mean_sample = "top_trees", mean_scale = FALSE, mean_round = 1, main = "(C)")
NNCVIMP_Qall

VIMP_NNC180_MDD <- min_depth_distribution(RF_NNC_180)
NNCVIMP_180 <- plot_min_depth_distribution(VIMP_NNC180_MDD, k = 7, min_no_of_trees = 0, mean_sample = "top_trees", mean_scale = FALSE, mean_round = 1, main = "(C1)")
NNCVIMP_180

VIMP_NNC120_MDD <- min_depth_distribution(RF_NNC_120)
NNCVIMP_120 <- plot_min_depth_distribution(VIMP_NNC120_MDD, k = 7, min_no_of_trees = 0, mean_sample = "top_trees", mean_scale = FALSE, mean_round = 1, main = "(C2)")
NNCVIMP_120

#### SBH
VIMP_SBH_MDD <- min_depth_distribution(RF_SBH_2All)
SBHVIMP_Qall <- plot_min_depth_distribution(VIMP_SBH_MDD, k = 8, min_no_of_trees = 0, mean_sample = "top_trees", mean_scale = FALSE, mean_round = 1, main = "(D)")
SBHVIMP_Qall

VIMP_SBH180_MDD <- min_depth_distribution(RF_SBH_180)
SBHVIMP_180 <- plot_min_depth_distribution(VIMP_SBH180_MDD, k = 8, min_no_of_trees = 0, mean_sample = "top_trees", mean_scale = FALSE, mean_round = 1, main = "(D1)")
SBHVIMP_180

VIMP_SBH120_MDD <- min_depth_distribution(RF_SBH_120)
SBHVIMP_120 <- plot_min_depth_distribution(VIMP_SBH120_MDD, k = 8, min_no_of_trees = 0, mean_sample = "top_trees", mean_scale = FALSE, mean_round = 1, main = "(D2)")
SBHVIMP_120

#### SMC
VIMP_SMC_MDD <- min_depth_distribution(RF_SMC_2All)
SMCVIMP_Qall <- plot_min_depth_distribution(VIMP_SMC_MDD, k = 8, min_no_of_trees = 0, mean_sample = "top_trees", mean_scale = FALSE, mean_round = 1, main = "(E)")
SMCVIMP_Qall

VIMP_SMC180_MDD <- min_depth_distribution(RF_SMC_180)
SMCVIMP_180 <- plot_min_depth_distribution(VIMP_SMC180_MDD, k = 8, min_no_of_trees = 0, mean_sample = "top_trees", mean_scale = FALSE, mean_round = 1, main = "(E1)")
SMCVIMP_180

VIMP_SMC120_MDD <- min_depth_distribution(RF_SMC_120)
SMCVIMP_120 <- plot_min_depth_distribution(VIMP_SMC120_MDD, k = 8, min_no_of_trees = 0, mean_sample = "top_trees", mean_scale = FALSE, mean_round = 1, main = "(E2)")
SMCVIMP_120

#### TMR
VIMP_TMR_MDD <- min_depth_distribution(RF_TMR_2All)
TMRVIMP_Qall <- plot_min_depth_distribution(VIMP_TMR_MDD, k = 8, min_no_of_trees = 0, mean_sample = "top_trees", mean_scale = FALSE, mean_round = 1, main = "(F)")
TMRVIMP_Qall

VIMP_TMR180_MDD <- min_depth_distribution(RF_TMR_180)
TMRVIMP_180 <- plot_min_depth_distribution(VIMP_TMR180_MDD, k = 8, min_no_of_trees = 0, mean_sample = "top_trees", mean_scale = FALSE, mean_round = 1, main = "(F1)")
TMRVIMP_180

VIMP_TMR120_MDD <- min_depth_distribution(RF_TMR_120)
TMRVIMP_120 <- plot_min_depth_distribution(VIMP_TMR120_MDD, k = 8, min_no_of_trees = 0, mean_sample = "top_trees", mean_scale = FALSE, mean_round = 1, main = "(F2)")
TMRVIMP_120

#### WEB
VIMP_WEB_MDD <- min_depth_distribution(RF_WEB_2All)
WEBVIMP_Qall <- plot_min_depth_distribution(VIMP_WEB_MDD, k = 8, min_no_of_trees = 0, mean_sample = "top_trees", mean_scale = FALSE, mean_round = 1, main = "(G)")
WEBVIMP_Qall

VIMP_WEB180_MDD <- min_depth_distribution(RF_WEB_180)
WEBVIMP_180 <- plot_min_depth_distribution(VIMP_WEB180_MDD, k = 8, min_no_of_trees = 0, mean_sample = "top_trees", mean_scale = FALSE, mean_round = 1, main = "(G1)")
WEBVIMP_180

VIMP_WEB120_MDD <- min_depth_distribution(RF_WEB_120)
WEBVIMP_120 <- plot_min_depth_distribution(VIMP_WEB120_MDD, k = 8, min_no_of_trees = 0, mean_sample = "top_trees", mean_scale = FALSE, mean_round = 1, main = "(G2)")
WEBVIMP_120

# Final Figures 15, 18, 21 & 24 
grid.arrange(ACCVIMP_Qall, HNDVIMP_Qall, ACCVIMP_180, HNDVIMP_180, ACCVIMP_120, HNDVIMP_120, ncol = 2)
grid.arrange(NNCVIMP_Qall, SBHVIMP_Qall, NNCVIMP_180, SBHVIMP_180, NNCVIMP_120, SBHVIMP_120, ncol = 2)
grid.arrange(SMCVIMP_Qall, TMRVIMP_Qall, SMCVIMP_180, TMRVIMP_180, SMCVIMP_120, TMRVIMP_120, ncol = 2)
grid.arrange(WEBVIMP_Qall, WEBVIMP_180, WEBVIMP_120, ncol = 1)

#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#                    FIGURES 16, 17, 19, 20, 22, 23, 25                  #
#                   Partial Dependence for all RF models                 #
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
plot.new()
#### ACC
plotmo(RF_ACC_2All, pmethod = "apartdep", ylim = NULL, degree2 = 0, caption = "(A)", mfrow = c(2,4), col = "#000000", lwd = 2)
plotmo(RF_ACC_180, pmethod = "apartdep", ylim = NULL, degree2 = 0, caption = "(A1)", mfrow = c(2,4), col = "#333333", lwd = 2)
plotmo(RF_ACC_120, pmethod = "apartdep", ylim = NULL, degree2 = 0, caption = "(A2)", mfrow = c(2,4), col = "#999999", lwd = 2)
#### HND
plotmo(RF_HND_2All, pmethod = "apartdep", ylim = NULL, degree2 = 0, caption = "(B)", mfrow = c(2,4), col = "#000000", lwd = 2)
plotmo(RF_HND_180, pmethod = "apartdep", ylim = NULL, degree2 = 0, caption = "(B1)", mfrow = c(2,4), col = "#333333", lwd = 2)
plotmo(RF_HND_120, pmethod = "apartdep", ylim = NULL, degree2 = 0, caption = "(B2)", mfrow = c(2,4), col = "#999999", lwd = 2)
#### NNC
plotmo(RF_NNC_2All, pmethod = "apartdep", ylim = NULL, degree2 = 0, caption = "(C)", mfrow = c(2,4), col = "#000000", lwd = 2)
plotmo(RF_NNC_180, pmethod = "apartdep", ylim = NULL, degree2 = 0, caption = "(C1)", mfrow = c(2,4), col = "#333333", lwd = 2)
plotmo(RF_NNC_120, pmethod = "apartdep", ylim = NULL, degree2 = 0, caption = "(C2)", mfrow = c(2,4), col = "#999999", lwd = 2)
#### SBH
plotmo(RF_SBH_2All, pmethod = "apartdep", ylim = NULL, degree2 = 0, caption = "(D)", mfrow = c(2,4), col = "#000000", lwd = 2)
plotmo(RF_SBH_180, pmethod = "apartdep", ylim = NULL, degree2 = 0, caption = "(D1)", mfrow = c(2,4), col = "#333333", lwd = 2)
plotmo(RF_SBH_120, pmethod = "apartdep", ylim = NULL, degree2 = 0, caption = "(D2)", mfrow = c(2,4), col = "#999999", lwd = 2)
#### SMC
plotmo(RF_SMC_2All, pmethod = "apartdep", ylim = NULL, degree2 = 0, caption = "(E)", mfrow = c(2,4), col = "#000000", lwd = 2)
plotmo(RF_SMC_180, pmethod = "apartdep", ylim = NULL, degree2 = 0, caption = "(E1)", mfrow = c(2,4), col = "#333333", lwd = 2)
plotmo(RF_SMC_120, pmethod = "apartdep", ylim = NULL, degree2 = 0, caption = "(E2)", mfrow = c(2,4), col = "#999999", lwd = 2)
#### TMR
plotmo(RF_TMR_2All, pmethod = "apartdep", ylim = NULL, degree2 = 0, caption = "(F)", mfrow = c(2,4), col = "#000000", lwd = 2)
plotmo(RF_TMR_180, pmethod = "apartdep", ylim = NULL, degree2 = 0, caption = "(F1)", mfrow = c(2,4), col = "#333333", lwd = 2)
plotmo(RF_TMR_120, pmethod = "apartdep", ylim = NULL, degree2 = 0, caption = "(F2)", mfrow = c(2,4), col = "#999999", lwd = 2)
#### WEB
plotmo(RF_WEB_2All, pmethod = "apartdep", ylim = NULL, degree2 = 0, caption = "(G)", mfrow = c(2,4), col = "#000000", lwd = 2)
plotmo(RF_WEB_180, pmethod = "apartdep", ylim = NULL, degree2 = 0, caption = "(G1)", mfrow = c(2,4), col = "#333333", lwd = 2)
plotmo(RF_WEB_120, pmethod = "apartdep", ylim = NULL, degree2 = 0, caption = "(G2)", mfrow = c(2,4), col = "#999999", lwd = 2)


#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#                             Model Accuracy                             #
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
plot.new()
par(mfrow = c(7, 3))
plot(RF_ACC_2All, main = "Random Forrest Regression Accuracy ACC Qnorm", sub = "% of Var explained")
plot(RF_ACC_180, main = "Random Forrest Regression Accuracy ACC Q80", sub = "% of Var explained")
plot(RF_ACC_120, main = "Random Forrest Regression Accuracy ACC Q20", sub = "% of Var explained")
plot(RF_HND_2All, main = "Random Forrest Regression Accuracy HND Qnorm", sub = "% of Var explained")
plot(RF_HND_180, main = "Random Forrest Regression Accuracy HND Q80", sub = "% of Var explained")
plot(RF_HND_120, main = "Random Forrest Regression Accuracy HND Q20", sub = "% of Var explained")
plot(RF_NNC_2All, main = "Random Forrest Regression Accuracy NNC Qnorm", sub = "% of Var explained")
plot(RF_NNC_180, main = "Random Forrest Regression Accuracy NNC Q80", sub = "% of Var explained")
plot(RF_NNC_120, main = "Random Forrest Regression Accuracy NNC Q20", sub = "% of Var explained")
plot(RF_SBH_2All, main = "Random Forrest Regression Accuracy SBH Qnorm", sub = "% of Var explained")
plot(RF_SBH_180, main = "Random Forrest Regression Accuracy SBH Q80", sub = "% of Var explained")
plot(RF_SBH_120, main = "Random Forrest Regression Accuracy SBH Q20", sub = "% of Var explained")
plot(RF_SMC_2All, main = "Random Forrest Regression Accuracy SMC Qnorm", sub = "% of Var explained")
plot(RF_SMC_180, main = "Random Forrest Regression Accuracy SMC Q80", sub = "% of Var explained")
plot(RF_SMC_120, main = "Random Forrest Regression Accuracy SMC Q20", sub = "% of Var explained")
plot(RF_TMR_2All, main = "Random Forrest Regression Accuracy TMR Qnorm", sub = "% of Var explained")
plot(RF_TMR_180, main = "Random Forrest Regression Accuracy TMR Q80", sub = "% of Var explained")
plot(RF_TMR_120, main = "Random Forrest Regression Accuracy TMR Q20", sub = "% of Var explained")
plot(RF_WEB_2All, main = "Random Forrest Regression Accuracy WEB Qnorm", sub = "% of Var explained")
plot(RF_WEB_180, main = "Random Forrest Regression Accuracy WEB Q80", sub = "% of Var explained")
plot(RF_WEB_120, main = "Random Forrest Regression Accuracy WEB Q20", sub = "% of Var explained")


#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
###                      VIMP plots for all RF models                  ###
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
plot.new()
par(mfrow = c(4, 6))
VIPACC2All <- varImpPlot(RF_ACC_2All, sort = TRUE, main = "VarImp at ACC Qnorm", type = 1, pch = 20, frame.plot = TRUE, pt.cex = 2)
VIPACC180 <- varImpPlot(RF_ACC_180, sort = TRUE, main = "VarImp at ACC Q80", type = 1, pch = 20, frame.plot = TRUE, pt.cex = 2)
VIPACC120 <- varImpPlot(RF_ACC_120, sort = TRUE, main = "VarImp at ACC Q20", type = 1, pch = 20, frame.plot = TRUE, pt.cex = 2)

VIPHND2All <- varImpPlot(RF_HND_2All, sort = TRUE, main = "VarImp at HND Qnorm", type = 1, pch = 20, frame.plot = TRUE, pt.cex = 2)
VIPHND180 <- varImpPlot(RF_HND_180, sort = TRUE, main = "VarImp at HND Q80", type = 1, pch = 20, frame.plot = TRUE, pt.cex = 2)
VIPHND120 <- varImpPlot(RF_HND_120, sort = TRUE, main = "VarImp at HND Q20", type = 1, pch = 20, frame.plot = TRUE, pt.cex = 2)

VIPNNC2All <- varImpPlot(RF_NNC_2All, sort = TRUE, main = "VarImp at NNC Qnorm", type = 1, pch = 20, frame.plot = TRUE, pt.cex = 2)
VIPNNC180 <- varImpPlot(RF_NNC_180, sort = TRUE, main = "VarImp at NNC Q80", type = 1, pch = 20, frame.plot = TRUE, pt.cex = 2)
VIPNNC120 <- varImpPlot(RF_NNC_120, sort = TRUE, main = "VarImp at NNC Q20", type = 1, pch = 20, frame.plot = TRUE, pt.cex = 2)

VIPSBH2All <- varImpPlot(RF_SBH_2All, sort = TRUE, main = "VarImp at SBH Qnorm", type = 1, pch = 20, frame.plot = TRUE, pt.cex = 2)
VIPSBH180 <- varImpPlot(RF_SBH_180, sort = TRUE, main = "VarImp at SBH Q80", type = 1, pch = 20, frame.plot = TRUE, pt.cex = 2)
VIPSBH120 <- varImpPlot(RF_SBH_120, sort = TRUE, main = "VarImp at SBH Q20", type = 1, pch = 20, frame.plot = TRUE, pt.cex = 2)

VIPSMC2All <- varImpPlot(RF_SMC_2All, sort = TRUE, main = "VarImp at SMC Qnorm", type = 1, pch = 20, frame.plot = TRUE, pt.cex = 2)
VIPSMC180 <- varImpPlot(RF_SMC_180, sort = TRUE, main = "VarImp at SMC Q80", type = 1, pch = 20, frame.plot = TRUE, pt.cex = 2)
VIPSMC120 <- varImpPlot(RF_SMC_120, sort = TRUE, main = "VarImp at SMC Q20", type = 1, pch = 20, frame.plot = TRUE, pt.cex = 2)

VIPTMR2All <- varImpPlot(RF_TMR_2All, sort = TRUE, main = "VarImp at TMR Qnorm", type = 1, pch = 20, frame.plot = TRUE, pt.cex = 2)
VIPTMR180 <- varImpPlot(RF_TMR_180, sort = TRUE, main = "VarImp at TMR Q80", type = 1, pch = 20, frame.plot = TRUE, pt.cex = 2)
VIPTMR120 <- varImpPlot(RF_TMR_120, sort = TRUE, main = "VarImp at TMR Q20", type = 1, pch = 20, frame.plot = TRUE, pt.cex = 2)

VIPWEB2All <- varImpPlot(RF_WEB_2All, sort = TRUE, main = "VarImp at WEB Qnorm", type = 1, pch = 20, frame.plot = TRUE, pt.cex = 2)
VIPWEB180 <- varImpPlot(RF_WEB_180, sort = TRUE, main = "VarImp at WEB Q80", type = 1, pch = 20, frame.plot = TRUE, pt.cex = 2)
VIPWEB120 <- varImpPlot(RF_WEB_120, sort = TRUE, main = "VarImp at WEB Q20", type = 1, pch = 20, frame.plot = TRUE, pt.cex = 2)

#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#                                FIGURE 26                               #
#                        Cross-correlation functions                     #
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
###                         ACC Qnorm                                 ###
#/////////////////////////////////////////////////////////////////////////
Q1_ACC <- dplyr::select(ACCnormal, Q_m3.s)
N1_ACC <- dplyr::select(ACCnormal, No3_mg.l)
Q1_ACC_ts <-  ts(log(Q1_ACC[,1]), start = 2015+10/12, frequency = 24)
N1_ACC_ts <-  ts(log(N1_ACC[,1]), start = 2015+10/12, frequency = 24)

#view data to determine possible ARIMA model structure
dev.off()
plot.new()
acf(Q1_ACC_ts)
pacf(Q1_ACC_ts)

#look for ARIMA model
armodel_ACC = arima(Q1_ACC_ts, order = c(0,1,1), seasonal = c(0,0,2))
armodel_ACC
coef(armodel_ACC)
pacf(armodel_ACC$residuals, lag.max = 24)
acf(armodel_ACC$residuals, lag.max = 24)

#pre-whiten n-data with fitted ARIMA model
Nfitted_ACC <- N1_ACC_ts - fitted(Arima(N1_ACC_ts, model = armodel_ACC))
ACCresiduals <- ts(armodel_ACC$residuals)
ACCfittedts <- ts(Nfitted_ACC)
CCFACC <- ccf(ACCresiduals, ACCfittedts, lag.max = 24)
CCFACC
sum(Q1_ACC$Q_m3.s - ACCresiduals) < 10^6

#/////////////////////////////////////////////////////////////////////////
###                         ACC Q80                                   ###
#/////////////////////////////////////////////////////////////////////////
Q1_ACC1 <- dplyr::select(ACC80, Q_m3.s)
N1_ACC1 <- dplyr::select(ACC80, No3_mg.l)
Q1_ACC_ts1 <-  ts(log(Q1_ACC1[,1]), start = 2015+10/12, frequency = 24)
N1_ACC_ts1 <-  ts(log(N1_ACC1[,1]), start = 2015+10/12, frequency = 24)

#view data to determine possible ARIMA model structure
dev.off()
plot.new()
acf(Q1_ACC_ts1)
pacf(Q1_ACC_ts1)

#look for ARIMA model
armodel_ACC1 = arima(Q1_ACC_ts1, order = c(2,1,1), seasonal = c(0,0,2))
armodel_ACC1
coef(armodel_ACC1)
pacf(armodel_ACC1$residuals, lag.max = 24)
acf(armodel_ACC1$residuals, lag.max = 24)

#pre-whiten n-data with fitted ARIMA model
Nfitted_ACC1 <- N1_ACC_ts1 - fitted(Arima(N1_ACC_ts1, model = armodel_ACC1))
ACCresiduals1 <- ts(armodel_ACC1$residuals)
ACCfittedts1 <- ts(Nfitted_ACC1)
CCFACC1 <- ccf(ACCresiduals1, ACCfittedts1, lag.max = 24)
CCFACC1
sum(Q1_ACC1$Q_m3.s - ACCresiduals1) < 10^6

#/////////////////////////////////////////////////////////////////////////
###                                HND Qnorm                           ###
#/////////////////////////////////////////////////////////////////////////
Q1_HND <- dplyr::select(HNDnormal, Q_m3.s)
N1_HND <- dplyr::select(HNDnormal, No3_mg.l)
Q1_HND_ts <-  ts(log(Q1_HND), start = 2015+10/12, frequency = 24)
N1_HND_ts <-  ts(log(N1_HND), start = 2015+10/12, frequency = 24)

#view data to determine ARIMA model structure
acf(Q1_HND_ts)
pacf(Q1_HND_ts)

#look for ARIMA model
armodel_HND = arima(Q1_HND_ts, order = c(2,1,1))
armodel_HND
coef(armodel_HND)
pacf(armodel_HND$residuals, lag.max = 24)
acf(armodel_HND$residuals, lag.max = 24)

#pre-whiten n-data with fitted ARIMA model
Nfitted1_HND <- N1_HND_ts - fitted(Arima(N1_HND_ts, model = armodel_HND))
HNDresiduals <- ts(armodel_HND$residuals)
HNDfittedts <- ts(Nfitted1_HND[,1])
CCFHND <- ccf(HNDresiduals, HNDfittedts)
CCFHND
sum(Q1_HND$Q_m3.s - HNDresiduals) < 10^6

#/////////////////////////////////////////////////////////////////////////
###                                HND Q80                             ###
#/////////////////////////////////////////////////////////////////////////
Q1_HND1 <- dplyr::select(HND80, Q_m3.s)
N1_HND1 <- dplyr::select(HND80, No3_mg.l)
Q1_HND_ts1 <-  ts(log(Q1_HND1), start = 2015+10/12, frequency = 24)
N1_HND_ts1 <-  ts(log(N1_HND1), start = 2015+10/12, frequency = 24)

#view data to determine ARIMA model structure
acf(Q1_HND_ts1)
pacf(Q1_HND_ts1)

#look for ARIMA model
armodel_HND1 = arima(Q1_HND_ts1, order = c(3,1,3), seasonal = c(0,0,1))
armodel_HND1
coef(armodel_HND1)
pacf(armodel_HND1$residuals, lag.max = 24)
acf(armodel_HND1$residuals, lag.max = 24)

#pre-whiten n-data with fitted ARIMA model
Nfitted1_HND1 <- N1_HND_ts1 - fitted(Arima(N1_HND_ts1, model = armodel_HND1))
HNDresiduals1 <- ts(armodel_HND1$residuals)
HNDfittedts1 <- ts(Nfitted1_HND1[,1])
CCFHND1 <- ccf(HNDresiduals1, HNDfittedts1)
CCFHND1
sum(Q1_HND1$Q_m3.s - HNDresiduals1) < 10^6

#/////////////////////////////////////////////////////////////////////////
###                                NNC Qnorm                           ###
#/////////////////////////////////////////////////////////////////////////
Q1_NNC <- dplyr::select(NNCnormal, Q_m3.s)
N1_NNC <- dplyr::select(NNCnormal, No3_mg.l)
Q1_NNC_ts <-  ts(log(Q1_NNC), start = 2015+10/12, frequency = 24)
N1_NNC_ts <-  ts(log(N1_NNC), start = 2015+10/12, frequency = 24)

#view data to determine ARIMA model structure
acf(Q1_NNC_ts)
pacf(Q1_NNC_ts)

#look for ARIMA model
armodel_NNC = arima(Q1_NNC_ts, order = c(1,1,2))
armodel_NNC 
coef(armodel_NNC)
pacf(armodel_NNC$residuals, lag.max = 24)
acf(armodel_NNC$residuals, lag.max = 24)

#pre-whiten n-data with fitted ARIMA model
Nfitted1_NNC <- N1_NNC_ts - fitted(Arima(N1_NNC_ts, model = armodel_NNC))
NNCresiduals <- ts(armodel_NNC$residuals)
NNCfittedts <- ts(Nfitted1_NNC[,1])
CCFNNC <- ccf(NNCresiduals, NNCfittedts)
CCFNNC
sum(Q1_NNC$Q_m3.s - NNCresiduals) < 10^6

#/////////////////////////////////////////////////////////////////////////
###                                NNC Q80                           ###
#/////////////////////////////////////////////////////////////////////////
Q1_NNC1 <- dplyr::select(NNC80, Q_m3.s)
N1_NNC1 <- dplyr::select(NNC80, No3_mg.l)
Q1_NNC_ts1 <-  ts(log(Q1_NNC1), start = 2015+10/12, frequency = 24)
N1_NNC_ts1 <-  ts(log(N1_NNC1), start = 2015+10/12, frequency = 24)

#view data to determine ARIMA model structure
acf(Q1_NNC_ts1)
pacf(Q1_NNC_ts1)

#look for ARIMA model
armodel_NNC1 = arima(Q1_NNC_ts1, order = c(2,1,0), seasonal = c(0,0,2))
armodel_NNC1 
coef(armodel_NNC1)
pacf(armodel_NNC1$residuals, lag.max = 24)
acf(armodel_NNC1$residuals, lag.max = 24)

#pre-whiten n-data with fitted ARIMA model
Nfitted1_NNC1 <- N1_NNC_ts1 - fitted(Arima(N1_NNC_ts1, model = armodel_NNC1))
NNCresiduals1 <- ts(armodel_NNC1$residuals)
NNCfittedts1 <- ts(Nfitted1_NNC1[,1])
CCFNNC1 <- ccf(NNCresiduals1, NNCfittedts1)
CCFNNC1
sum(Q1_NNC1$Q_m3.s - NNCresiduals1) < 10^6

#/////////////////////////////////////////////////////////////////////////
###                                SBH Qnorm                           ###
#/////////////////////////////////////////////////////////////////////////
Q1_SBH <- dplyr::select(SBHnormal, Q_m3.s)
N1_SBH <- dplyr::select(SBHnormal, No3_mg.l)
Q1_SBH_ts <-  ts(log(Q1_SBH), start = 2015+10/12, frequency = 24)
N1_SBH_ts <-  ts(log(N1_SBH), start = 2015+10/12, frequency = 24)

#view data to determine ARIMA model structure
acf(Q1_SBH_ts)
pacf(Q1_SBH_ts)

#look for ARIMA model
armodel_SBH = arima(Q1_SBH_ts, order = c(1,1,3), seasonal = c(1,0,0))
armodel_SBH 
coef(armodel_SBH)
pacf(armodel_SBH$residuals, lag.max = 24)
acf(armodel_SBH$residuals, lag.max = 24)

#pre-whiten n-data with fitted ARIMA model
Nfitted1_SBH <- N1_SBH_ts - fitted(Arima(N1_SBH_ts, model = armodel_SBH))
SBHresiduals <- ts(armodel_SBH$residuals)
SBHfittedts <- ts(Nfitted1_SBH[,1])
CCFSBH <- ccf(SBHresiduals, SBHfittedts)
CCFSBH
sum(Q1_SBH$Q_m3.s - SBHresiduals) < 10^6

#/////////////////////////////////////////////////////////////////////////
###                                SBH Q80                           ###
#/////////////////////////////////////////////////////////////////////////
Q1_SBH1 <- dplyr::select(SBH80, Q_m3.s)
N1_SBH1 <- dplyr::select(SBH80, No3_mg.l)
Q1_SBH_ts1 <-  ts(log(Q1_SBH1), start = 2015+10/12, frequency = 24)
N1_SBH_ts1 <-  ts(log(N1_SBH1), start = 2015+10/12, frequency = 24)

#view data to determine ARIMA model structure
acf(Q1_SBH_ts1)
pacf(Q1_SBH_ts1)

#look for ARIMA model
armodel_SBH1 = arima(Q1_SBH_ts1, order = c(1,1,1))
armodel_SBH1 
coef(armodel_SBH1)
pacf(armodel_SBH1$residuals, lag.max = 24)
acf(armodel_SBH1$residuals, lag.max = 24)

#pre-whiten n-data with fitted ARIMA model
Nfitted1_SBH1 <- N1_SBH_ts1 - fitted(Arima(N1_SBH_ts1, model = armodel_SBH1))
SBHresiduals1 <- ts(armodel_SBH1$residuals)
SBHfittedts1 <- ts(Nfitted1_SBH1[,1])
CCFSBH1 <- ccf(SBHresiduals1, SBHfittedts1)
CCFSBH1
sum(Q1_SBH1$Q_m3.s - SBHresiduals1) < 10^6

#/////////////////////////////////////////////////////////////////////////
###                                SMC Qnorm                           ###
#/////////////////////////////////////////////////////////////////////////
Q1_SMC <- dplyr::select(SMCnormal, Q_cfs)
N1_SMC <- dplyr::select(SMCnormal, No3_mg.l)
Q1_SMC_ts <-  ts(log(Q1_SMC), start = 2015+10/12, frequency = 24)
N1_SMC_ts <-  ts(log(N1_SMC), start = 2015+10/12, frequency = 24)

#view data to determine ARIMA model structure
acf(Q1_SMC_ts)
pacf(Q1_SMC_ts)

#look for ARIMA model
armodel_SMC = arima(Q1_SMC_ts, order = c(0,1,3), seasonal = c(0,0,1))
armodel_SMC 
coef(armodel_SMC)
pacf(armodel_SMC$residuals, lag.max = 24)
acf(armodel_SMC$residuals, lag.max = 24)

#pre-whiten n-data with fitted ARIMA model
Nfitted1_SMC <- N1_SMC_ts - fitted(Arima(N1_SMC_ts, model = armodel_SMC))
SMCresiduals <- ts(armodel_SMC$residuals)
SMCfittedts <- ts(Nfitted1_SMC[,1])
CCFSMC <- ccf(SMCresiduals, SMCfittedts)
CCFSMC
sum(Q1_SMC$Q_cfs - SMCresiduals) < 10^6

#/////////////////////////////////////////////////////////////////////////
###                                SMC Q80                           ###
#/////////////////////////////////////////////////////////////////////////
Q1_SMC1 <- dplyr::select(SMC80, Q_cfs)
N1_SMC1 <- dplyr::select(SMC80, No3_mg.l)
Q1_SMC_ts1 <-  ts(log(Q1_SMC1), start = 2015+10/12, frequency = 24)
N1_SMC_ts1 <-  ts(log(N1_SMC1), start = 2015+10/12, frequency = 24)

#view data to determine ARIMA model structure
acf(Q1_SMC_ts1)
pacf(Q1_SMC_ts1)

#look for ARIMA model
armodel_SMC1 = arima(Q1_SMC_ts1, order = c(2,1,1), seasonal = c(0,0,1))
armodel_SMC1 
coef(armodel_SMC1)
pacf(armodel_SMC1$residuals, lag.max = 24)
acf(armodel_SMC1$residuals, lag.max = 24)

#pre-whiten n-data with fitted ARIMA model
Nfitted1_SMC1 <- N1_SMC_ts1 - fitted(Arima(N1_SMC_ts1, model = armodel_SMC1))
SMCresiduals1 <- ts(armodel_SMC1$residuals)
SMCfittedts1 <- ts(Nfitted1_SMC1[,1])
CCFSMC1 <- ccf(SMCresiduals1, SMCfittedts1)
CCFSMC1
sum(Q1_SMC1$Q_cfs - SMCresiduals1) < 10^6


#/////////////////////////////////////////////////////////////////////////
###                                TMR Qnorm                           ###
#/////////////////////////////////////////////////////////////////////////
Q1_TMR <- dplyr::select(TMRnormal, Q_cfs)
N1_TMR <- dplyr::select(TMRnormal, No3_mg.l)
Q1_TMR_ts <-  ts(log(Q1_TMR), start = 2015+10/12, frequency = 24)
N1_TMR_ts <-  ts(log(N1_TMR), start = 2015+10/12, frequency = 24)

#view data to determine ARIMA model structure
acf(Q1_TMR_ts)
pacf(Q1_TMR_ts)

#look for ARIMA model
armodel_TMR = arima(Q1_TMR_ts, order = c(1,1,0), seasonal = c(1,0,0))
armodel_TMR 
coef(armodel_TMR)
pacf(armodel_TMR$residuals, lag.max = 24)
acf(armodel_TMR$residuals, lag.max = 24)

#pre-whiten n-data with fitted ARIMA model
Nfitted1_TMR <- N1_TMR_ts - fitted(Arima(N1_TMR_ts, model = armodel_TMR))
TMRresiduals <- ts(armodel_TMR$residuals)
TMRfittedts <- ts(Nfitted1_TMR[,1])
CCFTMR <- ccf(TMRresiduals, TMRfittedts)
CCFTMR
sum(Q1_TMR$Q_cfs - TMRresiduals) < 10^6

#/////////////////////////////////////////////////////////////////////////
###                                TMR Q80                           ###
#/////////////////////////////////////////////////////////////////////////
Q1_TMR1 <- dplyr::select(TMR80, Q_cfs)
N1_TMR1 <- dplyr::select(TMR80, No3_mg.l)
Q1_TMR_ts1 <-  ts(log(Q1_TMR1), start = 2015+10/12, frequency = 24)
N1_TMR_ts1 <-  ts(log(N1_TMR1), start = 2015+10/12, frequency = 24)

#view data to determine ARIMA model structure
acf(Q1_TMR_ts1)
pacf(Q1_TMR_ts1)

#look for ARIMA model
armodel_TMR1 = arima(Q1_TMR_ts1, order = c(1,1,1))
armodel_TMR1 
coef(armodel_TMR1)
pacf(armodel_TMR1$residuals, lag.max = 24)
acf(armodel_TMR1$residuals, lag.max = 24)

#pre-whiten n-data with fitted ARIMA model
Nfitted1_TMR1 <- N1_TMR_ts1 - fitted(Arima(N1_TMR_ts1, model = armodel_TMR1))
TMRresiduals1 <- ts(armodel_TMR1$residuals)
TMRfittedts1 <- ts(Nfitted1_TMR1[,1])
CCFTMR1 <- ccf(TMRresiduals1, TMRfittedts1)
CCFTMR1
sum(Q1_TMR1$Q_cfs - TMRresiduals1) < 10^6

#/////////////////////////////////////////////////////////////////////////
###                                WEB Qnorm                           ###
#/////////////////////////////////////////////////////////////////////////
Q1_WEB <- dplyr::select(WEBnormal, Q_l.s)
N1_WEB <- dplyr::select(WEBnormal, No3_mg.l)
Q1_WEB_ts <-  ts(log(Q1_WEB), start = 2013+10/12, frequency = 24)
N1_WEB_ts <-  ts(log(N1_WEB), start = 2013+10/12, frequency = 24)

#view data to determine ARIMA model structure
acf(Q1_WEB_ts)
pacf(Q1_WEB_ts)

#look for ARIMA model
armodel_WEB = arima(Q1_WEB_ts, order = c(2,1,2), seasonal = c(1,0,0))
armodel_WEB 
coef(armodel_WEB)
pacf(armodel_WEB$residuals, lag.max = 24)
acf(armodel_WEB$residuals, lag.max = 24)

#pre-whiten n-data with fitted ARIMA model
Nfitted1_WEB <- N1_WEB_ts - fitted(Arima(N1_WEB_ts, model = armodel_WEB))
WEBresiduals <- ts(armodel_WEB$residuals)
WEBfittedts <- ts(Nfitted1_WEB[,1])
CCFWEB <- ccf(WEBresiduals, WEBfittedts)
CCFWEB
sum(Q1_WEB$Q_l.s - WEBresiduals) < 10^6

#/////////////////////////////////////////////////////////////////////////
###                                WEB Q80                           ###
#/////////////////////////////////////////////////////////////////////////
Q1_WEB1 <- dplyr::select(WEB80, Q_l.s)
N1_WEB1 <- dplyr::select(WEB80, No3_mg.l)
Q1_WEB_ts1 <-  ts(Q1_WEB1, start = 2013+10/12, frequency = 24)
N1_WEB_ts1 <-  ts(N1_WEB1, start = 2013+10/12, frequency = 24)

#view data to determine ARIMA model structure
acf(Q1_WEB_ts1, na.action = na.pass)
pacf(Q1_WEB_ts1, na.action = na.pass)

#look for ARIMA model
armodel_WEB1 = arima(Q1_WEB_ts1, order = c(0,1,2), seasonal = c(0,0,3))
armodel_WEB1 
coef(armodel_WEB1)
pacf(armodel_WEB1$residuals, lag.max = 24, na.action = na.pass)
acf(armodel_WEB1$residuals, lag.max = 24, na.action = na.pass)

#pre-whiten n-data with fitted ARIMA model
Nfitted1_WEB1 <- N1_WEB_ts1 - fitted(Arima(N1_WEB_ts1, model = armodel_WEB1, na.action = na.pass))
WEBresiduals1 <- ts(armodel_WEB1$residuals)
WEBfittedts1 <- ts(Nfitted1_WEB1[,1])
CCFWEB1 <- ccf(WEBresiduals1, WEBfittedts1, na.action = na.pass)
CCFWEB1

#### plottiing CCFs (FIGURE 26)
plot.new()
par(mfrow = c(7, 2), mar = c(2.5, 3, 2, 1), oma = c(1, 2, 1, 2), mgp = c(1.3, 0.5, 0), 
    title(main = "", outer = TRUE, line = 0, cex.main = 2, cex = 1))
CCFACC <- Ccf(ACCresiduals, ACCfittedts, lag.max = 24, xlab = "lags (hours)", main = list("", cex = 1))
CCFACC1 <- Ccf(ACCresiduals1, ACCfittedts1, lag.max = 24, xlab = "lags (hours)", main = list("", cex = 1))
CCFHND <- Ccf(HNDresiduals, HNDfittedts, lag.max = 24, xlab = "lags (hours)", main = list("", cex = 1))
CCFHND1 <- Ccf(HNDresiduals1, HNDfittedts1, lag.max = 24, xlab = "lags (hours)", main = list("", cex = 1))
CCFNNC <- Ccf(NNCresiduals, NNCfittedts, lag.max = 24, xlab = "lags (hours)", main = list("", cex = 1))
CCFNNC1 <- Ccf(NNCresiduals1, NNCfittedts1, lag.max = 24, xlab = "lags (hours)", main = list("", cex = 1))
CCFSBH <- Ccf(SBHresiduals, SBHfittedts, lag.max = 24, xlab = "lags (hours)", main = list("", cex = 1))
CCFSBH1 <- Ccf(SBHresiduals1, SBHfittedts1, lag.max = 24, xlab = "lags (hours)", main = list("", cex = 1))
CCFSMC <- Ccf(SMCresiduals, SMCfittedts, lag.max = 24, xlab = "lags (hours)", main = list("", cex = 1))
CCFSMC1 <- Ccf(SMCresiduals1, SMCfittedts1, lag.max = 24, xlab = "lags (hours)", main = list("", cex = 1))
CCFTMR <- Ccf(TMRresiduals, TMRfittedts, lag.max = 24, xlab = "lags (hours)", main = list("", cex = 1))
CCFTMR1 <- Ccf(TMRresiduals1, TMRfittedts1, lag.max = 24, xlab = "lags (hours)", main = list("", cex = 1))
CCFWEB <- Ccf(WEBresiduals, WEBfittedts, lag.max = 24, xlab = "lags (hours)", main = list("", cex = 1))
CCFWEB1 <- Ccf(WEBresiduals1, WEBfittedts1, lag.max = 24, xlab = "lags (hours)", main = list("", cex = 1), na.remove = TRUE)
par(new=TRUE)
mtext("(A)", side = 3, line = 77, las = 1, cex = 0.8, adj = -1.15)
par(new=TRUE)
mtext("(A1)", side = 3, line = 77, las = 1, cex = 0.8, adj = 0.01)
par(new=TRUE)
mtext("(B)", side = 3, line = 64, las = 1, cex = 0.8, adj = -1.15)
par(new=TRUE)
mtext("(B1)", side = 3, line = 64, las = 1, cex = 0.8, adj = 0.01)
par(new=TRUE)
mtext("(C)", side = 3, line = 51.5, las = 1, cex = 0.8, adj = -1.15)
par(new=TRUE)
mtext("(C1)", side = 3, line = 51.5, las = 1, cex = 0.8, adj = 0.01)
par(new=TRUE)
mtext("(D)", side = 3, line = 38.5, las = 1, cex = 0.8, adj = -1.15)
par(new=TRUE)
mtext("(D1)", side = 3, line = 38.5, las = 1, cex = 0.8, adj = 0.01)
par(new=TRUE)
mtext("(E)", side = 3, line = 26, las = 1, cex = 0.8, adj = -1.15)
par(new=TRUE)
mtext("(E1)", side = 3, line = 26, las = 1, cex = 0.8, adj = 0.01)
par(new=TRUE)
mtext("(F)", side = 3, line = 13, las = 1, cex = 0.8, adj = -1.15)
par(new=TRUE)
mtext("(F1)", side = 3, line = 13, las = 1, cex = 0.8, adj = 0.01)
par(new=TRUE)
mtext("(G)", side = 3, line = 0.3, las = 1, cex = 0.8, adj = -1.15)
par(new=TRUE)
mtext("(G1)", side = 3, line = 0.3, las = 1, cex = 0.8, adj = 0.01)

#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#                              FIGURE 27                                #
#                         JAMES KIRCHNER IRFs                           #
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#### load IRF functions (IRFnnhs.R needs to be in the same directory)
source("IRFnnhs.R")

#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#                               ACC IRFs                                #
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#### Qnorm
ACCZZ <- IRF(y = log(ACCnormal$No3_mg.l), 
             x = log(ACCnormal$Q_m3.s), 
             m = 24,
             nk = 0,
             FD = TRUE,
             h = NULL,
             nu = 0.1,
             ARprob = 0.05,
             ARlim = 0.2,
             max.AR = 10,
             robust = TRUE,
             verbose=TRUE,
             max.chunk = 2e8,
             complete = FALSE)
ACCbb <- as.numeric(ACCZZ$IRF[,1])
#### Q80
ACCZZ1 <- IRF(y = log(ACC80$No3_mg.l), 
              x = log(ACC80$Q_m3.s), 
              m = 24,
              nk = 0,
              FD = TRUE,
              h = NULL,
              nu = 0.1,
              ARprob = 0.05,
              ARlim = 0.2,
              max.AR = 10,
              robust = TRUE,
              verbose=TRUE,
              max.chunk = 2e8,
              complete = FALSE)
ACCbb1 <- as.numeric(ACCZZ1$IRF[,1])
#### Q20
ACCZZ2 <- IRF(y = log(ACC20$No3_mg.l), 
              x = log(ACC20$Q_m3.s), 
              m = 24,
              nk = 0,
              FD = TRUE,
              h = NULL,
              nu = 0.1,
              ARprob = 0.05,
              ARlim = 0.2,
              max.AR = 10,
              robust = TRUE,
              verbose=TRUE,
              max.chunk = 2e8,
              complete = FALSE)
ACCbb2 <- as.numeric(ACCZZ2$IRF[,1])

#### extract plotting parameters from IRF outputs (Qnorm)
ACCIRF <- as.numeric(ACCZZ$IRF[,1])
ACCSE <- as.numeric(ACCZZ$se[,1])
ACCLags <- as.numeric(ACCZZ$lags)
ACCIRFPos <- ACCIRF+2*ACCSE #set up confidence bands with IRF +/- 2 * standard errors
ACCIRFNeg <- ACCIRF-2*ACCSE
ACClisttodf <- data.frame(ACCIRF, ACCSE, ACCLags, ACCIRFPos, ACCIRFNeg)
#### extract plotting parameters from IRF outputs (Q80)
ACCIRF1 <- as.numeric(ACCZZ1$IRF[,1])
ACCSE1 <- as.numeric(ACCZZ1$se[,1])
ACCLags1 <- as.numeric(ACCZZ1$lags)
ACCIRFPos1 <- ACCIRF1+2*ACCSE1
ACCIRFNeg1 <- ACCIRF1-2*ACCSE1
ACClisttodf1 <- data.frame(ACCIRF1, ACCSE1, ACCLags1, ACCIRFPos1, ACCIRFNeg1)
#### extract plotting parameters from IRF outputs (Q20)
ACCIRF2 <- as.numeric(ACCZZ2$IRF[,1])
ACCSE2 <- as.numeric(ACCZZ2$se[,1])
ACCLags2 <- as.numeric(ACCZZ2$lags)
ACCIRFPos2 <- ACCIRF2+2*ACCSE2
ACCIRFNeg2 <- ACCIRF2-2*ACCSE2
ACClisttodf2 <- data.frame(ACCIRF2, ACCSE2, ACCLags2, ACCIRFPos2, ACCIRFNeg2)
#### Plotting IRFs for ACC
ACCIRFggplot <- ggplot(data = NULL, aes(x = ACCLags, y = NULL))+
  geom_ribbon(aes(ymin = ACCIRFNeg, ymax = ACCIRFPos, color = "#82D5FF"), fill = "#FF8F8F", alpha = 0.5)+
  geom_ribbon(aes(ymin = ACCIRFNeg1, ymax = ACCIRFPos1, color = "#FF8F8F"), fill = "#82D5FF", alpha = 0.5)+
  geom_ribbon(aes(ymin = ACCIRFNeg2, ymax = ACCIRFPos2, color = "#45E090"), fill = "#45E090", alpha = 0.5)+
  geom_line(data = ACClisttodf, aes(y = ACCIRF), size = 1, linetype = 3)+
  geom_line(data = ACClisttodf1, aes(y = ACCIRF1), size = 1, linetype = "solid")+
  geom_line(data = ACClisttodf2, aes(y = ACCIRF2), size = 0.8, linetype = 3)+
  theme_bw()+
  theme(legend.position = "")+
  labs(title = "(A)",
       x = expression("lags (hours)"),
       y = expression(NO[3]-N))+
  theme(plot.title = element_text(size=14))+
  theme(axis.title.y = element_text(size=9))+
  theme(axis.title.x = element_text(size=9))
ACCIRFggplot

#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#                       HND study area IRFs                            #
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
HNDZZ <- IRF(y = log(HNDnormal$No3_mg.l), 
             x = log(HNDnormal$Q_m3.s), 
             m = 24,
             nk = 0,
             FD = TRUE,
             h = NULL,
             nu = 0.5,
             ARprob = 0.05,
             ARlim = 0.2,
             max.AR = 10,
             robust = TRUE,
             verbose=TRUE,
             max.chunk = 2e8,
             complete = FALSE)
HNDbb2 <- as.numeric(HNDZZ$IRF[,1])

HNDZZ1 <- IRF(y = log(HND80$No3_mg.l), 
              x = log(HND80$Q_m3.s), 
              m = 24,
              nk = 0,
              FD = TRUE,
              h = NULL,
              nu = 0.5,
              ARprob = 0.05,
              ARlim = 0.2,
              max.AR = 10,
              robust = TRUE,
              verbose=TRUE,
              max.chunk = 2e8,
              complete = FALSE)
HNDbbNEU <- as.numeric(HNDZZ1$IRF[,1])

HNDZZ2 <- IRF(y = log(HND20$No3_mg.l), 
              x = log(HND20$Q_m3.s), 
              m = 24,
              nk = 0,
              FD = TRUE,
              h = NULL,
              nu = 0.5,
              ARprob = 0.05,
              ARlim = 0.2,
              max.AR = 10,
              robust = TRUE,
              verbose=TRUE,
              max.chunk = 2e8,
              complete = FALSE)
HNDbbNEU1 <- as.numeric(HNDZZ2$IRF[,1])

#### extract plotting parameters from IRF outputs
HNDIRF <- as.numeric(HNDZZ$IRF[,1])
HNDSE <- as.numeric(HNDZZ$se[,1])
HNDLags <- as.numeric(HNDZZ$lags)
HNDIRFPos <- HNDIRF+2*HNDSE
HNDIRFNeg <- HNDIRF-2*HNDSE
HNDlisttodf <- data.frame(HNDIRF, HNDSE, HNDLags, HNDIRFPos, HNDIRFNeg)
#/////////////////////////
HNDIRF1 <- as.numeric(HNDZZ1$IRF[,1])
HNDSE1 <- as.numeric(HNDZZ1$se[,1])
HNDLags1 <- as.numeric(HNDZZ1$lags)
HNDIRFPos1 <- HNDIRF1+2*HNDSE1
HNDIRFNeg1 <- HNDIRF1-2*HNDSE1
HNDlisttodf1 <- data.frame(HNDIRF1, HNDSE1, HNDLags1, HNDIRFPos1, HNDIRFNeg1)
#/////////////////////////
HNDIRF2 <- as.numeric(HNDZZ2$IRF[,1])
HNDSE2 <- as.numeric(HNDZZ2$se[,1])
HNDLags2 <- as.numeric(HNDZZ2$lags)
HNDIRFPos2 <- HNDIRF2+2*HNDSE2
HNDIRFNeg2 <- HNDIRF2-2*HNDSE2
HNDlisttodf2 <- data.frame(HNDIRF2, HNDSE2, HNDLags2, HNDIRFPos2, HNDIRFNeg2)
#/////////////////////////
HNDIRFggplot <- ggplot(data = NULL, aes(x = HNDLags, y = NULL))+
  geom_ribbon(aes(ymin = HNDIRFNeg, ymax = HNDIRFPos, color = "#82D5FF"), fill = "#FF8F8F", alpha = 0.5)+
  geom_ribbon(aes(ymin = HNDIRFNeg1, ymax = HNDIRFPos1, color = "#FF8F8F"), fill = "#82D5FF", alpha = 0.5)+
  geom_ribbon(aes(ymin = HNDIRFNeg2, ymax = HNDIRFPos2, color = "#45E090"), fill = "#45E090", alpha = 0.5)+
  geom_line(data = HNDlisttodf, aes(y = HNDIRF), size = 1, linetype = 3, colour = "grey20")+
  geom_line(data = HNDlisttodf1, aes(y = HNDIRF1), size = 1.2, linetype = "solid")+
  geom_line(data = HNDlisttodf2, aes(y = HNDIRF2), size = 0.8, linetype = 3)+
  theme_bw()+
  theme(legend.position = "")+
  labs(title = "(B)",
       x = expression("lags (hours)"),
       y = expression(NO[3]-N))+
  theme(plot.title = element_text(size=14))+
  theme(axis.title.y = element_text(size=9))+
  theme(axis.title.x = element_text(size=9))
HNDIRFggplot

#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#                        NNC study area IRFs                            #
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
NNCZZ <- IRF(y = log(NNCnormal$No3_mg.l), 
             x = log(NNCnormal$Q_m3.s),
             m = 24,
             nk = 0,
             FD = TRUE,
             h = NULL,
             nu = 0.01,
             ARprob = 0.05,
             ARlim = 0.2,
             max.AR = 10,
             robust = TRUE,
             verbose=TRUE,
             max.chunk = 2e8,
             complete = TRUE)
NNCbb2 <- as.numeric(NNCZZ$IRF[,1])

NNCZZ1 <- IRF(y = log(NNC80$No3_mg.l), 
              x = log(NNC80$Q_m3.s),
              m = 24,
              nk = 0,
              FD = TRUE,
              h = NULL,
              nu = 0.01,
              ARprob = 0.05,
              ARlim = 0.2,
              max.AR = 10,
              robust = TRUE,
              verbose=TRUE,
              max.chunk = 2e8,
              complete = TRUE)
NNCbbNEU <- as.numeric(NNCZZ1$IRF[,1])


NNCZZ2 <- IRF(y = log(NNC20$No3_mg.l), 
              x = log(NNC20$Q_m3.s),
              m = 24,
              nk = 0,
              FD = TRUE,
              h = NULL,
              nu = 0.01,
              ARprob = 0.05,
              ARlim = 0.2,
              max.AR = 10,
              robust = TRUE,
              verbose=TRUE,
              max.chunk = 2e8,
              complete = TRUE)
NNCbbNEU1 <- as.numeric(NNCZZ2$IRF[,1])

#### extract plotting parameters from IRF outputs
NNCIRF <- as.numeric(NNCZZ$IRF[,1])
NNCSE <- as.numeric(NNCZZ$se[,1])
NNCLags <- as.numeric(NNCZZ$lags)
NNCIRFPos <- NNCIRF+2*NNCSE
NNCIRFNeg <- NNCIRF-2*NNCSE
NNClisttodf <- data.frame(NNCIRF, NNCSE, NNCLags, NNCIRFPos, NNCIRFNeg)
#/////////////////////////
NNCIRF1 <- as.numeric(NNCZZ1$IRF[,1])
NNCSE1 <- as.numeric(NNCZZ1$se[,1])
NNCLags1 <- as.numeric(NNCZZ1$lags)
NNCIRFPos1 <- NNCIRF1+2*NNCSE1
NNCIRFNeg1 <- NNCIRF1-2*NNCSE1
NNClisttodf1 <- data.frame(NNCIRF1, NNCSE1, NNCLags1, NNCIRFPos1, NNCIRFNeg1)
#/////////////////////////
NNCIRF2 <- as.numeric(NNCZZ2$IRF[,1])
NNCSE2 <- as.numeric(NNCZZ2$se[,1])
NNCLags2 <- as.numeric(NNCZZ2$lags)
NNCIRFPos2 <- NNCIRF2+2*NNCSE2
NNCIRFNeg2 <- NNCIRF2-2*NNCSE2
NNClisttodf2 <- data.frame(NNCIRF2, NNCSE2, NNCLags2, NNCIRFPos2, NNCIRFNeg2)
#/////////////////////////
#/////////////////////////
NNCIRFggplot <- ggplot(NNClisttodf, aes(x = NNCLags, y = NULL))+
  geom_ribbon(aes(ymin = NNCIRFNeg, ymax = NNCIRFPos, color = "#82D5FF"), fill = "#FF8F8F", alpha = 0.5)+
  geom_ribbon(aes(ymin = NNCIRFNeg1, ymax = NNCIRFPos1, color = "#FF8F8F"), fill = "#82D5FF", alpha = 0.5)+
  geom_ribbon(aes(ymin = NNCIRFNeg2, ymax = NNCIRFPos2, color = "#45E090"), fill = "#45E090", alpha = 0.5)+
  geom_line(data = NNClisttodf, aes(y = NNCIRF), size = 1, linetype = 3, colour = "grey20")+
  geom_line(data = NNClisttodf1, aes(y = NNCIRF1), size = 1.2, linetype = "solid")+
  geom_line(data = NNClisttodf2, aes(y = NNCIRF2), size = 0.8, linetype = 3)+
  theme_bw()+
  theme(legend.position = "")+
  labs(title = "(C)",
       x = expression("lags (hours)"),
       y = expression(NO[3]-N))+
  theme(plot.title = element_text(size=14))+
  theme(axis.title.y = element_text(size=9))+
  theme(axis.title.x = element_text(size=9))+
  scale_y_continuous(labels = scales::label_scientific(digits = 2, scale = 1, decimal.mark = "."))
NNCIRFggplot

#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#                       SBH study area IRFs                            #
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
SBHZZ <- IRF(y = log(SBHnormal$No3_mg.l), 
             x = log(SBHnormal$Q_m3.s),
             m = 24,
             nk = 0,
             FD = TRUE,
             h = NULL,
             nu = 0.1,
             ARprob = 0.05,
             ARlim = 0.2,
             max.AR = 10,
             robust = TRUE,
             verbose=TRUE,
             max.chunk = 2e8,
             complete = TRUE)
SBHbb2 <- as.numeric(SBHZZ$IRF[,1])


SBHZZ1 <- IRF(y = log(SBH80$No3_mg.l), 
              x = log(SBH80$Q_m3.s),
              m = 24,
              nk = 0,
              FD = TRUE,
              h = NULL,
              nu = 0.1,
              ARprob = 0.05,
              ARlim = 0.2,
              max.AR = 10,
              robust = TRUE,
              verbose=TRUE,
              max.chunk = 2e8,
              complete = TRUE)
SBHbbNEU <- as.numeric(SBHZZ1$IRF[,1])


SBHZZ2 <- IRF(y = log(SBH20$No3_mg.l), 
              x = log(SBH20$Q_m3.s),
              m = 24,
              nk = 0,
              FD = TRUE,
              h = NULL,
              nu = 0.1,
              ARprob = 0.05,
              ARlim = 0.2,
              max.AR = 10,
              robust = TRUE,
              verbose=TRUE,
              max.chunk = 2e8,
              complete = TRUE)
SBHbbNEU1 <- as.numeric(SBHZZ2$IRF[,1])

#### extract plotting parameters from IRF outputs
SBHIRF <- as.numeric(SBHZZ$IRF[,1])
SBHSE <- as.numeric(SBHZZ$se[,1])
SBHLags <- as.numeric(SBHZZ$lags)
SBHIRFPos <- SBHIRF+2*SBHSE
SBHIRFNeg <- SBHIRF-2*SBHSE
SBHlisttodf <- data.frame(SBHIRF, SBHSE, SBHLags, SBHIRFPos, SBHIRFNeg)
#/////////////////////////
SBHIRF1 <- as.numeric(SBHZZ1$IRF[,1])
SBHSE1 <- as.numeric(SBHZZ1$se[,1])
SBHLags1 <- as.numeric(SBHZZ1$lags)
SBHIRFPos1 <- SBHIRF1+2*SBHSE1
SBHIRFNeg1 <- SBHIRF1-2*SBHSE1
SBHlisttodf1 <- data.frame(SBHIRF1, SBHSE1, SBHLags1, SBHIRFPos1, SBHIRFNeg1)
#/////////////////////////
SBHIRF2 <- as.numeric(SBHZZ2$IRF[,1])
SBHSE2 <- as.numeric(SBHZZ2$se[,1])
SBHLags2 <- as.numeric(SBHZZ2$lags)
SBHIRFPos2 <- SBHIRF2+2*SBHSE2
SBHIRFNeg2 <- SBHIRF2-2*SBHSE2
SBHlisttodf2 <- data.frame(SBHIRF2, SBHSE2, SBHLags2, SBHIRFPos2, SBHIRFNeg2)
#/////////////////////////
SBHIRFggplot <- ggplot(SBHlisttodf, aes(x = SBHLags, y = NULL))+
  geom_ribbon(aes(ymin = SBHIRFNeg, ymax = SBHIRFPos, color = "#82D5FF"), fill = "#FF8F8F", alpha = 0.5)+
  geom_ribbon(aes(ymin = SBHIRFNeg1, ymax = SBHIRFPos1, color = "#FF8F8F"), fill = "#82D5FF", alpha = 0.5)+
  geom_ribbon(aes(ymin = SBHIRFNeg2, ymax = SBHIRFPos2, color = "#45E090"), fill = "#45E090", alpha = 0.5)+
  geom_line(data = SBHlisttodf, aes(y = SBHIRF), size = 1, linetype = 3, colour = "grey20")+
  geom_line(data = SBHlisttodf1, aes(y = SBHIRF1), size = 1.2, linetype = "solid")+
  geom_line(data = SBHlisttodf2, aes(y = SBHIRF2), size = 0.8, linetype = 3)+
  theme_bw()+
  theme(legend.position = "")+
  labs(title = "(D)",
       x = expression("lags (hours)"),
       y = expression(NO[3]-N))+
  theme(plot.title = element_text(size=14))+
  theme(axis.title.y = element_text(size=9))+
  theme(axis.title.x = element_text(size=9))
SBHIRFggplot

#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#                       SMC study area IRFs                            #
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
SMCZZ <- IRF(y = log(SMCnormal$No3_mg.l), 
             x = log(SMCnormal$Q_cfs), 
             m = 24,
             nk = 0,
             FD = TRUE,
             h = NULL,
             nu = 0.3,
             ARprob = 0.05,
             ARlim = 0.2,
             max.AR = 10,
             robust = TRUE,
             verbose=TRUE,
             max.chunk = 2e8,
             complete = TRUE)
SMCbb <- as.numeric(SMCZZ$IRF[,1])

SMCZZ1 <- IRF(y = log(SMC80$No3_mg.l), 
              x = log(SMC80$Q_cfs), 
              m = 24,
              nk = 0,
              FD = TRUE,
              h = NULL,
              nu = 0.3,
              ARprob = 0.05,
              ARlim = 0.2,
              max.AR = 10,
              robust = TRUE,
              verbose=TRUE,
              max.chunk = 2e8,
              complete = TRUE)
SMCbbNEU <- as.numeric(SMCZZ1$IRF[,1])

SMCZZ2 <- IRF(y = log(SMC20$No3_mg.l), 
              x = log(SMC20$Q_cfs), 
              m = 24,
              nk = 0,
              FD = TRUE,
              h = NULL,
              nu = 0.3,
              ARprob = 0.05,
              ARlim = 0.2,
              max.AR = 10,
              robust = TRUE,
              verbose=TRUE,
              max.chunk = 2e8,
              complete = TRUE)
SMCbbNEU1 <- as.numeric(SMCZZ2$IRF[,1])

#### extract plotting parameters from IRF outputs
SMCIRF <- as.numeric(SMCZZ$IRF[,1])
SMCSE <- as.numeric(SMCZZ$se[,1])
SMCLags <- as.numeric(SMCZZ$lags)
SMCIRFPos <- SMCIRF+2*SMCSE
SMCIRFNeg <- SMCIRF-2*SMCSE
SMClisttodf <- data.frame(SMCIRF, SMCSE, SMCLags, SMCIRFPos, SMCIRFNeg)
#/////////////////////////
SMCIRF1 <- as.numeric(SMCZZ1$IRF[,1])
SMCSE1 <- as.numeric(SMCZZ1$se[,1])
SMCLags1 <- as.numeric(SMCZZ1$lags)
SMCIRFPos1 <- SMCIRF1+2*SMCSE1
SMCIRFNeg1 <- SMCIRF1-2*SMCSE1
SMClisttodf1 <- data.frame(SMCIRF1, SMCSE1, SMCLags1, SMCIRFPos1, SMCIRFNeg1)
#/////////////////////////
SMCIRF2 <- as.numeric(SMCZZ2$IRF[,1])
SMCSE2 <- as.numeric(SMCZZ2$se[,1])
SMCLags2 <- as.numeric(SMCZZ2$lags)
SMCIRFPos2 <- SMCIRF2+2*SMCSE2
SMCIRFNeg2 <- SMCIRF2-2*SMCSE2
SMClisttodf2 <- data.frame(SMCIRF2, SMCSE2, SMCLags2, SMCIRFPos2, SMCIRFNeg2)
#/////////////////////////
SMCIRFggplot <- ggplot(SMClisttodf, aes(x = SMCLags, y = NULL))+
  geom_ribbon(aes(ymin = SMCIRFNeg, ymax = SMCIRFPos, color = "#82D5FF"), fill = "#FF8F8F", alpha = 0.5)+
  geom_ribbon(aes(ymin = SMCIRFNeg1, ymax = SMCIRFPos1, color = "#FF8F8F"), fill = "#82D5FF", alpha = 0.5)+
  geom_ribbon(aes(ymin = SMCIRFNeg2, ymax = SMCIRFPos2, color = "#45E090"), fill = "#45E090", alpha = 0.5)+
  geom_line(data = SMClisttodf, aes(y = SMCIRF), size = 1, linetype = 3, colour = "grey20")+
  geom_line(data = SMClisttodf1, aes(y = SMCIRF1), size = 1.2, linetype = "solid")+
  geom_line(data = SMClisttodf2, aes(y = SMCIRF2), size = 0.8, linetype = 3)+
  theme_bw()+
  theme(legend.position = "")+
  labs(title = "(E)",
       x = expression("lags (hours)"),
       y = expression(NO[3]-N))+
  theme(plot.title = element_text(size=14))+
  theme(axis.title.y = element_text(size=9))+
  theme(axis.title.x = element_text(size=9))
SMCIRFggplot
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#                       TMR study area IRFs                            #
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
TMRZZ <- IRF(y = log(TMRnormal$No3_mg.l), 
             x = log(TMRnormal$Q_cfs), 
             m = 24,
             nk = 0,
             FD = TRUE,
             h = NULL,
             nu = 0.1,
             ARprob = 0.05,
             ARlim = 0.2,
             max.AR = 10,
             robust = TRUE,
             verbose=TRUE,
             max.chunk = 2e8,
             complete = TRUE)
TMRbb2 <- as.numeric(TMRZZ$IRF[,1])

TMRZZ1 <- IRF(y = log(TMR80$No3_mg.l), 
              x = log(TMR80$Q_cfs), 
              m = 24,
              nk = 0,
              FD = TRUE,
              h = NULL,
              nu = 0.1,
              ARprob = 0.05,
              ARlim = 0.2,
              max.AR = 10,
              robust = TRUE,
              verbose=TRUE,
              max.chunk = 2e8,
              complete = TRUE)
TMRbbNEU <- as.numeric(TMRZZ1$IRF[,1])

TMRZZ2 <- IRF(y = log(TMR20$No3_mg.l), 
              x = log(TMR20$Q_cfs), 
              m = 24,
              nk = 0,
              FD = TRUE,
              h = NULL,
              nu = 0.1,
              ARprob = 0.05,
              ARlim = 0.2,
              max.AR = 10,
              robust = TRUE,
              verbose=TRUE,
              max.chunk = 2e8,
              complete = TRUE)
TMRbbNEU1 <- as.numeric(TMRZZ2$IRF[,1])

#### extract plotting parameters from IRF outputs
TMRIRF <- as.numeric(TMRZZ$IRF[,1])
TMRSE <- as.numeric(TMRZZ$se[,1])
TMRLags <- as.numeric(TMRZZ$lags)
TMRIRFPos <- TMRIRF+2*TMRSE
TMRIRFNeg <- TMRIRF-2*TMRSE
TMRlisttodf <- data.frame(TMRIRF, TMRSE, TMRLags, TMRIRFPos, TMRIRFNeg)
#/////////////////////////
TMRIRF1 <- as.numeric(TMRZZ1$IRF[,1])
TMRSE1 <- as.numeric(TMRZZ1$se[,1])
TMRLags1 <- as.numeric(TMRZZ1$lags)
TMRIRFPos1 <- TMRIRF1+2*TMRSE1
TMRIRFNeg1 <- TMRIRF1-2*TMRSE1
TMRlisttodf1 <- data.frame(TMRIRF1, TMRSE1, TMRLags1, TMRIRFPos1, TMRIRFNeg1)
#/////////////////////////
TMRIRF2 <- as.numeric(TMRZZ2$IRF[,1])
TMRSE2 <- as.numeric(TMRZZ2$se[,1])
TMRLags2 <- as.numeric(TMRZZ2$lags)
TMRIRFPos2 <- TMRIRF2+2*TMRSE2
TMRIRFNeg2 <- TMRIRF2-2*TMRSE2
TMRlisttodf2 <- data.frame(TMRIRF2, TMRSE2, TMRLags2, TMRIRFPos2, TMRIRFNeg2)
#/////////////////////////
TMRIRFggplot <- ggplot(TMRlisttodf, aes(x = TMRLags, y = NULL))+
  geom_ribbon(aes(ymin = TMRIRFNeg, ymax = TMRIRFPos, fill = "#FF8F8F", color = "#82D5FF"), alpha = 0.5)+
  geom_ribbon(aes(ymin = TMRIRFNeg1, ymax = TMRIRFPos1, fill = "#82D5FF", color =  "#FF8F8F"), alpha = 0.5)+
  geom_ribbon(aes(ymin = TMRIRFNeg2, ymax = TMRIRFPos2, fill = "#45E090"), color = "#45E090", alpha = 0.5)+
  geom_line(data = TMRlisttodf, aes(y = TMRIRF), size = 1, linetype = 3, colour = "grey20")+
  geom_line(data = TMRlisttodf1, aes(y = TMRIRF1), size = 1.2, linetype = "solid")+
  geom_line(data = TMRlisttodf2, aes(y = TMRIRF2), size = 0.8, linetype = 3)+
  scale_fill_identity(guide = "legend", name = "Flow Conditions", breaks = c("#FF8F8F", "#82D5FF", "#45E090"), labels = c("Qnorm", "Q80", "Q20"))+
  theme_bw()+
  theme(legend.position = c(0.5, -0.6))+
  labs(title = "(F)",
       x = expression("lags (hours)"),
       y = expression(NO[3]-N))+
  theme(plot.title = element_text(size=14))+
  theme(axis.title.y = element_text(size=9))+
  theme(axis.title.x = element_text(size=9))+
  guides(color = "none")
TMRIRFggplot
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#                       WEB study area IRFs                            #
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
WEBZZ <- IRF(y = log(WEBnormal$No3_mg.l), 
             x = log(WEBnormal$Q_l.s), 
             m = 24,
             nk = 0,
             FD = TRUE,
             h = NULL,
             nu = 0.6,
             ARprob = 0.05,
             ARlim = 0.2,
             max.AR = 12,
             robust = TRUE,
             verbose=TRUE,
             max.chunk = 2e8,
             complete = FALSE)
WEBbb2 <- as.numeric(WEBZZ$IRF[,1])

WEB80$Q_l.s[WEB80$Q_l.s == 0] <- NA
WEBZZ1 <- IRF(y = log(WEB80$No3_mg.l), 
              x = log(WEB80$Q_l.s), 
              m = 24,
              nk = 0,
              FD = TRUE,
              h = NULL,
              nu = 0.6,
              ARprob = 0.05,
              ARlim = 0.2,
              max.AR = 12,
              robust = TRUE,
              verbose=TRUE,
              max.chunk = 2e8,
              complete = FALSE)
WEBbbNEU <- as.numeric(WEBZZ1$IRF[,1])

WEBZZ2 <- IRF(y = log(WEB20$No3_mg.l), 
              x = log(WEB20$Q_l.s), 
              m = 24,
              nk = 0,
              FD = TRUE,
              h = NULL,
              nu = 0.6,
              ARprob = 0.05,
              ARlim = 0.2,
              max.AR = 12,
              robust = TRUE,
              verbose=TRUE,
              max.chunk = 2e8,
              complete = FALSE)
WEBbbNEU1 <- as.numeric(WEBZZ2$IRF[,1])

#### extract plotting parameters from IRF outputs
WEBIRF <- as.numeric(WEBZZ$IRF[,1])
WEBSE <- as.numeric(WEBZZ$se[,1])
WEBLags <- as.numeric(WEBZZ$lags)
WEBIRFPos <- WEBIRF+2*WEBSE
WEBIRFNeg <- WEBIRF-2*WEBSE
WEBlisttodf <- data.frame(WEBIRF, WEBSE, WEBLags, WEBIRFPos, WEBIRFNeg)
#/////////////////////////
WEBIRF1 <- as.numeric(WEBZZ1$IRF[,1])
WEBSE1 <- as.numeric(WEBZZ1$se[,1])
WEBLags1 <- as.numeric(WEBZZ1$lags)
WEBIRFPos1 <- WEBIRF1+2*WEBSE1
WEBIRFNeg1 <- WEBIRF1-2*WEBSE1
WEBlisttodf1 <- data.frame(WEBIRF1, WEBSE1, WEBLags1, WEBIRFPos1, WEBIRFNeg1)
#/////////////////////////
WEBIRF2 <- as.numeric(WEBZZ2$IRF[,1])
WEBSE2 <- as.numeric(WEBZZ2$se[,1])
WEBLags2 <- as.numeric(WEBZZ2$lags)
WEBIRFPos2 <- WEBIRF2+2*WEBSE2
WEBIRFNeg2 <- WEBIRF2-2*WEBSE2
WEBlisttodf2 <- data.frame(WEBIRF2, WEBSE2, WEBLags2, WEBIRFPos2, WEBIRFNeg2)
#/////////////////////////
WEBIRFggplot <- ggplot(WEBlisttodf, aes(x = WEBLags, y = NULL))+
  geom_ribbon(aes(ymin = WEBIRFNeg, ymax = WEBIRFPos, color = "#82D5FF"), fill = "#FF8F8F", alpha = 0.5)+
  geom_ribbon(aes(ymin = WEBIRFNeg1, ymax = WEBIRFPos1, color =  "#FF8F8F"), fill = "#82D5FF", alpha = 0.5)+
  geom_ribbon(aes(ymin = WEBIRFNeg2, ymax = WEBIRFPos2, color = "#45E090"), fill = "#45E090", alpha = 0.5)+
  geom_line(data = WEBlisttodf, aes(y = WEBIRF), size = 1, linetype = 3, colour = "grey20")+
  geom_line(data = WEBlisttodf1, aes(y = WEBIRF1), size = 1.2, linetype = "solid")+
  geom_line(data = WEBlisttodf2, aes(y = WEBIRF2), size = 0.8, linetype = 3)+
  theme_bw()+
  theme(legend.position = "")+
  labs(title = "(G)",
       x = expression("lags (hours)"),
       y = expression(NO[3]-N))+
  theme(plot.title = element_text(size=14))+
  theme(axis.title.y = element_text(size=9))+
  theme(axis.title.x = element_text(size=9))
WEBIRFggplot

#///////////////////////////////////////////////////////////////////////////
# plot final IRFs (FIGURE 27)
#///////////////////////////////////////////////////////////////////////////
FinalIRFs <- grid.arrange(ACCIRFggplot, HNDIRFggplot, NNCIRFggplot, SBHIRFggplot, SMCIRFggplot, TMRIRFggplot, WEBIRFggplot, 
                          ncol =2, 
                          top = textGrob("", gp=gpar(fontsize=14)))

#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
###                    Time Series decomposition                       ###
###                            Appendix I                              ###
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
# the descomponer plugin gives us a data frame to create our own plots. 
# This way we can easily combine N and Q decompositions in one plot to compare trends
# Create decompositions for all data sets (using daily data to avoid cluttered plots). 
# I use yearly seasonality because that is the natural and dominant frequency of the data.
# multi seasonal compositions (mstl) earlier revealed little significance of diurnal, monthly etc. seasonalities
# decompositions for Q
decomposeACC <- descomponer(ACCD$Q_m3.s, 365, 2)
decomposeHND <- descomponer(HNDD$Q_m3.s, 365, 2)
decomposeNNC <- descomponer(NNCD$Q_m3.s, 365, 2)
decomposeSBH <- descomponer(SBHD$Q_m3.s, 365, 2)
decomposeSMC <- descomponer(SMCD$Q_cfs, 365, 2)
decomposeTMR <- descomponer(TMRD$Q_cfs*0.028316846592, 365, 2) #to m³/s to scale better with NO3
decomposeWEB <- descomponer(WEBD$Q_m3.s*0.035314666721489, 365, 2) #to cfs to scale better with NO3

# now the same for N
decomposeACCN <- descomponer(ACCD$No3_mg.l, 365, 2)
decomposeHNDN <- descomponer(HNDD$No3_mg.l, 365, 2)
decomposeNNCN <- descomponer(NNCD$No3_mg.l, 365, 2)
decomposeSBHN <- descomponer(SBHD$No3_mg.l, 365, 2)
decomposeSMCN <- descomponer(SMCD$No3_mg.l, 365, 2)
decomposeTMRN <- descomponer(TMRD$No3_mg.l, 365, 2)
decomposeWEBN <- descomponer(WEBD$No3_mg.l, 365, 2)

# quick plots for sanity check
ACCdcmp <- plot(ts(decomposeACC$datos, frequency = 365))
HNDdcmp <- plot(ts(decomposeHND$datos, frequency = 365))
NNCdcmp <- plot(ts(decomposeNNC$datos, frequency = 365))
SBHdcmp <- plot(ts(decomposeSBH$datos, frequency = 365))
SMCdcmp <- plot(ts(decomposeSMC$datos, frequency = 365))
TMRdcmp <- plot(ts(decomposeTMR$datos, frequency = 365))
WEBdcmp <- plot(ts(decomposeWEB$datos, frequency = 365))

ACCdcmpN <- plot(ts(decomposeACCN$datos, frequency = 365))
HNDdcmpN <- plot(ts(decomposeHNDN$datos, frequency = 365))
NNCdcmpN <- plot(ts(decomposeNNCN$datos, frequency = 365))
SBHdcmpN <- plot(ts(decomposeSBHN$datos, frequency = 365))
SMCdcmpN <- plot(ts(decomposeSMCN$datos, frequency = 365))
TMRdcmpN <- plot(ts(decomposeTMRN$datos, frequency = 365))
WEBdcmpN <- plot(ts(decomposeWEBN$datos, frequency = 365))

# nicer plots with ggplot2
# create a data frame for Q
ACCY <- as.data.frame(decomposeACC$datos[,1:5])
ACCdcmp1 <- cbind(ACCD$datetime, ACCY)
colnames(ACCdcmp1) <-  c("datetime", "Series", "Trend+Seasonal", "Trend", "Seasonal(Yearly)", "Remainder")

# create a data frame for N
ACCYn <- as.data.frame(decomposeACCN$datos[,1:5])
ACCdcmp1n <- cbind(ACCD$datetime, ACCYn)
colnames(ACCdcmp1n) <-  c("datetime", "Series", "Trend+Seasonal", "Trend", "Seasonal(Yearly)", "Remainder")

# combined plots for Q and N decompositions (ACC)
ACCseries <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = ACCdcmp1, aes(datetime, Series), ma_fun = SMA, n = 28,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = ACCdcmp1n, aes(datetime, Series), ma_fun = SMA, n = 28, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 28 (days)", size = 3) 
ACCseries

ACCtrd <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = ACCdcmp1, aes(datetime, Trend), ma_fun = SMA, n = 28,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = ACCdcmp1n, aes(datetime, Trend), ma_fun = SMA, n = 28, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 28 (days)", size = 3) 
ACCtrd

ACCseas <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = ACCdcmp1, aes(datetime, `Seasonal(Yearly)`), ma_fun = SMA, n = 28,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = ACCdcmp1n, aes(datetime, `Seasonal(Yearly)`), ma_fun = SMA, n = 28, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 28 (days)", size = 3) 
ACCseas

ACCtrdsrs <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = ACCdcmp1, aes(datetime, `Trend+Seasonal`), ma_fun = SMA, n = 28,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = ACCdcmp1n, aes(datetime, `Trend+Seasonal`), ma_fun = SMA, n = 28, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 28 (days)", size = 3) 
ACCtrdsrs

ACCrem <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = ACCdcmp1, aes(datetime, Remainder), ma_fun = SMA, n = 28,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = ACCdcmp1n, aes(datetime, Remainder), ma_fun = SMA, n = 28, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 28 (days)", size = 3) 
ACCrem
ACCdcmpfin <- grid.arrange(ACCseries, ACCtrd, ACCseas, ACCtrdsrs, ACCrem, ncol = 1)

# Now the same for all study sites starting with HND
HNDY <- as.data.frame(decomposeHND$datos[,1:5])
HNDdcmp1 <- cbind(HNDD$datetime, HNDY)
colnames(HNDdcmp1) <-  c("datetime", "Series", "Trend+Seasonal", "Trend", "Seasonal(Yearly)", "Remainder")

HNDYn <- as.data.frame(decomposeHNDN$datos[,1:5])
HNDdcmp1n <- cbind(HNDD$datetime, HNDYn)
colnames(HNDdcmp1n) <-  c("datetime", "Series", "Trend+Seasonal", "Trend", "Seasonal(Yearly)", "Remainder")

HNDseries <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = HNDdcmp1, aes(datetime, Series), ma_fun = SMA, n = 28,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = HNDdcmp1n, aes(datetime, Series), ma_fun = SMA, n = 28, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 28 (days)", size = 3)
HNDseries

HNDtrd <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = HNDdcmp1, aes(datetime, Trend), ma_fun = SMA, n = 28,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = HNDdcmp1n, aes(datetime, Trend), ma_fun = SMA, n = 28, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 28 (days)", size = 3)
HNDtrd

HNDseas <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = HNDdcmp1, aes(datetime, `Seasonal(Yearly)`), ma_fun = SMA, n = 28,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = HNDdcmp1n, aes(datetime, `Seasonal(Yearly)`), ma_fun = SMA, n = 28, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 28 (days)", size = 3)
HNDseas

HNDtrdsrs <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = HNDdcmp1, aes(datetime, `Trend+Seasonal`), ma_fun = SMA, n = 28,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = HNDdcmp1n, aes(datetime, `Trend+Seasonal`), ma_fun = SMA, n = 28, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 28 (days)", size = 3)
HNDtrdsrs

HNDrem <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = HNDdcmp1, aes(datetime, Remainder), ma_fun = SMA, n = 28,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = HNDdcmp1n, aes(datetime, Remainder), ma_fun = SMA, n = 28, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 28 (days)", size = 3)
HNDrem
HNDdcmpfin <- grid.arrange(HNDseries, HNDtrd, HNDseas, HNDtrdsrs, HNDrem, ncol = 1)

# NNC
NNCY <- as.data.frame(decomposeNNC$datos[,1:5])
NNCdcmp1 <- cbind(NNCD$datetime, NNCY)
colnames(NNCdcmp1) <-  c("datetime", "Series", "Trend+Seasonal", "Trend", "Seasonal(Yearly)", "Remainder")

NNCYn <- as.data.frame(decomposeNNCN$datos[,1:5])
NNCdcmp1n <- cbind(NNCD$datetime, NNCYn)
colnames(NNCdcmp1n) <-  c("datetime", "Series", "Trend+Seasonal", "Trend", "Seasonal(Yearly)", "Remainder")

NNCseries <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = NNCdcmp1, aes(datetime, Series), ma_fun = SMA, n = 7,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = NNCdcmp1n, aes(datetime, Series), ma_fun = SMA, n = 7, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 7 (days)", size = 3)
NNCseries

NNCtrd <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = NNCdcmp1, aes(datetime, Trend), ma_fun = SMA, n = 7,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = NNCdcmp1n, aes(datetime, Trend), ma_fun = SMA, n = 7, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 7 (days)", size = 3)
NNCtrd

NNCseas <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = NNCdcmp1, aes(datetime, `Seasonal(Yearly)`), ma_fun = SMA, n = 7,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = NNCdcmp1n, aes(datetime, `Seasonal(Yearly)`), ma_fun = SMA, n = 7, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 7 (days)", size = 3)
NNCseas

NNCtrdsrs <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = NNCdcmp1, aes(datetime, `Trend+Seasonal`), ma_fun = SMA, n = 7,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = NNCdcmp1n, aes(datetime, `Trend+Seasonal`), ma_fun = SMA, n = 7, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 7 (days)", size = 3)
NNCtrdsrs

NNCrem <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = NNCdcmp1, aes(datetime, Remainder), ma_fun = SMA, n = 7,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = NNCdcmp1n, aes(datetime, Remainder), ma_fun = SMA, n = 7, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 7 (days)", size = 3)
NNCrem
NNCdcmpfin <- grid.arrange(NNCseries, NNCtrd, NNCseas, NNCtrdsrs, NNCrem, ncol = 1)

# SBH
SBHY <- as.data.frame(decomposeSBH$datos[,1:5])
SBHdcmp1 <- cbind(SBHD$datetime, SBHY)
colnames(SBHdcmp1) <-  c("datetime", "Series", "Trend+Seasonal", "Trend", "Seasonal(Yearly)", "Remainder")

SBHYn <- as.data.frame(decomposeSBHN$datos[,1:5])
SBHdcmp1n <- cbind(SBHD$datetime, SBHYn)
colnames(SBHdcmp1n) <-  c("datetime", "Series", "Trend+Seasonal", "Trend", "Seasonal(Yearly)", "Remainder")

SBHseries <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = SBHdcmp1, aes(datetime, Series), ma_fun = SMA, n = 28,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = SBHdcmp1n, aes(datetime, Series), ma_fun = SMA, n = 28, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 28 (days)", size = 3)
SBHseries

SBHtrd <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = SBHdcmp1, aes(datetime, Trend), ma_fun = SMA, n = 28,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = SBHdcmp1n, aes(datetime, Trend), ma_fun = SMA, n = 28, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 28 (days)", size = 3)
SBHtrd

SBHseas <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = SBHdcmp1, aes(datetime, `Seasonal(Yearly)`), ma_fun = SMA, n = 28,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = SBHdcmp1n, aes(datetime, `Seasonal(Yearly)`), ma_fun = SMA, n = 28, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 28 (days)", size = 3)
SBHseas

SBHtrdsrs <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = SBHdcmp1, aes(datetime, `Trend+Seasonal`), ma_fun = SMA, n = 28,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = SBHdcmp1n, aes(datetime, `Trend+Seasonal`), ma_fun = SMA, n = 28, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 28 (days)", size = 3)
SBHtrdsrs

SBHrem <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = SBHdcmp1, aes(datetime, Remainder), ma_fun = SMA, n = 28,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = SBHdcmp1n, aes(datetime, Remainder), ma_fun = SMA, n = 28, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 28 (days)", size = 3)
SBHrem
SBHdcmpfin <- grid.arrange(SBHseries, SBHtrd, SBHseas, SBHtrdsrs, SBHrem, ncol = 1)

# SMC
SMCY <- as.data.frame(decomposeSMC$datos[,1:5])
SMCdcmp1 <- cbind(SMCD$datetime, SMCY)
colnames(SMCdcmp1) <-  c("datetime", "Series", "Trend+Seasonal", "Trend", "Seasonal(Yearly)", "Remainder")

SMCYn <- as.data.frame(decomposeSMCN$datos[,1:5])
SMCdcmp1n <- cbind(SMCD$datetime, SMCYn)
colnames(SMCdcmp1n) <-  c("datetime", "Series", "Trend+Seasonal", "Trend", "Seasonal(Yearly)", "Remainder")

SMCseries <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = SMCdcmp1, aes(datetime, Series), ma_fun = SMA, n = 28,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = SMCdcmp1n, aes(datetime, Series), ma_fun = SMA, n = 28, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 28 (days)", size = 3)
SMCseries

SMCtrd <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = SMCdcmp1, aes(datetime, Trend), ma_fun = SMA, n = 28,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = SMCdcmp1n, aes(datetime, Trend), ma_fun = SMA, n = 28, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 28 (days)", size = 3)
SMCtrd

SMCseas <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = SMCdcmp1, aes(datetime, `Seasonal(Yearly)`), ma_fun = SMA, n = 28,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = SMCdcmp1n, aes(datetime, `Seasonal(Yearly)`), ma_fun = SMA, n = 28, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 28 (days)", size = 3)
SMCseas

SMCtrdsrs <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = SMCdcmp1, aes(datetime, `Trend+Seasonal`), ma_fun = SMA, n = 28,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = SMCdcmp1n, aes(datetime, `Trend+Seasonal`), ma_fun = SMA, n = 28, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 28 (days)", size = 3)
SMCtrdsrs

SMCrem <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = SMCdcmp1, aes(datetime, Remainder), ma_fun = SMA, n = 28,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = SMCdcmp1n, aes(datetime, Remainder), ma_fun = SMA, n = 28, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 28 (days)", size = 3)
SMCrem
SMCdcmpfin <- grid.arrange(SMCseries, SMCtrd, SMCseas, SMCtrdsrs, SMCrem, ncol = 1)

# TMR
TMRY <- as.data.frame(decomposeTMR$datos[,1:5])
TMRdcmp1 <- cbind(TMRD$datetime, TMRY)
colnames(TMRdcmp1) <-  c("datetime", "Series", "Trend+Seasonal", "Trend", "Seasonal(Yearly)", "Remainder")

TMRYn <- as.data.frame(decomposeTMRN$datos[,1:5])
TMRdcmp1n <- cbind(TMRD$datetime, TMRYn)
colnames(TMRdcmp1n) <-  c("datetime", "Series", "Trend+Seasonal", "Trend", "Seasonal(Yearly)", "Remainder")

TMRseries <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = TMRdcmp1, aes(datetime, Series), ma_fun = SMA, n = 14,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = TMRdcmp1n, aes(datetime, Series), ma_fun = SMA, n = 14, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 14 (days)", size = 3)
TMRseries

TMRtrd <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = TMRdcmp1, aes(datetime, Trend), ma_fun = SMA, n = 14,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = TMRdcmp1n, aes(datetime, Trend), ma_fun = SMA, n = 14, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 14 (days)", size = 3)
TMRtrd

TMRseas <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = TMRdcmp1, aes(datetime, `Seasonal(Yearly)`), ma_fun = SMA, n = 14,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = TMRdcmp1n, aes(datetime, `Seasonal(Yearly)`), ma_fun = SMA, n = 14, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 14 (days)", size = 3)
TMRseas

TMRtrdsrs <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = TMRdcmp1, aes(datetime, `Trend+Seasonal`), ma_fun = SMA, n = 14,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = TMRdcmp1n, aes(datetime, `Trend+Seasonal`), ma_fun = SMA, n = 14, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 14 (days)", size = 3)
TMRtrdsrs

TMRrem <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = TMRdcmp1, aes(datetime, Remainder), ma_fun = SMA, n = 14,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = TMRdcmp1n, aes(datetime, Remainder), ma_fun = SMA, n = 14, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 14 (days)", size = 3)
TMRrem
TMRdcmpfin <- grid.arrange(TMRseries, TMRtrd, TMRseas, TMRtrdsrs, TMRrem, ncol = 1)

# WEB
WEBY <- as.data.frame(decomposeWEB$datos[,1:5])
WEBdcmp1 <- cbind(WEBD$datetime, WEBY)
colnames(WEBdcmp1) <-  c("datetime", "Series", "Trend+Seasonal", "Trend", "Seasonal(Yearly)", "Remainder")

WEBYn <- as.data.frame(decomposeWEBN$datos[,1:5])
WEBdcmp1n <- cbind(WEBD$datetime, WEBYn)
colnames(WEBdcmp1n) <-  c("datetime", "Series", "Trend+Seasonal", "Trend", "Seasonal(Yearly)", "Remainder")

WEBseries <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = WEBdcmp1, aes(datetime, Series), ma_fun = SMA, n = 7,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = WEBdcmp1n, aes(datetime, Series), ma_fun = SMA, n = 7, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 7 (days)", size = 3)
WEBseries

WEBtrd <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = WEBdcmp1, aes(datetime, Trend), ma_fun = SMA, n = 7,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = WEBdcmp1n, aes(datetime, Trend), ma_fun = SMA, n = 7, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 7 (days)", size = 3)
WEBtrd

WEBseas <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = WEBdcmp1, aes(datetime, `Seasonal(Yearly)`), ma_fun = SMA, n = 7,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = WEBdcmp1n, aes(datetime, `Seasonal(Yearly)`), ma_fun = SMA, n = 7, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 7 (days)", size = 3)
WEBseas

WEBtrdsrs <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = WEBdcmp1, aes(datetime, `Trend+Seasonal`), ma_fun = SMA, n = 7,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = WEBdcmp1n, aes(datetime, `Trend+Seasonal`), ma_fun = SMA, n = 7, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 7 (days)", size = 3)
WEBtrdsrs

WEBrem <- ggplot(data = NULL, x = datetime, y = NULL)+
  geom_ma(data = WEBdcmp1, aes(datetime, Remainder), ma_fun = SMA, n = 7,  lwd = 0.8, lty = "solid", color = "#003399")+
  geom_ma(data = WEBdcmp1n, aes(datetime, Remainder), ma_fun = SMA, n = 7, lwd = 0.8, lty = "solid", color = "#00B084")+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  annotate("label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf, hjust = 0, vjust = 1, label = "SMA = 7 (days)", size = 3)
WEBrem
WEBdcmpfin <- grid.arrange(WEBseries, WEBtrd, WEBseas, WEBtrdsrs, WEBrem, ncol = 1)

DCMPACCNNC <- grid.arrange(ACCdcmpfin, NNCdcmpfin, ncol = 2)
DCMPHNDSBH <- grid.arrange(SBHdcmpfin, HNDdcmpfin, ncol = 2)
DCMPSMCTMR <- grid.arrange(SMCdcmpfin, TMRdcmpfin, ncol = 2)

#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
###                    Time Series plots Q and NO3                     ###
###                            Appendix II                             ###
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#### ACC
ACCDBreaks <- seq.POSIXt(from = lubridate::ymd_hms("20151015 230000"), 
                         to   = lubridate::ymd_hms("20210930 230000"), by = "4 months")
ACCTSFF <- ggplot(data = ACC)+
  geom_col(aes(x = datetime, y = Q_m3.s), color = "#00798c")+
  geom_col(aes(x = datetime, y = -No3_mg.l), color = "#00B084")+
  theme_bw()+
  ylab("NO3- mg/l and Q m³/s")+
  theme(axis.title.x = element_blank(), panel.grid.major.x = element_blank(), axis.title.y = element_text(size = 10))+
  geom_hline(yintercept = 0.591, na.rm = TRUE, lwd = 1, lty = 1, colour = "#000000")+
  geom_hline(yintercept = 0.124, na.rm = TRUE, lwd = 1, lty = 2, colour = "#000000")+
  scale_x_datetime(date_labels = "%m/%y", breaks = ACCDBreaks)+
  annotate("label", x = min(ACC$datetime), y = max(ACC$Q_m3.s), label = "A", size = 4)
ACCTSFF

#### HND
HNDDBreaks <- seq.POSIXt(from = lubridate::ymd_hms("20151001 230000"), 
                         to   = lubridate::ymd_hms("20200930 230000"), by = "4 months")
HNDTSFF <- ggplot(data = HND)+
  geom_col(aes(x = datetime, y = Q_m3.s), color = "#00798c")+
  geom_col(aes(x = datetime, y = -No3_mg.l), color = "#00B084")+
  theme_bw()+
  ylab("NO3- mg/l and Q m³/s")+
  theme(axis.title.x = element_blank(), panel.grid.major.x = element_blank(), axis.title.y = element_text(size = 10))+
  geom_hline(yintercept = 1.62, na.rm = TRUE, lwd = 1, lty = 1, colour = "#000000")+
  geom_hline(yintercept = 0.313, na.rm = TRUE, lwd = 1, lty = 2, colour = "#000000")+
  scale_x_datetime(date_labels = "%m/%y", breaks = HNDDBreaks)+
  annotate("label", x = min(HND$datetime), y = max(HND$Q_m3.s), label = "B", size = 4)
HNDTSFF

#### NNC
NNCDBreaks <- seq.POSIXt(from = lubridate::ymd_hms("20161001 230000"), 
                         to   = lubridate::ymd_hms("20180930 230000"), by = "3 months")
NNCTSFF <- ggplot(data = NNC)+
  geom_col(aes(x = datetime, y = Q_m3.s), color = "#00798c")+
  geom_col(aes(x = datetime, y = -No3_mg.l), color = "#00B084")+
  theme_bw()+
  ylab("NO3- mg/l and Q m³/s")+
  theme(axis.title.x = element_blank(), panel.grid.major.x = element_blank(), axis.title.y = element_text(size = 10))+
  geom_hline(yintercept = 3.015744, na.rm = TRUE, lwd = 1, lty = 1, colour = "#000000")+
  geom_hline(yintercept = 1.4031, na.rm = TRUE, lwd = 1, lty = 2, colour = "#000000")+
  scale_x_datetime(date_labels = "%m/%y", breaks = NNCDBreaks)+
  annotate("label", x = min(NNC$datetime), y = max(NNC$Q_m3.s), label = "C", size = 4)
NNCTSFF

#### SBH
SBHTSFF <- ggplot(data = SBH)+
  geom_col(aes(x = datetime, y = Q_m3.s), color = "#00798c")+
  geom_col(aes(x = datetime, y = -No3_mg.l), color = "#00B084")+
  theme_bw()+
  ylab("NO3- mg/l and Q m³/s")+
  theme(axis.title.x = element_blank(), panel.grid.major.x = element_blank(), axis.title.y = element_text(size = 10))+
  geom_hline(yintercept = 0.905, na.rm = TRUE, lwd = 1, lty = 1, colour = "#000000")+
  geom_hline(yintercept = 0.135, na.rm = TRUE, lwd = 1, lty = 2, colour = "#000000")+
  scale_x_datetime(date_labels = "%m/%y", breaks = HNDDBreaks)+
  annotate("label", x = min(SBH$datetime), y = max(SBH$Q_m3.s), label = "D", size = 4)
SBHTSFF

#### SMC
SMCTSFF <- ggplot(data = SMC)+
  geom_col(aes(x = datetime, y = Q_cfs), color = "#00798c")+
  geom_col(aes(x = datetime, y = -No3_mg.l), color = "#00B084")+
  theme_bw()+
  ylab("NO3- mg/l and Q cf/s")+
  theme(axis.title.x = element_blank(), panel.grid.major.x = element_blank(), axis.title.y = element_text(size = 10))+
  geom_hline(yintercept = 2.2, na.rm = TRUE, lwd = 1, lty = 1, colour = "#000000")+
  geom_hline(yintercept = 1.32, na.rm = TRUE, lwd = 1, lty = 2, colour = "#000000")+
  scale_x_datetime(date_labels = "%m/%y", breaks = HNDDBreaks)+
  annotate("label", x = min(SMC$datetime), y = max(SMC$Q_cfs), label = "E", size = 4)
SMCTSFF

#### TMR
TMRBreaks <- seq.POSIXt(from = lubridate::ymd_hms("20141001 230000"), 
                        to   = lubridate::ymd_hms("20170930 230000"), by = "3 months")
TMRTSFF <- ggplot(data = TMR)+
  geom_col(aes(x = datetime, y = Q_cfs*0.028316846592), color = "#00798c")+
  geom_col(aes(x = datetime, y = -No3_mg.l), color = "#00B084")+
  theme_bw()+
  ylab("NO3- mg/l and Q m³/s")+
  theme(axis.title.x = element_blank(), panel.grid.major.x = element_blank(), axis.title.y = element_text(size = 10))+
  geom_hline(yintercept = 37.625*0.028316846592, na.rm = TRUE, lwd = 1, lty = 1, colour = "#000000")+
  geom_hline(yintercept = 24.1443*0.028316846592, na.rm = TRUE, lwd = 1, lty = 2, colour = "#000000")+
  scale_x_datetime(date_labels = "%m/%y", breaks = TMRBreaks)+
  annotate("label", x = min(TMR$datetime), y = max(TMR$Q_cfs*0.028316846592), label = "F", size = 4)
TMRTSFF

#### WEB
View(WEB)
WEBBreaks <- seq.POSIXt(from = lubridate::ymd_hms("20131112 230000"), 
                        to   = lubridate::ymd_hms("20150930 230000"), by = "2 months")
WEBTSFF <- ggplot(data = WEB)+
  geom_col(aes(x = datetime, y = Q_l.s*0.035314666721489), color = "#00798c")+
  geom_col(aes(x = datetime, y = -No3_mg.l), color = "#00B084")+
  theme_bw()+
  ylab("NO3- mg/l and Q cf/s")+
  theme(axis.title.x = element_blank(), panel.grid.major.x = element_blank(), axis.title.y = element_text(size = 10))+
  geom_hline(yintercept = 10.25*0.035314666721489, na.rm = TRUE, lwd = 1, colour = "#000000", lty = 1)+
  geom_hline(yintercept = 0.6*0.035314666721489, na.rm = TRUE, lwd = 1, colour = "#000000", lty = 2)+
  scale_x_datetime(date_labels = "%m/%y", breaks = WEBBreaks)+
  annotate("label", x = min(WEB$datetime), y = max(WEB$Q_l.s*0.035314666721489), label = "G", size = 4)
WEBTSFF
        
grid.arrange(ACCTSFF, HNDTSFF, NNCTSFF, ncol = 1, nrow = 3)
grid.arrange(SBHTSFF, SMCTSFF, TMRTSFF, WEBTSFF, ncol = 1, nrow = 4)

#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
###                      Seasonal plots for RUNOFF                     ###
###                            Appendix III                            ###
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
LFOACC1RO <- as.lfobj(ACCX$RO, hyearstart = 10)
LFOHND1RO <- as.lfobj(HNDXDM$RO, hyearstart = 10)
LFONNC1RO <- as.lfobj(NNCXDM$RO, hyearstart = 10)
LFOSBH1RO <- as.lfobj(SBHXDM$RO, hyearstart = 10)
LFOSMC1RO <- as.lfobj(SMCXDM$RO, hyearstart = 10)
LFOTMR1RO <- as.lfobj(TMRXDM$RO, hyearstart = 10)
LFOWEB1RO <- as.lfobj(WEBXDM$RO, hyearstart = 10)

ACCMMRO <- meanflow(LFOACC1RO, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
HNDMMRO <- meanflow(LFOHND1RO, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
NNCMMRO <- meanflow(LFONNC1RO, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
SBHMMRO <- meanflow(LFOSBH1RO, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
SMCMMRO <- meanflow(LFOSMC1RO, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
TMRMMRO <- meanflow(LFOTMR1RO, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
WEBMMRO <- meanflow(LFOWEB1RO, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)

plot.new()
ACCMMPRO <- ggplot(ACCMMRO, aes(month, flow))+
  geom_col(width = 0.5, fill = "#FF3D3D")+
  labs(title = "ACC (2016 - 2021)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("RO"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
HNDMMPRO <- ggplot(HNDMMRO, aes(month, flow))+
  geom_col(width = 0.5, fill = "#FF3D3D")+
  labs(title = "HND (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("RO"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
NNCMMPRO <- ggplot(NNCMMRO, aes(month, flow))+
  geom_col(width = 0.5, fill = "#FF3D3D")+
  labs(title = "NNC (2017 - 2018)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("RO"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
SBHMMPRO <- ggplot(SBHMMRO, aes(month, flow))+
  geom_col(width = 0.5, fill = "#FF3D3D")+
  labs(title = "SBH (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("RO"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
SMCMMPRO <- ggplot(SMCMMRO, aes(month, flow))+
  geom_col(width = 0.5, fill = "#FF3D3D")+
  labs(title = "SMC (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("RO"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
TMRMMPRO <- ggplot(TMRMMRO, aes(month, flow))+
  geom_col(width = 0.5, fill = "#FF3D3D")+
  labs(title = "TMR (2015 - 2017)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("RO"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
WEBMMPRO <- ggplot(WEBMMRO, aes(month, flow))+
  geom_col(width = 0.5, fill = "#FF3D3D")+
  labs(title = "WEB (2014 - 2015)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("RO"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
ROarrange <- grid.arrange(ACCMMPRO, HNDMMPRO, NNCMMPRO, SBHMMPRO, SMCMMPRO, TMRMMPRO, WEBMMPRO, ncol = 2,
                          layout_matrix = cbind(c(1, 3, 5, 7), c(2, 4, 6, NA)), top = textGrob("(A)", gp=gpar(fontsize=14), x = 0, hjust = 0))


LFOACC1ROS <- as.lfobj(ACCX$ROS, hyearstart = 10)
LFOHND1ROS <- as.lfobj(HNDXDM$ROS, hyearstart = 10)
LFONNC1ROS <- as.lfobj(NNCXDM$ROS, hyearstart = 10)
LFOSBH1ROS <- as.lfobj(SBHXDM$ROS, hyearstart = 10)
LFOSMC1ROS <- as.lfobj(SMCXDM$ROS, hyearstart = 10)
LFOTMR1ROS <- as.lfobj(TMRXDM$ROS, hyearstart = 10)
LFOWEB1ROS <- as.lfobj(WEBXDM$ROS, hyearstart = 10)

ACCMMROS <- meanflow(LFOACC1ROS, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
HNDMMROS <- meanflow(LFOHND1ROS, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
NNCMMROS <- meanflow(LFONNC1ROS, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
SBHMMROS <- meanflow(LFOSBH1ROS, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
SMCMMROS <- meanflow(LFOSMC1ROS, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
TMRMMROS <- meanflow(LFOTMR1ROS, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
WEBMMROS <- meanflow(LFOWEB1ROS, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)

plot.new()
ACCMMPROS <- ggplot(ACCMMROS, aes(month, flow))+
  geom_col(width = 0.5, fill = "#FC7272")+
  labs(title = "ACC (2016 - 2021)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("ROS"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
HNDMMPROS <- ggplot(HNDMMROS, aes(month, flow))+
  geom_col(width = 0.5, fill = "#FC7272")+
  labs(title = "HND (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("ROS"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
NNCMMPROS <- ggplot(NNCMMROS, aes(month, flow))+
  geom_col(width = 0.5, fill = "#FC7272")+
  labs(title = "NNC (2017 - 2018)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("ROS"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
SBHMMPROS <- ggplot(SBHMMROS, aes(month, flow))+
  geom_col(width = 0.5, fill = "#FC7272")+
  labs(title = "SBH (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("ROS"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
SMCMMPROS <- ggplot(SMCMMROS, aes(month, flow))+
  geom_col(width = 0.5, fill = "#FC7272")+
  labs(title = "SMC (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("ROS"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
TMRMMPROS <- ggplot(TMRMMROS, aes(month, flow))+
  geom_col(width = 0.5, fill = "#FC7272")+
  labs(title = "TMR (2015 - 2017)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("ROS"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
WEBMMPROS <- ggplot(WEBMMROS, aes(month, flow))+
  geom_col(width = 0.5, fill = "#FC7272")+
  labs(title = "WEB (2014 - 2015)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("ROS"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
ROSarrange <- grid.arrange(ACCMMPROS, HNDMMPROS, NNCMMPROS, SBHMMPROS, SMCMMPROS, TMRMMPROS, WEBMMPROS, ncol = 2,
                           layout_matrix = cbind(c(1, 3, 5, 7), c(2, 4, 6, NA)), top = textGrob("(B)", gp=gpar(fontsize=14), x = 0, hjust = 0))

LFOACC1ROSS <- as.lfobj(ACCX$ROSS, hyearstart = 10)
LFOHND1ROSS <- as.lfobj(HNDXDM$ROSS, hyearstart = 10)
LFONNC1ROSS <- as.lfobj(NNCXDM$ROSS, hyearstart = 10)
LFOSBH1ROSS <- as.lfobj(SBHXDM$ROSS, hyearstart = 10)
LFOSMC1ROSS <- as.lfobj(SMCXDM$ROSS, hyearstart = 10)
LFOTMR1ROSS <- as.lfobj(TMRXDM$ROSS, hyearstart = 10)
LFOWEB1ROSS <- as.lfobj(WEBXDM$ROSS, hyearstart = 10)

ACCMMROSS <- meanflow(LFOACC1ROSS, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
HNDMMROSS <- meanflow(LFOHND1ROSS, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
NNCMMROSS <- meanflow(LFONNC1ROSS, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
SBHMMROSS <- meanflow(LFOSBH1ROSS, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
SMCMMROSS <- meanflow(LFOSMC1ROSS, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
TMRMMROSS <- meanflow(LFOTMR1ROSS, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
WEBMMROSS <- meanflow(LFOWEB1ROSS, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)

plot.new()
ACCMMPROSS <- ggplot(ACCMMROSS, aes(month, flow))+
  geom_col(width = 0.5, fill = "#FFABAB")+
  labs(title = "ACC (2016 - 2021)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("ROSS"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
HNDMMPROSS <- ggplot(HNDMMROSS, aes(month, flow))+
  geom_col(width = 0.5, fill = "#FFABAB")+
  labs(title = "HND (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("ROSS"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
NNCMMPROSS <- ggplot(NNCMMROSS, aes(month, flow))+
  geom_col(width = 0.5, fill = "#FFABAB")+
  labs(title = "NNC (2017 - 2018)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("ROSS"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
SBHMMPROSS <- ggplot(SBHMMROSS, aes(month, flow))+
  geom_col(width = 0.5, fill = "#FFABAB")+
  labs(title = "SBH (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("ROSS"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
SMCMMPROSS <- ggplot(SMCMMROSS, aes(month, flow))+
  geom_col(width = 0.5, fill = "#FFABAB")+
  labs(title = "SMC (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("ROSS"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
TMRMMPROSS <- ggplot(TMRMMROSS, aes(month, flow))+
  geom_col(width = 0.5, fill = "#FFABAB")+
  labs(title = "TMR (2015 - 2017)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("ROSS"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
WEBMMPROSS <- ggplot(WEBMMROSS, aes(month, flow))+
  geom_col(width = 0.5, fill = "#FFABAB")+
  labs(title = "WEB (2014 - 2015)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("ROSS"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
ROSSarrange <- grid.arrange(ACCMMPROSS, HNDMMPROSS, NNCMMPROSS, SBHMMPROSS, SMCMMPROSS, TMRMMPROSS, WEBMMPROSS, ncol = 2,
                            layout_matrix = cbind(c(1, 3, 5, 7), c(2, 4, 6, NA)), top = textGrob("(C)", gp=gpar(fontsize=14), x = 0, hjust = 0))

plot.new()
grid.arrange(ROarrange, ROSarrange, ROSSarrange, ncol = 2, top = "")

#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
###                   Seasonal plots for Soil Water                    ###
###                            Appendix III                            ###
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
LFOACC1VSWL1 <- as.lfobj(ACCX$VSWL1, hyearstart = 10)
LFOHND1VSWL1 <- as.lfobj(HNDXDM$VSWL1, hyearstart = 10)
LFONNC1VSWL1 <- as.lfobj(NNCXDM$VSWL1, hyearstart = 10)
LFOSBH1VSWL1 <- as.lfobj(SBHXDM$VSWL1, hyearstart = 10)
LFOSMC1VSWL1 <- as.lfobj(SMCXDM$VSWL1, hyearstart = 10)
LFOTMR1VSWL1 <- as.lfobj(TMRXDM$VSWL1, hyearstart = 10)
LFOWEB1VSWL1 <- as.lfobj(WEBXDM$VSWL1, hyearstart = 10)

ACCMMVSWL1 <- meanflow(LFOACC1VSWL1, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
HNDMMVSWL1 <- meanflow(LFOHND1VSWL1, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
NNCMMVSWL1 <- meanflow(LFONNC1VSWL1, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
SBHMMVSWL1 <- meanflow(LFOSBH1VSWL1, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
SMCMMVSWL1 <- meanflow(LFOSMC1VSWL1, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
TMRMMVSWL1 <- meanflow(LFOTMR1VSWL1, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
WEBMMVSWL1 <- meanflow(LFOWEB1VSWL1, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)

plot.new()
ACCMMPVSWL1 <- ggplot(ACCMMVSWL1, aes(month, flow))+
  geom_col(width = 0.5, fill = "#5712A6")+
  labs(title = "ACC (2016 - 2021)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL1"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
HNDMMPVSWL1 <- ggplot(HNDMMVSWL1, aes(month, flow))+
  geom_col(width = 0.5, fill = "#5712A6")+
  labs(title = "HND (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL1"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
NNCMMPVSWL1 <- ggplot(NNCMMVSWL1, aes(month, flow))+
  geom_col(width = 0.5, fill = "#5712A6")+
  labs(title = "NNC (2017 - 2018)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL1"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
SBHMMPVSWL1 <- ggplot(SBHMMVSWL1, aes(month, flow))+
  geom_col(width = 0.5, fill = "#5712A6")+
  labs(title = "SBH (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL1"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
SMCMMPVSWL1 <- ggplot(SMCMMVSWL1, aes(month, flow))+
  geom_col(width = 0.5, fill = "#5712A6")+
  labs(title = "SMC (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL1"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
TMRMMPVSWL1<- ggplot(TMRMMVSWL1, aes(month, flow))+
  geom_col(width = 0.5, fill = "#5712A6")+
  labs(title = "TMR (2015 - 2017)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL1"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
WEBMMPVSWL1 <- ggplot(WEBMMVSWL1, aes(month, flow))+
  geom_col(width = 0.5, fill = "#5712A6")+
  labs(title = "WEB (2014 - 2015)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL1"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
VSWL1arrange <- grid.arrange(ACCMMPVSWL1, HNDMMPVSWL1, NNCMMPVSWL1, SBHMMPVSWL1, SMCMMPVSWL1, TMRMMPVSWL1, WEBMMPVSWL1, ncol = 2,
                             layout_matrix = cbind(c(1, 3, 5, 7), c(2, 4, 6, NA)), top = textGrob("(A)", gp=gpar(fontsize=14), x = 0, hjust = 0))

LFOACC1VSWL2 <- as.lfobj(ACCX$VSWL2, hyearstart = 10)
LFOHND1VSWL2 <- as.lfobj(HNDXDM$VSWL2, hyearstart = 10)
LFONNC1VSWL2 <- as.lfobj(NNCXDM$VSWL2, hyearstart = 10)
LFOSBH1VSWL2 <- as.lfobj(SBHXDM$VSWL2, hyearstart = 10)
LFOSMC1VSWL2 <- as.lfobj(SMCXDM$VSWL2, hyearstart = 10)
LFOTMR1VSWL2 <- as.lfobj(TMRXDM$VSWL2, hyearstart = 10)
LFOWEB1VSWL2 <- as.lfobj(WEBXDM$VSWL2, hyearstart = 10)

ACCMMVSWL2 <- meanflow(LFOACC1VSWL2, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
HNDMMVSWL2 <- meanflow(LFOHND1VSWL2, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
NNCMMVSWL2 <- meanflow(LFONNC1VSWL2, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
SBHMMVSWL2 <- meanflow(LFOSBH1VSWL2, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
SMCMMVSWL2 <- meanflow(LFOSMC1VSWL2, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
TMRMMVSWL2 <- meanflow(LFOTMR1VSWL2, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
WEBMMVSWL2 <- meanflow(LFOWEB1VSWL2, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)

plot.new()
ACCMMPVSWL2 <- ggplot(ACCMMVSWL2, aes(month, flow))+
  geom_col(width = 0.5, fill = "#7C4CB8")+
  labs(title = "ACC (2016 - 2021)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL2"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
HNDMMPVSWL2 <- ggplot(HNDMMVSWL2, aes(month, flow))+
  geom_col(width = 0.5, fill = "#7C4CB8")+
  labs(title = "HND (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL2"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
NNCMMPVSWL2 <- ggplot(NNCMMVSWL2, aes(month, flow))+
  geom_col(width = 0.5, fill = "#7C4CB8")+
  labs(title = "NNC (2017 - 2018)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL2"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
SBHMMPVSWL2 <- ggplot(SBHMMVSWL2, aes(month, flow))+
  geom_col(width = 0.5, fill = "#7C4CB8")+
  labs(title = "SBH (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL2"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
SMCMMPVSWL2 <- ggplot(SMCMMVSWL2, aes(month, flow))+
  geom_col(width = 0.5, fill = "#7C4CB8")+
  labs(title = "SMC (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL2"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
TMRMMPVSWL2<- ggplot(TMRMMVSWL2, aes(month, flow))+
  geom_col(width = 0.5, fill = "#7C4CB8")+
  labs(title = "TMR (2015 - 2017)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL2"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
WEBMMPVSWL2 <- ggplot(WEBMMVSWL2, aes(month, flow))+
  geom_col(width = 0.5, fill = "#7C4CB8")+
  labs(title = "WEB (2014 - 2015)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL2"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
VSWL2arrange <- grid.arrange(ACCMMPVSWL2, HNDMMPVSWL2, NNCMMPVSWL2, SBHMMPVSWL2, SMCMMPVSWL2, TMRMMPVSWL2, WEBMMPVSWL2, ncol = 2,
                             layout_matrix = cbind(c(1, 3, 5, 7), c(2, 4, 6, NA)), top = textGrob("(B)", gp=gpar(fontsize=14), x = 0, hjust = 0))

LFOACC1VSWL3 <- as.lfobj(ACCX$VSWL3, hyearstart = 10)
LFOHND1VSWL3 <- as.lfobj(HNDXDM$VSWL3, hyearstart = 10)
LFONNC1VSWL3 <- as.lfobj(NNCXDM$VSWL3, hyearstart = 10)
LFOSBH1VSWL3 <- as.lfobj(SBHXDM$VSWL3, hyearstart = 10)
LFOSMC1VSWL3 <- as.lfobj(SMCXDM$VSWL3, hyearstart = 10)
LFOTMR1VSWL3 <- as.lfobj(TMRXDM$VSWL3, hyearstart = 10)
LFOWEB1VSWL3 <- as.lfobj(WEBXDM$VSWL3, hyearstart = 10)

ACCMMVSWL3 <- meanflow(LFOACC1VSWL3, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
HNDMMVSWL3 <- meanflow(LFOHND1VSWL3, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
NNCMMVSWL3 <- meanflow(LFONNC1VSWL3, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
SBHMMVSWL3 <- meanflow(LFOSBH1VSWL3, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
SMCMMVSWL3 <- meanflow(LFOSMC1VSWL3, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
TMRMMVSWL3 <- meanflow(LFOTMR1VSWL3, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
WEBMMVSWL3 <- meanflow(LFOWEB1VSWL3, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)

plot.new()
ACCMMPVSWL3 <- ggplot(ACCMMVSWL3, aes(month, flow))+
  geom_col(width = 0.5, fill = "#8664B0")+
  labs(title = "ACC (2016 - 2021)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL3"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
HNDMMPVSWL3 <- ggplot(HNDMMVSWL3, aes(month, flow))+
  geom_col(width = 0.5, fill = "#8664B0")+
  labs(title = "HND (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL3"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
NNCMMPVSWL3 <- ggplot(NNCMMVSWL3, aes(month, flow))+
  geom_col(width = 0.5, fill = "#8664B0")+
  labs(title = "NNC (2017 - 2018)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL3"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
SBHMMPVSWL3 <- ggplot(SBHMMVSWL3, aes(month, flow))+
  geom_col(width = 0.5, fill = "#8664B0")+
  labs(title = "SBH (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL3"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
SMCMMPVSWL3 <- ggplot(SMCMMVSWL3, aes(month, flow))+
  geom_col(width = 0.5, fill = "#8664B0")+
  labs(title = "SMC (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL3"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
TMRMMPVSWL3<- ggplot(TMRMMVSWL3, aes(month, flow))+
  geom_col(width = 0.5, fill = "#8664B0")+
  labs(title = "TMR (2015 - 2017)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL3"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
WEBMMPVSWL3 <- ggplot(WEBMMVSWL3, aes(month, flow))+
  geom_col(width = 0.5, fill = "#8664B0")+
  labs(title = "WEB (2014 - 2015)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL3"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
VSWL3arrange <- grid.arrange(ACCMMPVSWL3, HNDMMPVSWL3, NNCMMPVSWL3, SBHMMPVSWL3, SMCMMPVSWL3, TMRMMPVSWL3, WEBMMPVSWL3, ncol = 2,
                             layout_matrix = cbind(c(1, 3, 5, 7), c(2, 4, 6, NA)), top = textGrob("(C)", gp=gpar(fontsize=14), x = 0, hjust = 0))

LFOACC1VSWL4 <- as.lfobj(ACCX$VSWL4, hyearstart = 10)
LFOHND1VSWL4 <- as.lfobj(HNDXDM$VSWL4, hyearstart = 10)
LFONNC1VSWL4 <- as.lfobj(NNCXDM$VSWL4, hyearstart = 10)
LFOSBH1VSWL4 <- as.lfobj(SBHXDM$VSWL4, hyearstart = 10)
LFOSMC1VSWL4 <- as.lfobj(SMCXDM$VSWL4, hyearstart = 10)
LFOTMR1VSWL4 <- as.lfobj(TMRXDM$VSWL4, hyearstart = 10)
LFOWEB1VSWL4 <- as.lfobj(WEBXDM$VSWL4, hyearstart = 10)

ACCMMVSWL4 <- meanflow(LFOACC1VSWL4, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
HNDMMVSWL4 <- meanflow(LFOHND1VSWL4, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
NNCMMVSWL4 <- meanflow(LFONNC1VSWL4, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
SBHMMVSWL4 <- meanflow(LFOSBH1VSWL4, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
SMCMMVSWL4 <- meanflow(LFOSMC1VSWL4, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
TMRMMVSWL4 <- meanflow(LFOTMR1VSWL4, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)
WEBMMVSWL4 <- meanflow(LFOWEB1VSWL4, year = "any", monthly = TRUE, yearly = FALSE, breakdays = NULL, na.rm = TRUE)

summary(ACCMMVSWL4$flow)
summary(HNDMMVSWL4$flow)
summary(NNCMMVSWL4$flow)
summary(SBHMMVSWL4$flow)
summary(SMCMMVSWL4$flow)
summary(TMRMMVSWL4$flow)
summary(WEBMMVSWL4$flow)

plot.new()
ACCMMPVSWL4 <- ggplot(ACCMMVSWL4, aes(month, flow))+
  geom_col(width = 0.5, fill = "#AC95C7")+
  labs(title = "ACC (2016 - 2021)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL4"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
HNDMMPVSWL4 <- ggplot(HNDMMVSWL4, aes(month, flow))+
  geom_col(width = 0.5, fill = "#AC95C7")+
  labs(title = "HND (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL4"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
NNCMMPVSWL4 <- ggplot(NNCMMVSWL4, aes(month, flow))+
  geom_col(width = 0.5, fill = "#AC95C7")+
  labs(title = "NNC (2017 - 2018)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL4"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
SBHMMPVSWL4 <- ggplot(SBHMMVSWL4, aes(month, flow))+
  geom_col(width = 0.5, fill = "#AC95C7")+
  labs(title = "SBH (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL4"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
SMCMMPVSWL4 <- ggplot(SMCMMVSWL4, aes(month, flow))+
  geom_col(width = 0.5, fill = "#AC95C7")+
  labs(title = "SMC (2016 - 2020)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL4"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
TMRMMPVSWL4<- ggplot(TMRMMVSWL4, aes(month, flow))+
  geom_col(width = 0.5, fill = "#AC95C7")+
  labs(title = "TMR (2015 - 2017)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL4"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
WEBMMPVSWL4 <- ggplot(WEBMMVSWL4, aes(month, flow))+
  geom_col(width = 0.5, fill = "#AC95C7")+
  labs(title = "WEB (2014 - 2015)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels =  c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S"))+
  theme(axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression("VSWL4"))+
  theme(plot.title = element_text(size=9))+
  theme(axis.title.y = element_text(size=9))
VSWL4arrange <- grid.arrange(ACCMMPVSWL4, HNDMMPVSWL4, NNCMMPVSWL4, SBHMMPVSWL4, SMCMMPVSWL4, TMRMMPVSWL4, WEBMMPVSWL4, ncol = 2,
                             layout_matrix = cbind(c(1, 3, 5, 7), c(2, 4, 6, NA)), top = textGrob("(D)", gp=gpar(fontsize=14), x = 0, hjust = 0))

plot.new()
grid.arrange(VSWL1arrange, VSWL2arrange, VSWL3arrange, VSWL4arrange, ncol = 2, top = "")

#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#                               APPENDIX IV                              #
#                     Investigating model permormance                    #
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////
dev.off()
plot.new()
par(mfrow = c(3, 3))
plotres(RF_ACC_2All, which = 1, main = "Error vs Number of Trees (Qnorm)")
plotres(RF_ACC_2All, which = 3, main = "Residuals vs Fitted (Qnorm)")
plotres(RF_ACC_2All, which = 4, main = "Residual QQ (Qnorm)")
plotres(RF_ACC_180, which = 1, main = "Error vs Number of Trees (Q80)")
plotres(RF_ACC_180, which = 3, main = "Residuals vs Fitted (Q80)")
plotres(RF_ACC_180, which = 4, main = "Residual QQ (Q80)")
plotres(RF_ACC_120, which = 1, main = "Error vs Number of Trees (Q20)")
plotres(RF_ACC_120, which = 3, main = "Residuals vs Fitted (Q20)")
plotres(RF_ACC_120, which = 4, main = "Residual QQ (Q20)")

plot.new()
par(mfrow = c(3, 3))
plotres(RF_HND_2All, which = 1, main = "Error vs Number of Trees (Qnorm)")
plotres(RF_HND_2All, which = 3, main = "Residuals vs Fitted (Qnorm)")
plotres(RF_HND_2All, which = 4, main = "Residual QQ (Qnorm)")
plotres(RF_HND_180, which = 1, main = "Error vs Number of Trees (Q80)")
plotres(RF_HND_180, which = 3, main = "Residuals vs Fitted (Q80)")
plotres(RF_HND_180, which = 4, main = "Residual QQ (Q80)")
plotres(RF_HND_120, which = 1, main = "Error vs Number of Trees (Q20)")
plotres(RF_HND_120, which = 3, main = "Residuals vs Fitted (Q20)")
plotres(RF_HND_120, which = 4, main = "Residual QQ (Q20)")

plot.new()
par(mfrow = c(3, 3))
plotres(RF_NNC_2All, which = 1, main = "Error vs Number of Trees (Qnorm)")
plotres(RF_NNC_2All, which = 3, main = "Residuals vs Fitted (Qnorm)")
plotres(RF_NNC_2All, which = 4, main = "Residual QQ (Qnorm)")
plotres(RF_NNC_180, which = 1, main = "Error vs Number of Trees (Q80)")
plotres(RF_NNC_180, which = 3, main = "Residuals vs Fitted (Q80)")
plotres(RF_NNC_180, which = 4, main = "Residual QQ (Q80)")
plotres(RF_NNC_120, which = 1, main = "Error vs Number of Trees (Q20)")
plotres(RF_NNC_120, which = 3, main = "Residuals vs Fitted (Q20)")
plotres(RF_NNC_120, which = 4, main = "Residual QQ (Q20)")

plot.new()
par(mfrow = c(3, 3))
plotres(RF_SBH_2All, which = 1, main = "Error vs Number of Trees (Qnorm)")
plotres(RF_SBH_2All, which = 3, main = "Residuals vs Fitted (Qnorm)")
plotres(RF_SBH_2All, which = 4, main = "Residual QQ (Qnorm)")
plotres(RF_SBH_180, which = 1, main = "Error vs Number of Trees (Q80)")
plotres(RF_SBH_180, which = 3, main = "Residuals vs Fitted (Q80)")
plotres(RF_SBH_180, which = 4, main = "Residual QQ (Q80)")
plotres(RF_SBH_120, which = 1, main = "Error vs Number of Trees (Q20)")
plotres(RF_SBH_120, which = 3, main = "Residuals vs Fitted (Q20)")
plotres(RF_SBH_120, which = 4, main = "Residual QQ (Q20)")

plot.new()
par(mfrow = c(3, 3))
plotres(RF_SMC_2All, which = 1, main = "Error vs Number of Trees (Qnorm)")
plotres(RF_SMC_2All, which = 3, main = "Residuals vs Fitted (Qnorm)")
plotres(RF_SMC_2All, which = 4, main = "Residual QQ (Qnorm)")
plotres(RF_SMC_180, which = 1, main = "Error vs Number of Trees (Q80)")
plotres(RF_SMC_180, which = 3, main = "Residuals vs Fitted (Q80)")
plotres(RF_SMC_180, which = 4, main = "Residual QQ (Q80)")
plotres(RF_SMC_120, which = 1, main = "Error vs Number of Trees (Q20)")
plotres(RF_SMC_120, which = 3, main = "Residuals vs Fitted (Q20)")
plotres(RF_SMC_120, which = 4, main = "Residual QQ (Q20)")

plot.new()
par(mfrow = c(3, 3))
plotres(RF_TMR_2All, which = 1, main = "Error vs Number of Trees (Qnorm)")
plotres(RF_TMR_2All, which = 3, main = "Residuals vs Fitted (Qnorm)")
plotres(RF_TMR_2All, which = 4, main = "Residual QQ (Qnorm)")
plotres(RF_TMR_180, which = 1, main = "Error vs Number of Trees (Q80)")
plotres(RF_TMR_180, which = 3, main = "Residuals vs Fitted (Q80)")
plotres(RF_TMR_180, which = 4, main = "Residual QQ (Q80)")
plotres(RF_TMR_120, which = 1, main = "Error vs Number of Trees (Q20)")
plotres(RF_TMR_120, which = 3, main = "Residuals vs Fitted (Q20)")
plotres(RF_TMR_120, which = 4, main = "Residual QQ (Q20)")

plot.new()
par(mfrow = c(3, 3))
plotres(RF_WEB_2All, which = 1, main = "Error vs Number of Trees (Qnorm)")
plotres(RF_WEB_2All, which = 3, main = "Residuals vs Fitted (Qnorm)")
plotres(RF_WEB_2All, which = 4, main = "Residual QQ (Qnorm)")
plotres(RF_WEB_180, which = 1, main = "Error vs Number of Trees (Q80)")
plotres(RF_WEB_180, which = 3, main = "Residuals vs Fitted (Q80)")
plotres(RF_WEB_180, which = 4, main = "Residual QQ (Q80)")
plotres(RF_WEB_120, which = 1, main = "Error vs Number of Trees (Q20)")
plotres(RF_WEB_120, which = 3, main = "Residuals vs Fitted (Q20)")
plotres(RF_WEB_120, which = 4, main = "Residual QQ (Q20)")

#================================================================================================================
#================================================================================================================
#=======================================================END======================================================
#================================================================================================================
#================================================================================================================


#addtional Information not presented in the final thesis
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#              multi seasonal time series decomposition for discharge (MSTL)               #
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
ACCmsts <- msts(ACCD$Q_m3.s, seasonal.periods = c(30, 365), start = 2015+10/12)
HNDmsts <- msts(HND$Q_m3.s, seasonal.periods = c(24, 24*30, 24*365), start = 2015+10/12)
NNCmsts <- msts(NNC$Q_m3.s, seasonal.periods = c(24, 24*30, 24*90), start = 2016+10/12)
SBHmsts <- msts(SBH$Q_m3.s, seasonal.periods = c(24, 24*30, 24*365), start = 2015+10/12)
SMCmsts <- msts(SMC$Q_cfs, seasonal.periods = c(24, 24*30, 24*365), start = 2015+10/12)
TMRmsts <- msts(TMR$Q_cfs, seasonal.periods = c(24, 24*30, 24*365), start = 2014+10/12)
WEBmsts <- msts(WEB$Q_m3.s, seasonal.periods = c(24, 24*30, 24*90), start = 2013+10/12)

ACCmstl <- mstl(ACCmsts, lambda = "auto", iterate = 10, s.window = c(30, 365), s.degree = 0, t.degree = 0)
HNDmstl <- mstl(HNDmsts, lambda = "auto", iterate = 10, s.window = c(24, 24*30, 24*365), s.degree = 0, t.degree = 0)
NNCmstl <- mstl(NNCmsts, lambda = "auto", iterate = 10, s.window = c(24, 24*30, 24*90), s.degree = 0, t.degree = 0)
SBHmstl <- mstl(SBHmsts, lambda = "auto", iterate = 10, s.window = c(24, 24*30, 24*365), s.degree = 0, t.degree = 0)
SMCmstl <- mstl(SMCmsts, lambda = "auto", iterate = 10, s.window = c(24, 24*30, 24*365), s.degree = 0, t.degree = 0)
TMRmstl <- mstl(TMRmsts, lambda = "auto", iterate = 10, s.window = c(24, 24*30, 24*365), s.degree = 0, t.degree = 0)
WEBmstl <- mstl(WEBmsts, lambda = "auto", iterate = 10, s.window = c(24, 24*30, 24*90), s.degree = 0, t.degree = 0)

ACC_mstl_Q <- autoplot(ACCmstl)+
  ggtitle(bquote("Q"~m^3~"/s"))+
  theme(text = element_text(size = 8.5))+
  scale_x_continuous(breaks = seq(2015, 2021, 0.5))
HND_mstl_Q <- autoplot(HNDmstl)+
  ggtitle(bquote("Q"~m^3~"/s"))+
  theme(text = element_text(size = 8.5))+
  scale_x_continuous(breaks = seq(2015, 2020, 0.5))
NNC_mstl_Q <- autoplot(NNCmstl)+
  ggtitle(bquote("Q"~m^3~"/s"))+
  theme(text = element_text(size = 8.5))+
  scale_x_continuous(breaks = seq(0, 23, 1))
SBH_mstl_Q <- autoplot(SBHmstl)+
  ggtitle(bquote("Q"~m^3~"/s"))+
  theme(text = element_text(size = 8.5))+
  scale_x_continuous(breaks = seq(2015, 2020, 0.5))
SMC_mstl_Q <- autoplot(SMCmstl)+
  ggtitle(bquote("Q cfs"))+
  theme(text = element_text(size = 8.5))+
  scale_x_continuous(breaks = seq(2015, 2020, 0.5))
TMR_mstl_Q <- autoplot(TMRmstl)+
  ggtitle(bquote("Q cfs"))+
  theme(text = element_text(size = 8.5))+
  scale_x_continuous(breaks = seq(2014, 2017, 0.5))
WEB_mstl_Q <- autoplot(WEBmstl)+
  ggtitle(bquote("Q l/s"))+
  theme(text = element_text(size = 8.5))+
  scale_x_continuous(breaks = seq(0, 22, 1))
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#                multi seasonal time series decomposition for nitrate (MSTL)              #
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
ACCmstsN <- msts(ACC$No3_mg.l, seasonal.periods = c(24, 24*30, 24*365), start = 2015+10/12)
HNDmstsN <- msts(HND$No3_mg.l, seasonal.periods = c(24, 24*30, 24*365), start = 2015+10/12)
NNCmstsN <- msts(NNC$No3_mg.l, seasonal.periods = c(24, 24*30, 24*90), start = 2016+10/12)
SBHmstsN <- msts(SBH$No3_mg.l, seasonal.periods = c(24, 24*30, 24*365), start = 2015+10/12)
SMCmstsN <- msts(SMC$No3_mg.l, seasonal.periods = c(24, 24*30, 24*365), start = 2015+10/12)
TMRmstsN <- msts(TMR$No3_mg.l, seasonal.periods = c(24, 24*30, 24*365), start = 2014+10/12)
WEBmstsN <- msts(WEB$No3_mg.l, seasonal.periods = c(24, 24*30, 24*90), start = 2013+10/12)

ACCmstlN <- mstl(ACCmstsN, lambda = "auto", iterate = 10, s.window = c(24, 24*30, 24*365), s.degree = 0, t.degree = 0)
HNDmstlN <- mstl(HNDmstsN, lambda = "auto", iterate = 10, s.window = c(24, 24*30, 24*365), s.degree = 0, t.degree = 0)
NNCmstlN <- mstl(NNCmstsN, lambda = "auto", iterate = 10, s.window = c(24, 24*30, 24*90), s.degree = 0, t.degree = 0)
SBHmstlN <- mstl(SBHmstsN, lambda = "auto", iterate = 10, s.window = c(24, 24*30, 24*365), s.degree = 0, t.degree = 0)
SMCmstlN <- mstl(SMCmstsN, lambda = "auto", iterate = 10, s.window = c(24, 24*30, 24*365), s.degree = 0, t.degree = 0)
TMRmstlN <- mstl(TMRmstsN, lambda = "auto", iterate = 10, s.window = c(24, 24*30, 24*365), s.degree = 0, t.degree = 0)
WEBmstlN <- mstl(WEBmstsN, lambda = "auto", iterate = 10, s.window = c(24, 24*30, 24*90), s.degree = 0, t.degree = 0)

ACC_mstl_N <- autoplot(ACCmstlN, main = "ACC  - multiple seasonal decomposition")+
  ggtitle(bquote("NO3-"~"mg/l"))+
  theme(text = element_text(size = 8.5))+
  scale_x_continuous(breaks = seq(2015, 2021, 0.5))
HND_mstl_N <- autoplot(HNDmstlN, main = "HND  - multiple seasonal decomposition")+
  ggtitle(bquote("NO3-"~"mg/l"))+
  theme(text = element_text(size = 8.5))+
  scale_x_continuous(breaks = seq(2015, 2020, 0.5))
NNC_mstl_N <- autoplot(NNCmstlN, main = "NNC  - multiple seasonal decomposition")+
  ggtitle(bquote("NO3-"~"mg/l"))+
  scale_x_continuous(breaks = seq(0, 23, 1))+
  theme(text = element_text(size = 8.5))
SBH_mstl_N <- autoplot(SBHmstlN, main = "SBH  - multiple seasonal decomposition")+
  ggtitle(bquote("NO3-"~"mg/l"))+
  theme(text = element_text(size = 8.5))+
  scale_x_continuous(breaks = seq(2015, 2020, 0.5))
SMC_mstl_N <- autoplot(SMCmstlN, main = "SMC  - multiple seasonal decomposition")+
  ggtitle(bquote("NO3-"~"mg/l"))+
  theme(text = element_text(size = 8.5))+
  scale_x_continuous(breaks = seq(2015, 2020, 0.5))
TMR_mstl_N <- autoplot(TMRmstlN, main = "TMR  - multiple seasonal decomposition")+
  ggtitle(bquote("NO3-"~"mg/l"))+
  theme(text = element_text(size = 8.5))+
  scale_x_continuous(breaks = seq(2015, 2017, 0.25))
WEB_mstl_N <- autoplot(WEBmstlN, main = "WEB  - multiple seasonal decomposition")+
  ggtitle(bquote("NO3-"~"mg/l"))+
  scale_x_continuous(breaks = seq(0, 22, 2))+
  theme(text = element_text(size = 8.5))

grid.arrange(ACC_mstl_Q, ACC_mstl_N)
grid.arrange(HND_mstl_Q, HND_mstl_N)
grid.arrange(NNC_mstl_Q, NNC_mstl_N)
grid.arrange(SBH_mstl_Q, SBH_mstl_N)
grid.arrange(SMC_mstl_Q, SMC_mstl_N)
grid.arrange(TMR_mstl_Q, TMR_mstl_N)
grid.arrange(WEB_mstl_Q, WEB_mstl_N)

#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#                                   plotting droughts                                     #
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////
ACCDroughts <- find_droughts(LFOACC1, threshold = "Q80")
HNDDroughts <- find_droughts(LFOHND1, threshold = "Q80")
NNCDroughts <- find_droughts(LFONNC1, threshold = "Q80")
SBHDroughts <- find_droughts(LFOSBH1, threshold = "Q80")
SMCDroughts <- find_droughts(LFOSMC1, threshold = "Q80")
TMRDroughts <- find_droughts(LFOTMR1, threshold = "Q80")
WEBDroughts <- find_droughts(LFOWEB1, threshold = "Q80")

#### pooling droughts by moving average (7 days)
ACCPooled <- pool_ma(ACCDroughts, n=7)
HNDPooled <- pool_ma(HNDDroughts, n=7)
NNCPooled <- pool_ma(NNCDroughts, n=7)
SBHPooled <- pool_ma(SBHDroughts, n=7)
SMCPooled <- pool_ma(SMCDroughts, n=7)
TMRPooled <- pool_ma(TMRDroughts, n=7)
WEBPooled <- pool_ma(WEBDroughts, n=7)

#### as interactive plots
plot(ACCPooled)
plot(HNDPooled)
plot(NNCPooled)
plot(SBHPooled)
plot(SMCPooled)
plot(TMRPooled)
plot(WEBPooled)

#### as XTS plots
plot.new()
par(mfrow = c(4,2), cex = 0.5, mar = c(4, 3, 2, 1), oma = c(3, 2, 3, 2))
plot.xts(ACCDroughts, col = "steel blue", observation.based = TRUE, ylim = c(-90000, 30000), yaxis.left = TRUE, yaxis.right = FALSE, lwd = 0.9, main = "ACC - Q80", )
plot.xts(HNDDroughts, col = "steel blue", observation.based = TRUE, ylim = c(-300000, 50000), yaxis.left = TRUE, yaxis.right = FALSE, lwd = 0.9, main = "HND - Q80")
plot.xts(NNCDroughts, col = "steel blue", observation.based = TRUE, ylim = c(-6e+05, 2e+05), yaxis.left = TRUE, yaxis.right = FALSE, lwd = 0.9, main = "NNC - Q80")
plot.xts(SBHDroughts, col = "steel blue", observation.based = TRUE, ylim = c(-4e+05, 1e+05), yaxis.left = TRUE, yaxis.right = FALSE, lwd = 0.9, main = "SBH - Q80")
plot.xts(SMCDroughts, col = "steel blue", observation.based = TRUE, ylim = c(-7e+05, 1e+05), yaxis.left = TRUE, yaxis.right = FALSE, lwd = 0.9, main = "SMC - Q80")
plot.xts(TMRDroughts, col = "steel blue", observation.based = TRUE, ylim = c(-1e+07, 2e+06), yaxis.left = TRUE, yaxis.right = FALSE, lwd = 0.9, main = "TMR - Q80")
plot.xts(WEBDroughts, col = "steel blue", observation.based = TRUE, ylim = c(-5000, 500), yaxis.left = TRUE, yaxis.right = FALSE, lwd = 0.9, main = "WEB - Q80")
