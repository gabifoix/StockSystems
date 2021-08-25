rm(list = ls(all.names = TRUE))
library(YHFinR)
library(tidyverse)
library(magrittr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(xts)
library(quantmod)
library(PerformanceAnalytics)
source('R/StockResearchCore.R')

.reduceRQM <- function(RQM.list) {
  RQM.list <- RQM.list[sort(names(RQM.list))]
  RQMhist <- RQM.list %>%
    reduce(left_join, by = "Date")
  RQMhist
}

# Config ----
Ref_folder <- "Source"
end.date <- as.POSIXct(strptime("2021-07-25", format = "%Y-%m-%d"))
Country <- "US"

LossThreshold <- 0.15
ProfitDropThreshold <- 0.2

# Import ----
Files <- list.files(Ref_folder)

HistPr.list <- readRDS(file.path(Ref_folder, grep("HistPr.list", Files, value = TRUE)))
PF <- read.csv(file.path(Ref_folder, "RQMportfolio.csv"), stringsAsFactors = FALSE)

if (Country == "US") {
  MktPr <- YHFinR::getYFHistPrices("^GSPC", "2y", "1d")$`^GSPC`
} else {
  MktPr <- YHFinR::getYFHistPrices("^STOXX50E", "2y", "1d")$`^STOXX50E`
}

# Select Portfolio ----
subPF <- PF %>%
  subset(Portfolio == Country)
subPF$Date <- as.POSIXct(strptime(subPF$Date, format = "%d-%m-%Y"))
subPF <- subPF[subPF$Date < end.date, c("Date", "Ticker")]

# Hist Performance ----
# > Loop over start.date
RQMhist.list <- list()
SLhist.list <- list()
MKT.list <- list()
for (i in 1:length(unique(subPF$Date))) { # start.date = sort(unique(subPF$Date))[7]
  start.date <- unique(subPF$Date)[i]
  datelabel <- paste0("Y", year(start.date), "M", month(start.date))
  message("Date: ", datelabel)
  
  subPF_t <- subPF %>%
    subset(Date == start.date)
  
  HistPr.list_t <- HistPr.list[names(HistPr.list) %in% subPF_t$Ticker] %>% 
    lapply(convertHistPr2xts) %>% 
    lapply(function(x) x[paste(start.date, end.date,sep="::")])
  
  # Mkt Index. One index per date
  MKT.DF <- convertHistPr2xts(MktPr)[paste(start.date, end.date,sep="::")] %>% 
    RQMDFformat("MKT", datelabel)
  MKT.list[[datelabel]] <- MKT.DF
  
  DF.list <- list()
  SLDF.list <- list()
  # >> Loop over assets for each date 
  for (one.asset in names(HistPr.list_t)) { # one.asset = names(HistPr.list_t)[1]
    message(" > Ticker: ", one.asset)
    one.HistPr <- HistPr.list_t[[one.asset]]
    DF <- RQMDFformat(one.HistPr, one.asset, datelabel)
    DF.list[[one.asset]] <- DF
    
    # Calc Stop-Loss
    # Assumption: Adquisition Price = 1
    SLDF <- data.frame(DF,
                       MKT = MKT.DF[, 2], 
                       TrMAX = cummax(DF[,2])) 
    colnames(SLDF)[2] <- "PR"
    SLDF <- SLDF %>% 
      mutate(MKTadj = 1+((MKT-1)/2),
             LSL = (1 - LossThreshold) * MKTadj,
             PSL = (1 - ProfitDropThreshold) * MKTadj * TrMAX,
             SL = ifelse(PR > 1 + LossThreshold, PSL, LSL)) %>% 
      .[, c("Date", "SL")]
    colnames(SLDF)[2] <- paste("SL", one.asset, datelabel, sep = "_")
    SLDF.list[[one.asset]] <- SLDF
    
  }
  RQMhist.list[[datelabel]] <- .reduceRQM(DF.list)
  SLhist.list[[datelabel]] <- .reduceRQM(SLDF.list)
}

# RQMhist data.frame: nrow = Date, ncol = Ticker
# NAs. for "Not Aquired yet" assets and/or "Already sold" assets.
RQMhist <- .reduceRQM(RQMhist.list)
SLhist <- .reduceRQM(SLhist.list)
stopifnot(dim(RQMhist) == dim(SLhist))

# Apply Stop-Loss ----
# refDates = first Monday of the month
refDates <- unique(subPF$Date)

# SLMonth, unique SL per month
SLMonth <- SLhist %>% 
  mutate(YearMonth = paste0(year(Date), month(Date)))

Date2YearMonth <- unique(SLMonth[, c("Date", "YearMonth")])

SLMonth <- SLMonth %>% 
  subset(Date %in% refDates) %>%
  select(-Date) %>% 
  full_join(Date2YearMonth, by = "YearMonth") %>% 
  .[, colnames(SLhist)]

stopifnot(dim(RQMhist) == dim(SLMonth))

# Historical holdings
RQMholdingshist <- RQMhist
RQMholdingshist[, setdiff(colnames(RQMhist), "Date")] <- RQMhist[, setdiff(colnames(RQMhist), "Date")] > SLMonth[, setdiff(colnames(SLMonth), "Date")]

# From the 1st StopLoss, all FALSE
for (i in 2:ncol(RQMholdingshist)) {
  firstmatch <- match(unique(RQMholdingshist[,i]), RQMholdingshist[,i])
  firstFALSE <- which(unique(RQMholdingshist[,i]) == FALSE)
  if (length(firstFALSE) == 1) {
    RQMholdingshist[seq(firstmatch[firstFALSE], length(RQMholdingshist[,i])), i] <- rep(FALSE, length(seq(firstmatch[firstFALSE], length(RQMholdingshist[,i]))))
  }
}

