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
end.date <- as.POSIXct(strptime("2021-09-30", format = "%Y-%m-%d"))
Country <- "US"

LossThreshold <- 0.1
ProfitDropThreshold <- 0.1

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
  
  # Subsetting dates
  subPF_t <- subPF %>%
    subset(Date == start.date)
  
  # Pull data from YF in case of incomplete adjclose 
  idx <- sapply(subPF_t$Ticker, function(x) any(is.na(HistPr.list[[x]]$adjclose)))
  idxNULL <- sapply(subPF_t$Ticker, function(x) is.null(HistPr.list[[x]]))
  IncompleteTickers <- subPF_t$Ticker[idx | idxNULL]
  if (length(IncompleteTickers) > 0) {
    IncompHistPrices <- YHFinR::getYFHistPrices(IncompleteTickers, "2y", "1d", block = 100, slp = 10)
    # And replace on the main list
    HistPr.list[names(HistPr.list) %in% IncompleteTickers] <- IncompHistPrices
  }
  
  # Convert Hist.Prices list into xts
  HistPr.list_t <- HistPr.list[names(HistPr.list) %in% subPF_t$Ticker] %>% 
    lapply(convertHistPr2xts) %>% 
    lapply(function(x) x[paste(start.date, end.date,sep="::")])
  
  # Mkt Index. One index per date
  MKT.DF <- convertHistPr2xts(MktPr)[paste(start.date, end.date, sep="::")] %>% 
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
      # MKTadj = 50% of the Market performance
      # Example: MKT down 20%; MKTadj = 1+((0.8-1)/2) = 0.9
      # LossThreshold = 0.1; (1-0.1)*0.9 = 0.81
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

# Building SLMonth 
# Adding a YearMonth label into SLhist to 
SLMonth <- SLhist %>% 
  mutate(YearMonth = paste0(year(Date), month(Date)))

Date2YearMonth <- unique(SLMonth[, c("Date", "YearMonth")])

# Same dimension as RQMhist
# NA could not be the same because the SL starts at the begining of the month.
SLMonth <- SLMonth %>% 
  subset(Date %in% refDates) %>%
  select(-Date) %>% 
  full_join(Date2YearMonth, by = "YearMonth") %>% 
  .[, colnames(SLhist)]

stopifnot(dim(RQMhist) == dim(SLMonth))
# apply(RQMhist, 2, function(x) sum(is.na(x)))
# apply(SLMonth, 2, function(x) sum(is.na(x)))

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
stopifnot(dim(RQMhist) == dim(RQMholdingshist))

# RQM history after applying Stop-Losses
# RQMhist_SL with NAs for SL triggers for the portfolio holdings
# RQMhist_SL_perf with the SL price for performance
RQMhist_SL <- RQMhist
RQMhist_SL_perf <- RQMhist
for (i in 1:nrow(RQMhist)) {
  for (j in 2:ncol(RQMhist)) {
    # NA after selling the asset
    RQMhist_SL[i,j] <- ifelse(RQMholdingshist[i,j], RQMhist[i,j], NA)
    # SL after selling the asset
    RQMhist_SL_perf[i,j] <- ifelse(RQMholdingshist[i,j], RQMhist[i,j], SLMonth[i,j])
  }
}

# RQM portfolio performance
# For each datelabel (month), calculate the return assuming that all assets are adquired at T0 with Pr = 1.
DateLabels <- unique(unlist(lapply(strsplit(colnames(RQMhist_SL), "_"), function(x) x[2]))) %>% subset(!is.na(.))
RQMPFperformance.list <- list()
for (datelabel in DateLabels) {
  Stocks <- grep(datelabel, colnames(RQMhist_SL), value = TRUE)
  RQMPFperformance <- data.frame(Date = RQMhist_SL_perf[,1],
                                 Base = rowSums(!apply(RQMhist_SL_perf[,Stocks, drop = FALSE], 2, is.na)),
                                 PF = rowSums(RQMhist_SL_perf[,Stocks, drop = FALSE], na.rm = TRUE)) %>% 
    mutate(PFReturn = (PF / Base)) %>% 
    .[, c("Date", "PFReturn")]
  colnames(RQMPFperformance)[colnames(RQMPFperformance) == "PFReturn"] <- paste0("PFReturn", datelabel)
  
  RQMPFperformance.list[[datelabel]] <- RQMPFperformance
}

RQM <- sapply(RQMPFperformance.list, function(x) diff(x[,2])) %>% 
  as.data.frame() %>% 
  mutate(PFTotalReturn = rowMeans(., na.rm = TRUE) + 1,
            RQMcumRet = cumprod(PFTotalReturn),
            Date = RQMPFperformance.list[[1]][-1, 1]) %>% 
  left_join(MKT.DF, by = "Date")

# Output ----

# > 1 Cum Return ----
fileout <- paste(Country, "RQMcumRet", tail(RQM$Date,1), "csv", sep = ".") 
colMKT <- grep("MKT_", colnames(RQM), value = TRUE)
out1 <- RQM[ , c("Date", "RQMcumRet", colMKT)] %>% 
  set_colnames(c("Date", "RQMcumRet", "MKTcumRet")) %>%
  mutate(Since = head(RQMhist_SL$Date, 1)) 

# write.csv(out1, fileout, row.names = FALSE)
out1 %>% tail()

# # Portfolio at each Date
out2 <- apply(RQMhist_SL, 1, function(x) {
  assetsname <- names(which(!is.na(x))) %>% setdiff("Date")
  tmp <- as.numeric(x[which(!is.na(x))[-1]])
  wwe <- tmp / sum(tmp)
  wwe <- t(wwe) %>% as.data.frame(row.names = NULL) %>% 
    set_colnames(assetsname)
  wwe
})

tail(out2, 1)[[1]]



