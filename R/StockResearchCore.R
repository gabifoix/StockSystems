# New package: StockSystemsTools

# HistPrices.yahoo.list <- readRDS("HistPrices.yahoo.list.tmp.rds")
# HistPrices.yahoo.xts.list <- lapply(HistPrices.yahoo.list, convertHistPr2xts)
# HistPrices.wk.list <- lapply(HistPrices.yahoo.xts.list, daily2weekly)
# HistPrices.wk.ma.list <- lapply(HistPrices.wk.list, addMA)

#findLocalMin(HistPrices.wk.list[["GSY.TO"]])
findLocalMin <- function(obj, col = "adjclose") {
  Price <- obj[!is.na(obj[, col])]
  GlobalMin <- Price[Price == min(Price)]
  
  MonthlyMinPoints <- unique(xts::apply.monthly(Price, min))

  # https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
  inflect <- function(x, threshold = 1){
    up   <- sapply(1:threshold, function(n) c(x[-(seq(n))], rep(NA, n)))
    down <-  sapply(-1:-threshold, function(n) c(rep(NA,abs(n)), x[-seq(length(x), length(x) - abs(n) + 1)]))
    a    <- cbind(x,up,down)
    list(minima = which(apply(a, 1, min) == a[,1]), maxima = which(apply(a, 1, max) == a[,1]))
  }
  
  inflectPoints <- inflect(MonthlyMinPoints)
  
  LocalMin <- Price[Price %in% unique(c(min(Price), MonthlyMinPoints[inflectPoints$minima]))]
  # Only min from Global min onwards
  LocalMin <- window(LocalMin, start = date(GlobalMin), end = date(tail(obj, 1)))
  LocalMin
}


# LocalMin <- findLocalMin(HistPrices.wk.list[[70]])
# oneStock <- HistPrices.wk.list[[70]]
# oneStock <- addMA(oneStock)
# plot(oneStock,
#          ylab = "Adj Closing Value",
#          main = "ATZ.TO")
# points(LocalMin, col="red", pch=17, on=1)


getMinSlope <- function(LocalMin) {
  model <- lm(adjclose ~ seq(1:length(LocalMin)), data = LocalMin)
  minSlope <- model$coefficients[2]
  minSlope
}


daily2weekly <- function(obj, ColName = "adjclose") {
  res <- xts::to.weekly(obj[, ColName] , name = ColName) 
  colnames(res)[colnames(res) == paste(ColName, "Close", sep = ".")] <- ColName
  res[, ColName]
}

# addMA(HistPrices.wk.list[["GSY.TO"]])
addMA <- function(obj, n = 30, ColName = "adjclose", ma = "SMA") {
  if (ma == "SMA") {
    obj$MA <- TTR::runMean(obj[ ,ColName], n = n)
  }
  if (ma == "WMA") {
    obj$MA  <- TTR::WMA(obj[ ,ColName], n= n)
  }
  obj
}

# obj.xts <- xts(x=1:100, order.by = Sys.Date()-1:100)
# colnames(obj.xts) <- "MA"
# addMASlope(obj.xts)
addMASlope <- function(obj, maColName = "MA") {
  obj$MA.Slope <- NA
  rad2degree <- 180 / pi
  MA <- as.numeric(na.omit(obj[, maColName]))
  lenMA <- length(MA) - 1
  posret <- tail(which(!is.na(obj[, maColName])), lenMA)
  obj[posret, "MA.Slope"] <- sapply(seq_len(lenMA), function(i) rad2degree * atan((MA[i+1] - MA[i]) / MA[i]))
  obj
}



# calcInterMomentum(HistPrices.yahoo.xts.list$VOW3.DE)
calcGenericMomentum <- function(obj.xts, colPrice = "adjclose", term = 12, noise = 1) {
  res <- tail(cumprod(head(tail(suppressWarnings(quantmod::monthlyReturn(obj.xts[,colPrice])), term), term - noise) + 1), 1) - 1
  as.numeric(res)
}

# VarCols <- c("EV", "EBITDA", "PER", "PrBk")
# YFNamesRemap <- read.csv("MD/YFNamesRemap.csv", sep = ";") %>% 
#   subset(FinRatio != "" & FinRatio %in%  VarCols)
# FinRatOri.df <- getFinRatios(c("UNA.AS", "TGYM.MI", "VOW3.DE","PUM.DE",  "MC.PA", "CS.PA", "REE.MC"))
# FinRat.df <-  FinRatOri.df %>% 
#   renameFinRatios(as.character(YFNamesRemap$YFName), as.character(YFNamesRemap$FinRatio)) 
# rankValue(FinRat.df)
rankValue <- function(FinRat.df, VarCols = c("EV", "EBITDA", "PER", "PrBk")) {
  df <- FinRat.df %>% 
    subset(Variable %in% VarCols) %>% 
    .castnclean(VarCols) %>% 
    dplyr::mutate(EBITDA2EV = convertLetterAmount2num(EV) / convertLetterAmount2num(EBITDA)) %>% 
    rankVariables("VAL", c("PER", "PrBk", "EBITDA2EV"))
  df
}

# getFinRatios(c("UNA.AS" ,"VOW3.DE", "MC.PA", "REE.MC")) %>% 
#   getMarketCap()
getMarketCap <- function(FinRat.df, MCapCol = "MarketCap") {
  res <-  FinRat.df %>%
    renameFinRatios(c("Market Cap"), MCapCol) %>% 
    subset(Variable %in% MCapCol) %>% 
    .castnclean(MCapCol) %>% 
    dplyr::mutate(MarketCap = convertLetterAmount2num(MarketCap))
  res
}


# PF.df <- data.frame(Ticker = c("UNA.AS" ,"VOW3.DE", "MC.PA", "REE.MC"),
#                     MarketCap = c(100, 200, 85, 1500),
#                     stringsAsFactors = FALSE)
# weight.df <- data.frame(Ticker = c("UNA.AS" ,"VOW3.DE", "MC.PA", "REE.MC"),
#                         weight = c(0.1, 0.5, 0.3, 0.1),
#                         stringsAsFactors = FALSE)
# addPFavgFactor(PF.df, weight.df)
addPFavgFactor <- function(PF.df, weight.df = NULL) {
  
  NumCols <- colnames(PF.df)[sapply(PF.df, is.numeric)]
  keyCol <- setdiff(colnames(PF.df), NumCols)
  # Empty data.frame
  PF <- setNames(data.frame(matrix(ncol = ncol(PF.df), nrow = 1)), colnames(PF.df))
  PF[,keyCol] <- "PF"
  if(is.null(weight.df)) {
    # Equally weighted
    PF[1, NumCols] <- colMeans(PF.df[,NumCols, drop = FALSE], na.rm = TRUE)
  } else {
    # Weighted Portfolio
    stopifnot(nrow(PF.df) == nrow(weight.df))
    tmp <- merge(PF.df, weight.df) 
    PF[1, NumCols] <- colSums(tmp[, NumCols, drop = FALSE] * tmp$weight, na.rm = TRUE)
  }
  res <- rbind(PF.df, PF)
  res
}




     
# x = as.numeric(df$PER )
# uniformize(x, thehigherthebetter = FALSE)
uniformize <- function(x, thehigherthebetter = TRUE) {
   res <-  sapply(x, function(i) 
      (i - min(x)) / (max(x) - min(x)))
   if (!thehigherthebetter) {
     res <- 1 - res
   }
   res
}
          
# VarCols = c("ROE", "OpMar")
# FinRat.df <- getFinRatios(c("UNA.AS", "TGYM.MI", "VOW3.DE","PUM.DE",  "MC.PA", "CS.PA", "REE.MC"))
# FinRat.df <-  FinRat.df %>% 
#   renameFinRatios(c("Return on Equity", "Operating Margin"), c("ROE", "OpMar")) 
rankProfitability <- function(FinRat.df, VarCols = VarCols) {
  df <- FinRat.df %>% 
    subset(Variable %in% VarCols) %>% 
    dplyr::mutate(Value = convertPer2num(Value)) %>% 
    .castnclean(VarCols) %>% 
    rankVariables("Profit", VarCols)
  df
}

rankBS <- function(FinRat.df, VarCols = c("CashSH", "D2E", "CR", "MA50")) {
  df <- FinRat.df %>% 
    subset(Variable %in% VarCols) %>% 
    dplyr::mutate(Value = as.numeric(Value)) %>% 
    .castnclean(VarCols) %>% 
    mutate(CastR = CashSH / MA50) %>% 
    rankVariables("BS", c("CastR", "D2E", "CR"))
  df
}



#' Rank a df considering all variables defined in VarCols
#'
#' @param df 
#' @param VarCols 
#'
#' @return `data.frame` with the same dim as df
#'
#' @examples
# df <- data.frame(Ticker = letters[1:10],
#                 aa = seq(1:10),
#                 bb = c(0.23, 0.45, 0.56, 1.1, 0.032, 0.8, 0.7, 0.1, 0.9, 1.5))
# rankVariables(df, VarCols = c("aa", "bb"))
rankVariables <- function(df, Rank.Name = "Final_rank", VarCols, KeepVarCols = FALSE){
  rank.df <- df %>%
    dplyr::mutate_at(VarCols, rank) %>% 
    dplyr::mutate(Final_rank = rank(rowSums(.[, VarCols, drop = FALSE]))) %>% 
    dplyr::select(Ticker, Final_rank, VarCols)
  colnames(rank.df) <- c("Ticker", Rank.Name, VarCols)
  if (!KeepVarCols)
    rank.df <- rank.df[, c("Ticker", Rank.Name)] 
  
  rank.df
}


#' Calculates the Stop Loss of all elements of the portfoio given a global threshold
#'
#' @param PF 
#' @param LossThreshold 
#' @param ProfitDropThreshold 
#'
#' @return
#' @export
#'
#' @examples 
#' PF <- data.frame(Ticker = letters[1:8],
#' nShares = c(100, 200, 100, 10, 15, 500, 1200, 86),
#' CP = c(50, 40, 43, 598, 256, 8, 3.656, 95),
#' AP = c(48, 25, 38, 254, 100, 9, 4.26, 42),
#' MA = c(49, 35, 44, 500, 200, 7, 3.59, 94))
#' calcStopLoss(PF, LossThreshold = 0.15, ProfitDropThreshold = 0.25)
calcStopLoss <- function(PF, LossThreshold = 0.2, ProfitDropThreshold = 0.3, MAbuffer = 0.05) {
  CVPF <- sum(PF$nShares * PF$CP)
  MaxLoss <- LossThreshold / nrow(PF)
  MaxProfitDrop <- ProfitDropThreshold / nrow(PF)
  
  PF %>% 
    mutate(CV = nShares * CP,
           PnL = nShares * (CP - AP),
           StopLossDefAmount = CVPF * MaxLoss,
           PFStopLoss = ifelse(PnL <= 0, AP - (StopLossDefAmount / nShares), 
                             ifelse(PnL > 0 & PnL > StopLossDefAmount, 
                                    CP- (CVPF * MaxProfitDrop / nShares),
                                    CP- (StopLossDefAmount / nShares))),
           PrStopLoss = MA * (1 - MAbuffer),
           StopLoss = pmax(PFStopLoss, PrStopLoss),
           LossMargin = 1- PFStopLoss/CP)
  
}

calcTrStopLoss <- function(PF, LossThreshold = 0.15, ProfitDropThreshold = 0.2) {
  PF %>% 
    mutate(maxreturn = (MAX/AP) - 1,
           StopLoss = ifelse(maxreturn > ProfitDropThreshold, 
                             MAX*(1-ProfitDropThreshold), 
                             AP * (1-LossThreshold)))
}

#feesOptimalSpots(10000, 10, 0.01, 100)
feesOptimalSpots <- function(K, Min, PerFee, Max, MaxAssetsDiverNull = 25, MinAssets = 3) {
  MinInvest <- Min / (PerFee/100)
  max(min(floor(K / MinInvest), MaxAssetsDiverNull), MinAssets)
}

# feesTotal(3, 30000, 10, 0.025, 100)
feesTotal <- function(Nspots, K, Min, PerFee, Max) {
  Invest <- floor(K / Nspots)
  Nspots * min(max(Min, Invest * (PerFee/100)), Max)
}

# Find maximum since adquisition date
maxfromADate <- function(one.Ticker, Portfolio, HistPrices.list) {
  start.date <- as.POSIXct(Portfolio$ADate[Portfolio$Ticker == one.Ticker])
  end.date <- as.POSIXct(today())
  obj <- HistPrices.list[[one.Ticker]]
  res <- max(obj[start.date <= index(obj) & index(obj) <= end.date]$adjclose)
  res
}

# Buying / Selling signal
getSignal <- function(obj, thresholdMA_SELL = 0.01, thresholdMA_BUY = 0.09,  thresholdSlope_SELL = -0.2, thresholdSlope_BUY = 0.2) {
  x <- tail(obj, 1)
  ifelse(x$adjclose < ((1 -thresholdMA_SELL) * x$MA) & x$MA.Slope < thresholdSlope_SELL, "SELL", 
         ifelse(x$adjclose < ((1 + thresholdMA_BUY) * x$MA) & x$adjclose > x$MA & x$MA.Slope > thresholdSlope_BUY, "BUY", "HOLD"))
}

extract_CF <- function(CF) {
  res <- lapply(CF, function(x) {
    df <- data.frame(CFOpeAct = .cleanNAs(head(x[[1]]$totalCashFromOperatingActivities$raw, 1)),
                     CapExp = .cleanNAs(head(x[[1]]$capitalExpenditures$raw, 1)),
                     stringsAsFactors = FALSE)
    if (nrow(df) == 1) {
      df$FCF = df$CFOpeAct + df$CapExp
    } else {
      df <- data.frame(CFOpeAct = 0,
                       CapExp = 0,
                       FCF = 0)
    }
    df
  } )
  bind_rows(res, .id = "Ticker")
}

extract_BS <- function(BS) {
  res <- lapply(BS, function(x) {
    df <- data.frame(Debt = .cleanNAs(head(x[[1]]$totalLiab$raw, 1)),
                     Assets = .cleanNAs(head(x[[1]]$totalAssets$raw, 1)),
                     Equity = .cleanNAs(head(x[[1]]$totalStockholderEquity$raw, 1)),
                     Cash = .cleanNAs(head(x[[1]]$cash$raw, 1)),
                     stringsAsFactors = FALSE)
    if (nrow(df) != 1) {
      df <- data.frame(Debt = 0,
                       Assets = 0,
                       Equity = 0,
                       Cash = 0)
    } 
    df
  } )
  bind_rows(res, .id = "Ticker")
}

extract_IS <- function(IS) {
  res <- lapply(IS, function(x) {
    df <- data.frame(netIncome = .cleanNAs(head(x[[1]]$netIncome$raw, 1)),
                     Sales = .cleanNAs(head(x[[1]]$totalRevenue$raw, 1)),
                     stringsAsFactors = FALSE)
    if (nrow(df) != 1) {
      df <- data.frame(netIncome = 0,
                       Sales = 0)
    } 
    df
  } )
  bind_rows(res, .id = "Ticker")
}

extract_KS <- function(KS) {
  # SBO.VI
   res <- lapply(KS, function(x) {
    # message(xn)
    # x = KS[xn]
    if (all(c("floatShares", "enterpriseValue", "trailingEps") %in% names(x)) &
        c("raw" %in% colnames(x[1,1]))) {
      df <- data.frame(Shares = .cleanNAs(x$floatShares$raw),
                       EV = .cleanNAs(x$enterpriseValue$raw, 1),
                       EPS = .cleanNAs(x$trailingEps$raw),
                       stringsAsFactors = FALSE)
    } else {
    # df <- data.frame(Shares = .cleanNAs(x$floatShares$raw),
    #                  EV = .cleanNAs(x$enterpriseValue$raw, 1),
    #                  EPS = .cleanNAs(x$trailingEps$raw),
    #                  stringsAsFactors = FALSE)
    # if (nrow(df) != 1) {
      df <- data.frame(Shares = 0,
                       EPS = 0,
                       EV = 0)
    } 
    df
  } )
  bind_rows(res, .id = "Ticker")
}

extract_FB <- function(FB) {
  res <- lapply(FB, function(x) {
    if (all(c("ebitda", "returnOnAssets", "returnOnEquity", "freeCashflow") %in% names(x)) &
        c("raw" %in% colnames(x[1,1]))) {
      df <- data.frame(ebitda = .cleanNAs(x$ebitda$raw),
                       returnOnAssets = .cleanNAs(x$returnOnAssets$raw),
                       returnOnEquity = .cleanNAs(x$returnOnEquity$raw),
                       freeCashflow = .cleanNAs(x$freeCashflow$raw),
                       stringsAsFactors = FALSE)
      
    } else {
      df <- data.frame(ebitda = 0,
                       returnOnAssets = 0,
                       returnOnEquity = 0,
                       freeCashflow = 0)
    } 
    df
  } )
  bind_rows(res, .id = "Ticker")
}



extract_HistPr <- function(HistPr) {
  res <- lapply(HistPr, function(x) {
    df <- data.frame(CP = .cleanNAs(tail(x$adjclose, 1)),
                     StartP = .cleanNAs(head(x$adjclose, 1)),
                     MinP = .cleanNAs(min(x$adjclose, na.rm = TRUE)),
                     MaxP = .cleanNAs(max(x$adjclose, na.rm = TRUE)),
                     Vol = .cleanNAs(mean(x$volume, na.rm = TRUE)),
                     MA = .cleanNAs(mean(x$adjclose, na.rm = TRUE)),
                     stringsAsFactors = FALSE)
    if (nrow(df) == 0) {
      df <- data.frame(CP = 0,
                       StartP = 0,
                       MinP = 0,
                       MaxP = 0,
                       Vol = 0,
                       MA = 0)
    }
    df
  } )
  bind_rows(res, .id = "Ticker")
}



filter6m <- function(HistPr.list) {
  idx <- sapply(HistPr.list, nrow) %>% unlist() > 252
  idx2 <- unlist(lapply(HistPr.list, ncol)) == 4
  idx3 <- unlist(lapply(HistPr.list, function(x) all(c("adjclose", "Date") %in% colnames(x))))
  HistPr.f.list <- HistPr.list[idx3]
  Pre6m <- seq(as.Date(tail(HistPr.f.list[[1]]$Date, 1)), length = 2, by = "-6 months")[2]
  rmOld <- HistPr.f.list[[1]]$Date > Pre6m
  HistPr.f.6m.list <- lapply(HistPr.f.list, function(x) {x <- x[rmOld, ]} )
  HistPr.f.6m.list <- lapply(HistPr.f.6m.list, function(x) {x <- subset(x, adjclose > 0)})
}


# Utils ----

# Stock Research format
.SRformat <- function(obj, colname, TickerNames = NULL){
  if(is.null(TickerNames)) {
    res <- obj %>% 
      as.data.frame() %>% 
      set_colnames(colname) %>% 
      mutate(Ticker = rownames(.))
    
  } else {
    res <- obj %>% 
      as.data.frame() %>% 
      set_colnames(colname) %>% 
      mutate(Ticker = TickerNames)
    
  }
  res
}

#convertHistPr2xts(HistPrices.yahoo.list$REE.MC)
convertHistPr2xts <- function(df, DataCols = c("volume", "close", "adjclose"), DateCol = "Date") {
  df$datetime <- as.POSIXct(strptime(df[, DateCol], format = "%Y-%m-%d"))
  res <- as.xts(df[ ,DataCols], order.by = df$datetime)
  res
}

cleanNArows <- function(HistPrices.yahoo.list) {
  #cols2check <- grep("[A-Z]", colnames(HistPrices.yahoo.list[[1]]), invert = TRUE, value = TRUE)
  res <- lapply(HistPrices.yahoo.list, function(obj) {
    cols2check <- grep("[A-Z]", colnames(obj), invert = TRUE, value = TRUE)
    idx <- !apply(obj[ ,cols2check], 1, function(x) all(is.na(x)))
    obj[idx,]
  })
  names(res) <- names(HistPrices.yahoo.list)
  res
}


.castnclean <- function(df, VarCols) {
  res <- reshape2::dcast(df, Ticker ~ Variable, value.var = 'Value')
  res[,VarCols] <- .cleanNAs(res[,VarCols])
  res
}



.cleanNAs  <- function(x, replace = 0) { 
  x[is.null(x)] <- replace
  x[is.na(x)] <- replace
  x
} 

.growth3 <-  function(t, x = 0.03 ) {
  cgr <- (1 + x)^(t)
  cgr 
} 

.cleaNA_Neg <- function(x, replace = 0, rplneg = NULL) { 
  if(!is.null(rplneg)) {
    x[x < 0] <- rplneg
  }
  x[is.null(x)] <- replace
  x[is.na(x)] <- replace
  x[is.infinite(x)] <- replace
  x
} 



xts2df = function(x) {
  df = data.frame(x = index(x), coredata(x)) %>%
    set_colnames(c("Date", strsplit(colnames(x),"[.]")[[1]][2]))
  return(df)
}





industry=function(ticker)
{
  url <- paste0("https://in.finance.yahoo.com/quote/", ticker, "/profile?ltr=1")
  
  mydat <- read.csv(textConnection(RCurl::getURL(url)), header=T)
  mydat[117]
  one.ticker.data <- RCurl::getURL(url)
  readLines(one.ticker.data)
  
  rawHistPrice <- sub("NaN", "\"NA\"" ,one.ticker.data) %>%
    jsonlite::fromJSON(one.ticker.data)
  

  mydata=as.data.frame(readLines(url))
  names(mydata)="text"
  ind=stringr::str_match(as.character(mydata$text[117]),'(?:<b>Industry: ?)(.*?)(?:<)')[,2]
  ind=str_replace_all(ind,'&amp;','&')
  return(ind)
}

