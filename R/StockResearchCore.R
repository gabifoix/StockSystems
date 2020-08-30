
daily2weekly <- function(obj, ColName = "adjclose") {
  res <- xts::to.weekly(obj[, ColName] , name = ColName) 
  colnames(res)[colnames(res) == paste(ColName, "Close", sep = ".")] <- ColName
  res[, ColName]
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
#'df <- data.frame(Ticker = letters[1:10],
#'                 aa = seq(1:10),
#'                 bb = c(0.23, 0.45, 0.56, 1.1, 0.032, 0.8, 0.7, 0.1, 0.9, 1.5))
#'rankVariables(df, VarCols = c("aa", "bb"))
rankVariables <- function(df, Rank.Name = "Final_rank", VarCols){
  
  rank.df <- df %>%
    dplyr::mutate_at(VarCols, rank) %>% 
    dplyr::mutate(Final_rank = rank(rowSums(.[, VarCols]))) %>% 
    dplyr::select(Ticker, Final_rank)
  colnames(rank.df) <- c("Ticker", Rank.Name)
  
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
#' AP = c(48, 25, 38, 254, 100, 9, 4.26, 42))
#' calcStopLoss(PF, LossThreshold = 0.15, ProfitDropThreshold = 0.25)
calcStopLoss <- function(PF, LossThreshold = 0.2, ProfitDropThreshold = 0.3) {
  CVPF <- sum(PF$nShares * PF$CP)
  MaxLoss <- LossThreshold / nrow(PF)
  MaxProfitDrop <- ProfitDropThreshold / nrow(PF)
  
  PF %>% 
    mutate(CV = nShares * CP,
           PnL = nShares * (CP - AP),
           StopLossDefAmount = CVPF * MaxLoss,
           StopLoss = ifelse(PnL <= 0, AP - (StopLossDefAmount / nShares), 
                             ifelse(PnL > 0 & PnL > StopLossDefAmount, 
                                    CP- (CVPF * MaxProfitDrop / nShares),
                                    CP- (StopLossDefAmount / nShares))),
           LossMargin = 1- StopLoss/CP)
  
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



# Utils ----

.castnclean <- function(df, VarCols) {
  res <- reshape2::dcast(df, Ticker ~ Variable, value.var = 'Value')
  res[,VarCols] <- .cleanNAs(res[,VarCols])
  res
}



.cleanNAs  <- function(x, replace = 0) { 
  x[is.na(x)] <- replace
  x
} 

.growth3 <-  function(t, x = 0.03 ) {
  cgr <- (1 + x)^(t)
  cgr 
} 

.cleaNA_Neg <- function(x, replace = 0) { 
  x[x < 0] <- replace
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

