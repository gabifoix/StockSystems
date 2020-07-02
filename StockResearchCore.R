
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
     
x = as.numeric(df$PER )
uniformize(x, thehigherthebetter = FALSE)
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

rankProfitability(FinRat.df) %>% 
  left_join(rankBS(FinRat.df), by = "Ticker") %>% 
  rankVariables("Total", c("Profit", "BS"))
  

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


xts2df = function(x) {
  df = data.frame(x = index(x), coredata(x)) %>%
    set_colnames(c("Date", strsplit(colnames(x),"[.]")[[1]][2]))
  return(df)
}




a <- xml2::read_xml(url)



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

