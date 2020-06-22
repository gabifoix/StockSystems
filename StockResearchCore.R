VarCols = c("ROE", "OpMar")
FinRat.df <- getFinRatios(c("UNA.AS", "TGYM.MI", "VOW3.DE","PUM.DE",  "MC.PA", "CS.PA", "REE.MC"))
FinRat.df <-  FinRat.df %>% 
  renameFinRatios(c("Return on Equity", "Operating Margin"), c("ROE", "OpMar"))
                  
df <- FinRat.df %>% 
  subset(Variable %in% VarCols) %>% 
  dplyr::mutate(Value = convertPer2num(Value))

rankVariables(df, VarCols)
rankVariables <- function(df, VarCols){
  
  Variables.df <- reshape::cast(df, Ticker ~ Variable, mean, value = 'Value')
  Variables.df[,VarCols] <- .cleanNAs(Variables.df[,VarCols])
  
  rank.df <- Variables.df %>%
    dplyr::mutate_at(VarCols, rank) %>% 
    dplyr::mutate(Final_rank = rank(rowSums(.[, VarCols]))) %>% 
    dplyr::select(Ticker, Final_rank)
  
  rank.df
}





# Utils ----




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

