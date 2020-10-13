# Data sources

# Euronext: Amsterdam, Brussels, Dublin, Lisbon, London, Milan, Oslo and Paris
# https://live.euronext.com/en/products/equities/list

# NYSE
# http://eoddata.com/stocklist/NYSE/B.htm

# NASDAQ
# http://eoddata.com/stocklist/NYSE/B.htm?e=NYSE&l=B





require(dplyr)

.removedupli <- function(CompaniesList) {
  idxdupli <- duplicated(CompaniesList$tickers)
  
  todelete <- CompaniesList[idxdupli,] %>% 
    subset(industry == "") %>% 
    mutate(todelete = TRUE)
  
  CompaniesList_new <- CompaniesList %>% 
    left_join(todelete, by = c("CompanyName", "tickers", "country", "industry", "ISIN")) %>% 
    subset(is.na(todelete)) %>% 
    .[, c("CompanyName", "tickers", "country", "industry", "ISIN")]
  
  CompaniesList_new
}


CompaniesList <- read.csv("CompanyList.20200703.csv", sep = ";")

CompaniesList_new <- .removedupli(CompaniesList)

write.csv2(CompaniesList_new2, "CompanyList.20200713.csv", row.names = FALSE)







Mapping <- data.frame(Market = c("Paris", "Amsterdam", "Brussels", "Dublin", "Lisbon"),
                      Append = c("PA", "AS", "BR", "IR", "LS"),
                      Country = c("FR", "HO", "BE", "IR", "PO"),
                      stringsAsFactors = FALSE)

Euronext_Equities <- read.csv("MD/Euronext_Equities_2020-07-13.csv", stringsAsFactors = FALSE)

Euronext_Equities <- Euronext_Equities %>% 
  subset(Symbol != "-" & Volume != "-") %>% 
  mutate(Volume = as.numeric(Volume)) %>% 
  subset(Volume > 999) %>% 
  subset(grepl("10.07.2020", Last.Date.Time))

Euronext_Equities <- Euronext_Equities %>% 
  mutate(Market2 = sapply(strsplit(Market, ","), function(x) x[1]),
         Market2 = gsub("Growth ", "", gsub("Euronext ", "", Market2))) %>% 
  left_join(Mapping, by = c("Market2" = "Market")) %>% 
  mutate(tickers = paste(Symbol, Append, sep = "."),
         industry = "") %>% 
  subset(!is.na(Append)) %>% 
  rename(CompanyName = Name, country = Country) %>% 
  .[, c("CompanyName", "tickers", "country", "industry", "ISIN")]


# TODO: Find equity 

CompaniesList_new2 <- rbind(CompaniesList_new, Euronext_Equities) %>% 
  group_by(tickers, country) %>% 
  summarize(CompanyName = firstNonEmpty(CompanyName),
            industry = firstNonEmpty(industry),
            ISIN = firstNonEmpty(ISIN)) %>% 
  .[, c("CompanyName", "tickers", "country", "industry", "ISIN")]

firstNonEmpty <- function(x)  x[which(x != "" & !is.na(x))[1]]


