# Data sources

# Euronext: Amsterdam, Brussels, Dublin, Lisbon, London, Milan, Oslo and Paris
# https://live.euronext.com/en/products/equities/list

# NYSE
# http://eoddata.com/stocklist/NYSE/B.htm

# NASDAQ
# http://eoddata.com/stocklist/NYSE/B.htm?e=NYSE&l=B


# https://www.deutsche-boerse-cash-market.com/dbcm-en/instruments-statistics/statistics/listes-companies



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

firstNonEmpty <- function(x)  x[which(x != "" & !is.na(x))[1]]


# CompaniesEUR Ori ----

CompaniesEUR <- read.csv("CompaniesEUR.20201229.csv", sep = ";")

CompaniesEUR_new <- .removedupli(CompaniesEUR)

# write.csv2(CompaniesList_new2, "CompanyList.20200713.csv", row.names = FALSE)


# Euronext_Equities ----
Mapping <- data.frame(Market = c("Paris", "Amsterdam", "Brussels", "Dublin", "Lisbon"),
                      Append = c("PA", "AS", "BR", "IR", "LS"),
                      Country = c("FR", "HO", "BE", "IR", "PO"),
                      stringsAsFactors = FALSE)

Euronext_Equities <- read.csv("MD/Euronext_Equities_2020-12-30.csv", sep = ";", stringsAsFactors = FALSE)

Euronext_Equities <- Euronext_Equities %>% 
  subset(Trading.Currency == "EUR") %>% 
  mutate(Volume = as.numeric(Volume)) %>% 
  subset(Volume > 99) 

Euronext_Equities <- Euronext_Equities %>% 
  mutate(Market2 = sapply(strsplit(Market, ","), function(x) x[1]),
         Market2 = gsub("Growth ", "", gsub("Euronext ", "", Market2))) %>% 
  left_join(Mapping, by = c("Market2" = "Market")) %>% 
  mutate(tickers = paste(Symbol, Append, sep = "."),
         industry = "") %>% 
  subset(!is.na(Append)) %>% 
  rename(CompanyName = Name, country = Country) %>% 
  .[, c("CompanyName", "tickers", "country", "industry", "ISIN")]



CompaniesEUR_new2 <- rbind(CompaniesEUR_new, Euronext_Equities) %>% 
  group_by(tickers, country) %>% 
  summarize(CompanyName = firstNonEmpty(CompanyName),
            industry = firstNonEmpty(industry),
            ISIN = firstNonEmpty(ISIN)) %>% 
  .[, c("CompanyName", "tickers", "country", "industry", "ISIN")]


CompaniesEUR_new3 <- .removedupli(CompaniesEUR_new2)

# write.csv2(CompaniesEUR_new3, "CompaniesEUR.20210103.csv", row.names = FALSE)


