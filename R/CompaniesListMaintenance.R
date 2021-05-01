# Data sources

# Euronext: Amsterdam, Brussels, Dublin, Lisbon, London, Oslo and Paris
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


complete_Profile <- function(Tickers.df) {
  
  CompProfile <- YHFinR::getYFProfile(Tickers.df$Ticker)
  
  Country.df <- data.frame(Reduce(rbind, sapply(CompProfile, function(x) ifelse(is.null(x$country), "NAYF", x$country)))) %>% 
    set_colnames(c("Country")) %>% 
    mutate(Ticker = names(CompProfile))
  
  Sector.df <- data.frame(Reduce(rbind, sapply(CompProfile, function(x) ifelse(is.null(x$sector), "NAYF", x$sector)))) %>% 
    set_colnames(c("Sector")) %>% 
    mutate(Ticker = names(CompProfile))
  
  Industry.df <- data.frame(Reduce(rbind, sapply(CompProfile, function(x) ifelse(is.null(x$industry), "NAYF", x$industry)))) %>% 
    set_colnames(c("Industry")) %>% 
    mutate(Ticker = names(CompProfile))
  
  CompProfile.df <- Tickers.df %>% 
    left_join(Country.df, by = "Ticker") %>% 
    left_join(Sector.df, by = "Ticker") %>% 
    left_join(Industry.df, by = "Ticker")
  
  CompProfile.df
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



Tickers.df <- rbind( read.csv("MD/NYSE.20201010.csv", sep = ",", stringsAsFactors = FALSE),
                     read.csv("MD/NASDAQ.20201010.csv", sep = ",", stringsAsFactors = FALSE), 
                     read.delim("MD/AMEX.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE))  %>% 
  dplyr::rename(Ticker = Symbol, CompanyName = Description)
Tickers.df <- Tickers.df[!grepl("[A-Z]-[A-Z]", Tickers.df$Ticker),]
Tickers.df <- Tickers.df[!grepl("%", Tickers.df$Ticker),]


US <- complete_Profile(Tickers.df)

write.csv2(US, "CompaniesUS.20210319.csv")

CompaniesEUR <- read.csv("MD/CompaniesEUR.20210103.csv", sep = ";") %>% 
  .[,c("CompanyName", "tickers")] %>% 
  dplyr::rename(Ticker = tickers)
EUR <- complete_Profile(CompaniesEUR)

write.csv2(EUR, "CompaniesEUR.20210429.csv")

