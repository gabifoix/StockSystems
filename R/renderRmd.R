require(rmarkdown)
require(dplyr)
require(YHFinR)


rmarkdown::render('PortfolioAssessment.Rmd', 
                  output_dir = "PortfolioReports",
                  output_file = paste0('PortfolioReport_', gsub("-", "", Sys.Date()), '.html'))




Ticker <- read.csv("CompanyList.20200713.csv", sep = ";") %>% 
  subset(country == "HO", select = "tickers")


SearchName <- "CAC"
Index <- "^FCHI"

Tickers<- YHFinR::getYFIndexComp(Index)$Symbol

rmarkdown::render('StockResearch.Rmd', 
                  output_dir = "StockResearch",
                  params = list(Ticker = head(unique(Tickers), 30)),
                  output_file = paste0('StockResearch_', SearchName, "_", gsub("-", "", Sys.Date()), '.html'))