
rmarkdown::render('PortfolioAssessment.Rmd', 
                  output_dir = "PortfolioReports",
                  output_file = paste0('PortfolioReport_', gsub("-", "", Sys.Date()), '.html'))




Ticker <- c("UNA.AS", "DGV.MI", "VOW3.DE","PUM.DE",  "MC.PA", "CS.PA", "TEF.MC", "SAN.MC", "AAPL", "FP.PA")
rmarkdown::render('StockResearch.Rmd', 
                  output_dir = "StockResearch",
                  params = list(Ticker = Ticker),
                  output_file = paste0('StockResearch', gsub("-", "", Sys.Date()), '.html'))