library(YHFinR)
library(magrittr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(xts)
library(quantmod)
source('R/StockResearchCore.R')


# Config ----

Ref_folder <- "Source"

Candidates.df <- read.csv("MD/CompaniesUS.20210319.csv", sep = ";", stringsAsFactors = FALSE) %>%
  subset(Sector != "NAYF" & Sector != "" ) %>%
  .[, c("Ticker", "CompanyName", "Country", "Sector", "Industry")] %>%
  unique()

# Candidates.df <- read.csv("MD/CompaniesEUR.20210429.csv", sep = ";", stringsAsFactors = FALSE) %>%
#     subset(Sector != "NAYF" & Sector != "" ) %>%
#     .[, c("Ticker", "CompanyName", "Country", "Sector", "Industry")] %>%
#     unique()
  

# Import ----
Files <- list.files(Ref_folder)

FB.list <- readRDS(file.path(Ref_folder, grep("FinBasic.list", Files, value = TRUE)))
KS.list <- readRDS(file.path(Ref_folder, grep("KeyStats.list", Files, value = TRUE)))
CF.list <- readRDS(file.path(Ref_folder, grep("CF.list", Files, value = TRUE)))
BS.list <- readRDS(file.path(Ref_folder, grep("BS.list", Files, value = TRUE)))
IS.list <- readRDS(file.path(Ref_folder, grep("IS.list", Files, value = TRUE)))
HistPr.list <- readRDS(file.path(Ref_folder, grep("HistPr.list", Files, value = TRUE)))

# Extract info ----
HistPr.f.6m.list <- filter6m(HistPr.list)
PR <- extract_HistPr(HistPr.f.6m.list)
rm(HistPr.list)

FB <- extract_FB(FB.list)
rm(FB.list)

KS <- extract_KS(KS.list)
rm(KS.list)

CF <- extract_CF(CF.list)
rm(CF.list)

BS <- extract_BS(BS.list)
rm(BS.list)

IS <- extract_IS(IS.list)
rm(IS.list)

# Calcs ----
# > Factor 1: Quality ----
# >> Healthy debts ----
# BS:D / CF:FCF
Base.df <- Candidates.df %>% 
  left_join(PR, by = "Ticker") %>% 
  left_join(KS, by = "Ticker") %>% 
  left_join(FB, by = "Ticker") %>% 
  left_join(BS, by = "Ticker") %>% 
  left_join(CF, by = "Ticker") %>% 
  left_join(IS, by = "Ticker") %>%
  mutate(MCap = Shares * CP,
         Debts = Debt / CFOpeAct,
         ROA = ifelse(returnOnAssets != 0, returnOnAssets, netIncome / Assets),
         FCFyield  = FCF / EV,
         EPS = ifelse(EPS != 0, EPS, netIncome / Shares), 
         PER = CP / EPS,
         StartP = ifelse(StartP !=0, StartP, CP),
         Pr6m = CP / StartP,
         Signal = ifelse(CP > 0.99 * MA & CP < MA * 1.09, "BUY", "HOLD")) 
    
# Rank
Base.df <- Base.df %>% 
  subset(!is.na(MCap) & CP > 0 & PER > 0) %>% 
  subset(MCap < 10000000000 & MCap > 500000000)


MomentumRank <- Base.df %>% 
  rankVariables(Rank.Name = "Momentum", VarCols = c("Pr6m")) %>% 
  mutate(N_Momentum = 100 *(Momentum / max(Momentum)))

QualityRank <- Base.df %>%
  mutate(FCF2D = 1/ Debts,
         FCF2EV = FCFyield) %>% 
  rankVariables(Rank.Name = "Quality", VarCols = c("FCF2D", "ROA","FCF2EV")) %>% 
  mutate(N_Quality = 100 *(Quality / max(Quality)))

Final <- Base.df %>% 
  left_join(QualityRank, by = "Ticker") %>% 
  left_join(MomentumRank, by = "Ticker")

FinalRank <- Final %>% 
  rankVariables(VarCols = c("Quality", "Momentum")) %>% 
  mutate(N_Final = 100 *(Final_rank / max(Final_rank))) 

Final <- Final %>% 
  left_join(FinalRank, by = "Ticker") %>% 
  arrange(desc(Final_rank))


SearchName <- "US_Medium"
output_file <- paste0('StockResearch/StockResearch_', SearchName, "_", gsub("-", "", Sys.Date()), '.csv')
write.csv(Final, output_file, row.names = FALSE)

