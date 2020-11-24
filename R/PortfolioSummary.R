require(XLConnect)
require(dplyr)
require(ggplot2)

# # Original step  ---- 
# fdatafolder <- "C:\\Users\\Gabi\\Downloads\\^GSPC.csv"
# SP500 <- read.csv(fdatafolder)
# SP500 <- SP500[, c("Date", "Adj.Close")]
# colnames(SP500) <- c("Date", "SP500")
# 
# fdatafolder <- "C:\\Users\\Gabi\\Downloads\\^STOXX50E.csv"
# 
# EUSTOXX50 <- read.csv(fdatafolder)
# EUSTOXX50 <- EUSTOXX50[, c("Date", "Adj.Close")]
# colnames(EUSTOXX50) <- c("Date", "EUSTOXX50")

# write.csv(merge(SP500, EUSTOXX50), "OldFiles\\SP500_EUSTOXX50.csv", row.names = FALSE)

SP500_EUSTOXX50 <- read.csv("OldFiles\\SP500_EUSTOXX50.csv")
# SP500_EUSTOXX50 <- merge(SP500, EUSTOXX50)


HistPF <- read.csv2("OldFiles\\HistoricalPerformance.csv", check.names = FALSE) %>% 
  mutate(Date = gsub(",", "-", Date))

PF <- HistPF[, c("Date", "Portfolio")] %>% 
  left_join(SP500_EUSTOXX50, by = "Date") %>% 
  mutate(Date = as.Date(Date),
         SP500 = 100 *(SP500 / head(SP500, 1)),
         EUSTOXX50 = 100 *(EUSTOXX50 / head(EUSTOXX50, 1)))


reshape2::melt(PF, id = "Date", value.name = "IdxValue", variable.name = "Idx") %>% 
  ggplot(aes(Date, IdxValue)) + 
  geom_line(aes(group = Idx, color = Idx), size = 1) +
  geom_hline(yintercept = 100, linetype="dashed", color = "black") +
  ggtitle("Portfolio Performance") +
  ylab("") + xlab("") +
  theme(legend.title = element_blank())


ggplot(PF, aes(x= Date, group = 1)) +
  ggtitle("Portfolio Performance") +
  geom_line(aes(y = SP500), color = "salmon", size = 0.8) + 
  geom_line(aes(y = EUSTOXX50), color="blue", size = 0.8) +
  geom_line(aes(y = Portfolio), color = "black", size = 1.2) +
  geom_hline(yintercept = 100, linetype="dashed", color = "red") +
  ylab("") + xlab("") +
  scale_x_date(date_labels = "%m/%Y") 


ww <- tail(HistPF[, !colnames(HistPF) %in% c("Date", "Portfolio")], 1)  %>% 
  reshape2::melt(value.name = "weight", variable.name = "Allocation")

ggplot(ww, aes(x="", y = weight, fill = Allocation)) + 
  geom_bar(stat="identity", width = 1) +
  ylab("") + xlab("") +
  scale_fill_brewer(palette = "Paired")


ww <- HistPF[, colnames(HistPF) != "Portfolio"] %>% 
  mutate(Date = as.Date(Date)) %>% 
  reshape2::melt(id = "Date")

ggplot(ww, aes(x= Date, group = 1)) +
  theme(legend.title = element_text(colour="blue", size=16, face="bold")) +
  ggtitle("Portfolio Performance") +
  geom_bar(aes(y = StocksEUR), color = "salmon", size = 0.8) + 
  geom_bar(aes(y = Cash), color="blue",size = 0.8) +
  ylab("") + xlab("") +
  scale_x_date(date_labels = "%m/%Y")


