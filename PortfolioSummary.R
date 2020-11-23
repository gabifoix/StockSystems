require(XLConnect)
fdatafolder <- "C:\\Users\\Usuario\\Downloads\\^GSPC.csv"

SP500 <- read.csv(fdatafolder)
SP500 <- SP500[, c("Date", "Adj.Close")]
colnames(SP500) <- c("Date", "SP500")

fdatafolder <- "C:\\Users\\Usuario\\Downloads\\^STOXX50E.csv"

EUSTOXX50 <- read.csv(fdatafolder)
EUSTOXX50 <- EUSTOXX50[, c("Date", "Adj.Close")]
colnames(EUSTOXX50) <- c("Date", "EUSTOXX50")

write.csv(merge(SP500, EUSTOXX50), "SPEU.csv", row.names = FALSE)

