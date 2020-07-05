
# Create a basic pie chart bar
# Assumptions: 
    # Labels column is called "Tickers"
    # values is "weight"
          
piechartplot <- function(Alloc.df, date = today()) {
  pie = ggplot(Alloc.df, aes(x="", y = weight, fill = Tickers)) + geom_bar(stat="identity", width=1)
  pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(weight*100), "%")), position = position_stack(vjust = 0.5))
  plottitle <- paste0("Portfolio as of ", today())
  pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = plottitle)
  pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                      axis.text = element_blank(),
                                      axis.ticks = element_blank(),
                                      plot.title = element_text(hjust = 0.5, color = "#666666"))
  pie
  
}

# MarketCap.df <- data.frame(Ticker = c("UNA.AS" ,"VOW3.DE", "MC.PA", "REE.MC", "PFtest"),
#                     MarketCap = c(100, 200, 85, 1500, 500),
#                     stringsAsFactors = FALSE)
# barchartPFplot(MarketCap.df, "MarketCap", PFTicker = "PFtest")
barchartPFplot <- function(df, VarName, Key = "Ticker", PFTicker = "PF") {
  
  df <- setNames(df, c("Key", "VarName"))
  ggplot(df, aes(x = reorder(Key, -VarName), 
                           y = VarName,
                           fill= factor(ifelse(Key == PFTicker, "Highlighted", "Normal")))) + 
    geom_bar(stat = "identity") +
    scale_fill_manual(name = "Key", values=c("red","grey50")) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(VarName)
  
}

