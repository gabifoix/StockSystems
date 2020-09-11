
# Create a basic pie chart bar
# Assumptions: 
    # Labels column is called "Tickers"
    # values is "weight"
          
piechartplot <- function(Alloc.df, date = today()) {
  pie = ggplot(Alloc.df, aes(x="", y = weight, fill = Ticker)) + geom_bar(stat="identity", width=1)
  pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(weight*100), "%")), position = position_stack(vjust = 0.5))
  plottitle <- paste0("Portfolio as of ", today())
  pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = plottitle)
  pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                      axis.text = element_blank(),
                                      axis.ticks = element_blank(),
                                      plot.title = element_text(hjust = 0.5, color = "#666666"))
  pie
  
}

# test.df <- data.frame(Ticker = c("UNA.AS" ,"VOW3.DE", "MC.PA", "REE.MC", "PFtest"),
#                     test = c(0.025156, 0.1556, 0.26569, 0.002021, 0.41235),
#                     stringsAsFactors = FALSE)
# barchartPFplot(test.df, "test", PFTicker = "PFtest", labelPer = TRUE)
barchartPFplot <- function(df, VarName, Key = "Ticker", PFTicker = "PF", nround = 2, labelPer = FALSE) {
 
  # Prepare df
  df <- df[, c(Key, VarName)] %>%  
    setNames(c("Key", "VarName")) %>% 
    dplyr::arrange(desc(VarName)) 
  
  # Prepare labels
  if (labelPer) {
    labelTop <- paste0(round(df$VarName * 100, nround), "%")
    titleTop <- paste0(VarName, "%")
  } else {
    labelTop <- round(df$VarName, nround)
    titleTop <- VarName
  }
  
  # Plot
  ggplot(df, aes(x = reorder(Key, -VarName), 
                           y = VarName,
                           fill= factor(ifelse(Key == PFTicker, "Highlighted", "Normal")))) + 
    geom_bar(stat = "identity") +
    geom_text(aes(label = labelTop, vjust = -0.5)) +
    scale_fill_manual(name = "Key", values = c("red","grey50")) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(titleTop)
}

