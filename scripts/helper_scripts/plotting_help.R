
#plotting helper functions


plot_percent <- function(df, var, removeNAs = T, coordflippin = F) {
  if (removeNAs == F){
    yo <- df %>% group_by_(as.name(var)) %>% 
      summarize(tally = n()) #getting totals
    yo <- mutate(yo, percent = round(tally/sum(tally), digits=2))}  
  else if (removeNAs == T){
    yo <- df %>% group_by_(as.name(var)) %>% 
      summarize(tally = n()) #getting totals
    yo <- na.omit(yo) #removing NAs
    yo <- mutate(yo, percent = round(tally/sum(tally), digits=2))}  
  
  if (coordflippin == F){
    ggplot(data = yo, aes_string(x=var, y = "tally")) + 
      geom_bar(stat="identity", colour = "black", fill = "#F0E442") + ylab("count")+
      geom_text(stat="identity", aes_string(label="percent", vjust=-.1))}
  else if (coordflippin == T){
    ggplot(data = yo, aes_string(x=var, y = "tally")) +
      geom_bar(stat="identity", colour = "black", fill = "#F0E442") + ylab("count") +
      coord_flip() +
      geom_text(stat="identity", aes_string(label="percent", hjust = -.1))}
}


plot_percent2 <- function(df, var1, var2, removeNAs = F, 
                          coordflippin = F, percent = F) {
  if (removeNAs == T){
    yo <- df %>% group_by_(as.name(var1), as.name(var2)) %>% 
      summarize(tally = n()) #getting totals
    yo <- mutate(yo, percent = round(tally/sum(tally), digits=2))}  
  else if (removeNAs == F){
    yo <- df %>% group_by_(as.name(var1), as.name(var2)) %>% 
      summarize(tally = n()) #getting totals
    yo <- na.omit(yo) #removing NAs
    yo <- mutate(yo, percent = round(tally/sum(tally), digits=2))}  
  
  if (coordflippin == F & percent == F){
    ggplot(data = yo, aes_string(x=var1, y = "tally", fill = var2)) + 
      geom_bar(stat="identity", position=position_dodge()) + ylab("count")}
  
  else if (coordflippin == T & percent == F){
    ggplot(data = yo, aes_string(x=var1, y = "tally", fill = var2)) + 
      geom_bar(stat="identity", position=position_dodge()) + ylab("count")+ coord_flip()}
  
  else if (coordflippin == F & percent == T){
    ggplot(data = yo, aes_string(x=var1, y = "tally", fill = var2)) + 
      geom_bar(stat="identity", position=position_dodge()) + ylab("count") + 
      geom_text(stat="identity", aes_string(label="percent", vjust=-.25))}
  
  else if (coordflippin == T & percent == T){
    ggplot(data = yo, aes_string(x=var1, y = "tally", fill = var2)) + 
      geom_bar(stat="identity", position=position_dodge()) + ylab("count") + coord_flip() + 
      geom_text(stat="identity", aes_string(label="percent", vjust = -.25))}
}


plot_hist <- function(df, var, mean = T) {
  if (mean == T){
    numero <- df %>% #getting mean
      summarize_(promedio = mean(get(var, 
                                     envir=as.environment(df)), na.rm = T))
    numero <- as.character(numero[[1]])
    ggplot(df, aes_string(x=var)) + 
      geom_histogram(colour="black", fill = "#F0E442", binwidth = 1) +
      geom_vline(aes_string(xintercept=numero),   
                 color="black", linetype="dashed", size=.5)}
  else if (mean == F){
    ggplot(df, aes_string(x=var)) + 
      geom_histogram(colour="black", fill = "#F0E442", binwidth = 1)}
}
