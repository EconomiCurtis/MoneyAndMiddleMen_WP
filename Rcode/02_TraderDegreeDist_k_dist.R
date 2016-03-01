#' Figure of trader-degree (k) distributions
#' 
#' 
#' \begin{figure}
#' \centering
#' 
#' \subfloat[Trader degree distribution for the week starting 2011-08-09][ ]{
#'    \includegraphics[width=0.45\textwidth]{figs/TraderDegreeDist/k_dist_20110809.pdf}
#'    \label{fig:subfig1}}
#'    
#'\subfloat[Trader degree distribution for the week starting 2011-09-20][ ]{
#'    \includegraphics[width=0.45\textwidth]{figs/TraderDegreeDist/k_dist_20110920.pdf}
#'    \label{fig:subfig2}}
#'    
#'    
#'\qquad
#'\subfloat[Trader degree distribution for the week starting 2011-11-25][ ]{
#'    \includegraphics[width=0.45\textwidth]{figs/TraderDegreeDist/k_dist_20111025.pdf}
#'    \label{fig:subfig3}}
#'
#'\subfloat[Trader degree distribution for the week starting 2012-07-31][ ]{
#'    \includegraphics[width=0.45\textwidth]{figs/TraderDegreeDist/k_dist_20120731.pdf}
#'    \label{fig:subfig4}}
#'    
#'\qquad
#'
#'\subfloat[Trader degree distribution for the week starting 2012-11-20][ ]{
#'    \includegraphics[width=0.45\textwidth]{figs/TraderDegreeDist/k_dist_20121120.pdf}
#'    \label{fig:subfig5}}
#'    
#'\subfloat[Trader degree distribution for the week starting 2013-05-21][ ]{
#'    \includegraphics[width=0.45\textwidth]{figs/TraderDegreeDist/k_dist_20130521.pdf}
#'    \label{fig:subfig6}}
#'    
#'\caption{Degree distribution in trader network (log scales).}
#'\label{fig:TN:DegreeDist}
#'% Curtis: see TF2_TraderNetwork.Rmd in "/home/rstudio/rstudio_shared/master/Valve_NetworkStructure/TraderNetwork_Writings/''
#'\end{figure}
#'

source("Rcode/0_setup.R")


CurtisGGplotTheme =     theme_bw() + 
  theme(plot.title = element_text(lineheight=1.1, hjust = 0, family = "serif", size = 16),
        axis.title.x = element_text(hjust = 1, size = 15, family = "serif", color = "grey15"),
        axis.title.y = element_text( size = 15, family = "serif",color = "grey15"),
        axis.text.x =  element_text(size = 15, family = "serif",color = "grey15"),
        axis.text.y =  element_text(size = 15, family = "serif",color = "grey15"),
        legend.text = element_text(size = 15, family = "serif", hjust = 0),
        legend.title = element_text(size = 15, family = "serif", hjust = 0, face="bold"),
        panel.border = element_rect(color = "grey99"),
        axis.ticks = element_line(color = "white"),
        panel.grid.major.x = element_line(colour = 'grey80'),        
        panel.grid.minor.x = element_line(colour = 'grey90'),
        panel.grid.major.y = element_line(colour = 'grey80'),        
        panel.grid.minor.y = element_line(colour = 'grey95'),
        
        legend.key = element_rect(colour = NA)) 



for (FILE in list.files("data/trader_dist/")){
  load(paste("data/trader_dist/",FILE, sep = ""))
  
  graphDat <- eval(
    parse(text = gsub(".RData", "", FILE))
    )
  
  df = data.frame(
    xStart = 0,
    xStop  = 10,
    yMIN   = 0,
    yTop   = nrow((graphDat %>% dplyr::filter(k < 10)))
  )
  GrowBy = 1.5
  SEQ = c(10, 10^(seq(1.4,4,0.2)))
  
  for (j in 2:length(SEQ)){
    
    df <- rbind(
      df, 
      c(SEQ[j-1], 
        SEQ[j],
        0,
        nrow((graphDat %>% dplyr::filter(k >= SEQ[j-1] & k < SEQ[j])))
      )
    )
    
    
  }
  
  DATE <- gsub(".RData|graphDat_", "", FILE)
    
  
  p <- ggplot(df, aes(xmin = xStart, xmax = xStop, ymin = yMIN, ymax =yTop)) +
    geom_rect() +
    scale_x_log10(labels = comma, limits = c(NA, 6200)) + 
    scale_y_log10(labels = comma, limits = c(NA, 1e7)) + #you may have to tweak this a lot!
    CurtisGGplotTheme + 
    ggtitle(
      paste("Week Starting ", DATE ,sep = "") # removed "Node Degree Frequency \n"
    ) + 
    xlab("Trader Node Degree") + ylab("Frequency") 
  # 4in by 2.5in
  
  ggsave(
    filename = paste("figs/trade_d_dist_", DATE,".PDF", sep = ""),
    p, 
    width = 4, 
    height = 2.5, 
    units = "in"
    
  )
  
  
}







  
  
rm(list = gsub(".RData","",list.files("data/trader_dist/")))
rm(graphDat)
  
  



