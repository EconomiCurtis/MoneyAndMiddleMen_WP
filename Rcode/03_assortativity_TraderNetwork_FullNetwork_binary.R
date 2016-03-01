#' Assortativity plots
#' 
#' 
#'\begin{figure}
#'  \includegraphics[width=1.\linewidth]{figs/assortativity_TraderNetwork_FullNetwork_binary.pdf}
#'  \caption{Weekly assortativity $A(W^b)$ in the unweighted trader network. 
#'  Horizontal dashed lines show benchmarks from \citet{newman2002assortative}, 
#'  and vertical shading indicates promotional events that attract new traders.}
#'\label{fig:simpleAssortativity}
#'
#'% Curtis: see TF2_TraderNetwork.Rmd in "/home/rstudio/rstudio_shared/master/Valve_NetworkStructure/TraderNetwork_Writings/''
#'
#'\end{figure}

source("Rcode/0_setup.R")


CurtisGGplotTheme =     theme_bw() + 
  theme(plot.title = element_text(lineheight=1.1, hjust = 0, family = "serif", size = 21),
        axis.title.x = element_text(hjust = 1, size = 18, family = "serif", color = "grey15"),
        axis.title.y = element_text( size = 18, family = "serif",color = "grey15"),
        axis.text.x =  element_text(size = 12, family = "serif",color = "grey15"),
        axis.text.y =  element_text(size = 15, family = "serif",color = "grey15"),
        legend.text = element_text(size = 15, family = "serif", hjust = 0),
        legend.title = element_text(size = 15, family = "serif", hjust = 0, face="bold"),
        panel.border = element_rect(color = "grey95"),
        axis.ticks = element_line(color = "white"),
        panel.grid.major.x = element_line(colour = 'grey50'),        
        panel.grid.minor.x = element_line(colour = 'grey90'),
        legend.key = element_rect(colour = NA)) 
  
load(file = "data/AssortativityLoger_Good_V3.RData")
AssortativityLoger <- AssortativityLoger[!duplicated(I(paste(AssortativityLoger$Date,AssortativityLoger$AssotativeType,AssortativityLoger$Network))),]

ValveSales = data.frame(
  StartDate = as.Date(c("12/15/2011","6/27/2012","9/6/2012","10/26/2012","12/13/2012","5/15/2013"), "%m/%d/%Y"),
  EndDate   = as.Date(c("1/4/2012","7/11/2012","9/21/2012","11/8/2012","1/3/2013","5/25/2013"), "%m/%d/%Y")
)

AssortativityLoger <- AssortativityLoger %>%
  dplyr::arrange(AssotativeType,Date)
  


CurtisGGplotTheme =     theme_bw() + 
  theme(plot.title = element_text(lineheight=1.1, hjust = 0, family = "serif", size = 21),
        axis.title.x = element_text(hjust = 1, size = 18, family = "serif", color = "grey15"),
        axis.title.y = element_text( size = 18, family = "serif",color = "grey15"),
        axis.text.x =  element_text(size = 15, family = "serif",color = "grey15"),
        axis.text.y =  element_text(size = 15, family = "serif",color = "grey15"),
        legend.text = element_text(size = 15, family = "serif", hjust = 0),
        legend.title = element_text(size = 15, family = "serif", hjust = 0, face="bold"),
        panel.border = element_rect(color = "grey95"),
        axis.ticks = element_line(color = "white"),
        panel.grid.major.x = element_line(colour = 'grey50'),        
        panel.grid.minor.x = element_line(colour = 'grey90'),
        legend.key = element_rect(colour = NA)) 

ValveSales = data.frame(
  StartDate = as.Date(c("12/15/2011","6/27/2012","9/6/2012","10/26/2012","12/13/2012","5/15/2013"), "%m/%d/%Y"),
  EndDate   = as.Date(c("1/4/2012","7/11/2012","9/21/2012","11/8/2012","1/3/2013","5/25/2013"), "%m/%d/%Y")
)

ggplot(
  data = (
    AssortativityLoger %>%
      dplyr::filter(
        AssotativeType %in% c("Assortativity Degree") 
        & Network %in% c("All")
      ) %>%
      dplyr::mutate(
        Date = as.Date(Date)
      )
  )
) + 
  geom_line(aes(x = Date + 7, y = AssortativityEst, color = paste(AssotativeType, Network)), size = 1.5) +
  geom_rect(data = ValveSales, aes(xmin = as.Date(StartDate), xmax = as.Date(EndDate), ymin=-Inf, ymax=+Inf), fill='cadetblue4', alpha=0.4) +
  scale_colour_brewer(palette="Set1") +
  theme_bw() +
  ylim(-.25,.25) +
  CurtisGGplotTheme +
  ggtitle("Trader Network Assortativity") + # (All trading, unweighted binary network)
  xlab("Date") + 
  ylab("Assortativity") + 
  scale_x_date(labels = date_format(" %b  \n%Y "), 
               limits = as.Date(c(as.Date("2011-3-1"),as.Date("2013-07-01"))), 
               breaks = date_breaks("6 month"), 
               minor_breaks = date_breaks("1 month")) +
  CurtisGGplotTheme +
  theme(
    legend.position = "none") +
  geom_hline(yintercept=c(0.12, 0.2, 0, -0.18, -0.065), linetype = "dashed", colour = 'grey10')+
  geom_text(aes(x = as.Date("2011-3-1"), y = -0.18, label = "Internet Wiring", vjust = -0.5, hjust = 0), family = "serif",colour = "grey30", size = 6) + 
  geom_text(aes(x = as.Date("2011-3-1"), y = -0.065, label = "WWW Link", vjust = -0.5, hjust = 0),family = "serif", colour = "grey30", size = 6) + 
  geom_text(aes(x = as.Date("2011-3-1"), y = 0, label = "Random Graph", vjust = -0.5, hjust = 0),family = "serif",  colour = "grey30", size = 6) + 
  geom_text(aes(x = as.Date("2011-3-1"), y = 0.2, label = "Movie Actors", vjust = -0.5, hjust = 0), family = "serif", colour = "grey30", size = 6) +
  geom_text(aes(x = as.Date("2011-3-1"), y = .12, label = "Maths Coauthorship", vjust = -.5, hjust = 0), family = "serif", colour = "grey30", size = 5.5) 

#pdf 10w x 5.2h
# add to caption:  (All trading, unweighted binary network)

ggsave(
  file = "figs/assortativity_TraderNetwork_FullNetwork_binary.pdf",
  
  width = 10,
  height = 5.2,
  units = "in"
)

rm(ValveSales, 
   AssortativityLoger)