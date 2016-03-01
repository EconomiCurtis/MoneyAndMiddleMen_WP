#'\caption{Weekly transaction counts and unique trader account over the sample 
#'         period. Vertical shaded bars indicate promotional events}
#'         
#'          \begin{figure}
#'          \includegraphics[width=1.\linewidth]{figs/Results_tradecount.pdf}
#'          \centering
#'          \vspace*{-10mm}
#'          \caption{Weekly transaction counts and unique trader account over the sample period. Vertical shaded bars indicate promotional events}
#'          \label{fig:tradecount}
#'          % Curtis: see results_1stplots.R in "/home/rstudio/rstudio_shared/master/Valve_NetworkStructure/GoodsNetwork_Writings/''
#'          \end{figure}

source("Rcode/0_setup.R")


# workign with TF2MarketSum_v1 ValveSales
# load TF2MarketSum_v1 -------
load(file = "data/TF2MarketSum_v1.RData")

ValveSales = data.frame(
  StartDate = as.Date(c("12/15/2011","6/27/2012","9/6/2012","10/26/2012","12/13/2012","5/15/2013"), "%m/%d/%Y"),
  EndDate   = as.Date(c("1/4/2012","7/11/2012","9/21/2012","11/8/2012","1/3/2013","5/25/2013"), "%m/%d/%Y")
)

CurtisGGplotTheme =     theme_bw() + 
  theme(plot.title = element_text(lineheight=1.1, hjust = 0, family = "serif", size = 16),
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



p <- ggplot(
  rbind(
    (
      dplyr::select(
        TF2MarketSum, 
        Date, value = TradeCount
      ) %>%
        dplyr::mutate(
          var = "Edges (Trade Count)            "
        )
    ),
    (
      dplyr::select(
        TF2MarketSum, 
        Date, value = TraderCount
      ) %>%
        dplyr::mutate(
          var = "Nodes (Unique Trader Count)"
        )
    )
  )
) +
  geom_rect(data = ValveSales, aes(xmin = as.Date(StartDate), xmax = as.Date(EndDate), ymin=-Inf, ymax=+Inf), fill='cadetblue4', alpha=0.4) +
  geom_line(
    aes(
      x = as.Date(Date),
      y = value,
      colour = var,
      linetype = var
    ),
    size = 1.25
  ) +
  theme_bw() +
  CurtisGGplotTheme +
  scale_y_continuous(labels = comma) +
  # ggtitle("") + #Trades (weekly)") + # (All trading, unweighted binary network)
  xlab("") + 
  ylab("") + 
  scale_x_date(labels = date_format(" %b  \n%Y "), 
               limits = as.Date(c(as.Date("2011-9-1"),as.Date("2013-06-15"))), 
               breaks = date_breaks("6 month"), 
               minor_breaks = date_breaks("1 month")) +
  guides(col = guide_legend(ncol = 2))  +
  theme(legend.position="top",
        legend.key = element_rect(colour = NA)) +
  theme(legend.title=element_blank()) +
  theme(legend.key.width = unit(1.75, "cm"))

ggsave(
  "figs/Results_tradecount.pdf",
  plot = p, 
  width = 9,
  height = 4,
  units = "in"
)



# slides: pdf 9 by 4
# paper: x

remove(TF2MarketSum_v1,ValveSales, p)
