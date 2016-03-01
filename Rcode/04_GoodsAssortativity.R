#'  GoodsAssortativity
#'  Weekly assortativity $A(Z)$ in goods networks.
#'  Connected orange dots and green triangles indicate respectively the 
#'  unweighted (binary) full network and reduced (single money node) network 
#'  values, while dashed blue squares and dashed purple indicate the 
#'  corresponding weighted network values.
#' 
#' \begin{figure}
#' \includegraphics[width=1.\linewidth]{figs/GoodsAssortativity.pdf}
#'   \caption{Weekly assortativity $A(Z)$ in goods networks.
#'    Connected orange dots and green triangles indicate respectively the unweighted (binary) full network and reduced (single money node) network values, while dashed blue squares and dashed purple
#'    indicate the corresponding weighted network values. 
#' }
#' \label{fig:assort}
#' \end{figure}

source("Rcode/0_setup.R")


#load GoodsNetwork.networkLevelStats
load(file = "data/GoodsNetwork.networkLevelStats_V5_.RData")
load(file = "data/GoodsNetwork.networkLevelStats.MoneyNode_V5_.RData")


library(ggplot2)
library(scales)
library(dplyr)
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


GoodsNetwork.networkLevelStats.plot <- 
  GoodsNetwork.networkLevelStats.MoneyOneNode %>% 
  dplyr::filter(
    variable %in% c("Assortativity (Mod LC2005 Excess Weights)","Assortativity (Excess Weights)")
  ) %>%
  dplyr::mutate(
    variable = as.character(variable),
    variable = ifelse((variable == "Assortativity (Excess Weights)"),
                      yes = "Assortativity (Excess Weights) Single Money Node     ",
                      no = variable),
    variable = ifelse((variable == "Assortativity (Mod LC2005 Excess Weights)"),
                      yes = "Assortativity (Mod LC2005 Excess Weights) Single Money Node        ",
                      no = variable)
  )

GoodsNetwork.networkLevelStats.plot <- rbind(
  GoodsNetwork.networkLevelStats.plot,
  (GoodsNetwork.networkLevelStats %>% 
     dplyr::filter(
       variable %in% c("Assortativity (Mod LC2005 Excess Weights)","Assortativity (Excess Weights)")
     )
  )
) %>%
  dplyr::mutate(
    variable = ifelse(
      (variable == "Assortativity (Mod LC2005 Excess Weights)"),
      yes = "Assortativity (Mod LC 2007 Excess Weights)",
      no = variable
    ),
    variable = ifelse(
      (variable == "Assortativity (Mod LC2005 Excess Weights) Single Money Node"),
      yes = "Assortativity (Mod LC 2007 Excess Weights) Single Money Node",
      no = variable
    )
  )



ValveSales = data.frame(
  StartDate = as.Date(c("12/15/2011","6/27/2012","9/6/2012","10/26/2012","12/13/2012","5/14/2013"), "%m/%d/%Y"),
  EndDate   = as.Date(c("1/4/2012","7/11/2012","9/21/2012","11/8/2012","1/3/2013","5/28/2013"), "%m/%d/%Y")
)



ggplot2::ggplot(
  GoodsNetwork.networkLevelStats.plot
  ) + 
  geom_line(aes(x = as.Date(Date), y = value, color = variable, linetype = variable, shape = variable),
            size = 1.25) +
  geom_point(
    data = (
      GoodsNetwork.networkLevelStats.plot %>%
        dplyr::filter(
          Date %in% as.character(as.Date(seq(0,93, by = 4)*7, origin = "2011-08-15"))
        )
    ),
    aes(x = as.Date(Date), y = value, color = variable, linetype = variable, shape = variable),
    size = 4) + 
  
  theme_bw() + 
  CurtisGGplotTheme + 
  geom_rect(data = ValveSales, 
            aes(xmin = as.Date(StartDate), xmax = as.Date(EndDate), 
                ymin=-Inf, ymax=+Inf), fill='cadetblue4', alpha=0.4) +
  scale_x_date(labels = date_format(" %b  \n%Y "), 
               limits = as.Date(c(as.Date("2011-1-01"),as.Date("2013-07-01"))), 
               breaks = seq(as.Date("2011/7/1"), by = "6 month", length.out = 5), 
               minor_breaks = date_breaks("1 month")) +
  theme(legend.position="top",
        legend.key = element_rect(colour = NA),
        legend.text = element_text(size = 11, family = "serif", hjust = 0)) +
  theme(legend.key.width = unit(1.75, "cm")) + 
  theme(legend.title=element_blank()) +
  xlab("") + 
  ylab("Assortativity") +
  guides(col = guide_legend(nrow = 2)) +
  ylim(-0.75,.05)+
  #ggtitle("Assortativity") +
  geom_hline(yintercept=c(0.12, 0.2, 0, -0.18, -0.065), linetype = "dashed", colour = 'grey10')+
  geom_text(aes(x = as.Date("2011-2-1"), y = -0.18, label = "Internet Wiring", vjust = -0.2, hjust = 0), family = "serif",colour = "grey30") + 
  geom_text(aes(x = as.Date("2011-2-1"), y = -0.065, label = "WWW Link", vjust = -0.2, hjust = 0),family = "serif", colour = "grey30") + 
  geom_text(aes(x = as.Date("2011-2-1"), y = 0, label = "Random Graph", vjust = -0.2, hjust = 0),family = "serif",  colour = "grey30") + 
  # geom_text(aes(x = as.Date("2011-2-1"), y = 0.2, label = "Movie Actors", vjust = -0.2, hjust = 0), family = "serif", colour = "grey30") +
  # geom_text(aes(x = as.Date("2011-2-1"), y = .12, label = "Maths Coauthorship", vjust = -0.2, hjust = 0), family = "serif", colour = "grey30") +
  guides(color = F, points = F, linetype = F, shape = F) #remove legend, rely on caption

ggsave(
  file = "figs/GoodsAssortativity.pdf",
  width = 9,
  height = 5,
  units = "in"
  
)

