#' assortativity_hilo_weighted.pdf
#' 
#' Weekly assortativity $A(W^s)$ for weighted trader networks built from high value 
#'   ($s=$ hi, solid red line) and from low value ($s=$ lo, dashed blue line) transactions.
#'
#'\begin{figure}
#'    \includegraphics[width=1.\linewidth]{figs/assortativity_hilo_weighted.pdf}
#'    \caption{Weekly assortativity $A(W^s)$ for weighted trader networks built 
#'    from high value ($s=$ hi, solid red line) and from low value ($s=$ lo, 
#'    dashed blue line) transactions.}
#'  \label{fig:weightedAssortativity}
#'% Curtis: see TF2_TraderNetwork.Rmd in "/home/rstudio/rstudio_shared/master/Valve_NetworkStructure/TraderNetwork_Writings/''
#'\end{figure}

source("Rcode/0_setup.R")

load(file = "data/AssortativityLoger_Good_V3.RData")
AssortativityLoger <- AssortativityLoger[!duplicated(I(paste(AssortativityLoger$Date,AssortativityLoger$AssotativeType,AssortativityLoger$Network))),]


AssortativityLoger <- AssortativityLoger %>%
  dplyr::arrange(AssotativeType,Date)

ValveSales = data.frame(
  StartDate = as.Date(c("12/15/2011","6/27/2012","9/6/2012","10/26/2012","12/13/2012","5/15/2013"), "%m/%d/%Y"),
  EndDate   = as.Date(c("1/4/2012","7/11/2012","9/21/2012","11/8/2012","1/3/2013","5/25/2013"), "%m/%d/%Y")
)

ggplot(
  data = (
    AssortativityLoger %>%
      dplyr::filter(
        AssotativeType %in% c("Assortativity (Mod LC2005 Excess Weights)") 
        & !(Network %in% c("All"))
      ) %>%
      dplyr::mutate(
        Date = as.Date(Date)
      ) %>%
      dplyr::mutate(
        AssotativeType = ifelse(
          (AssotativeType == "Assortativity (Mod LC2005 Excess Weights)"), 
          yes = "Trading,  Assortativity (Excess Weights, Modified Leung and Chau, 2005)",
          no = AssotativeType
        )
      )
  )
) + 
  geom_line(aes(x = Date + 7, y = AssortativityEst, color = paste(Network, AssotativeType), linetype = paste(Network, AssotativeType)), size = 1.5) +
  geom_rect(data = ValveSales, 
            aes(xmin = as.Date(StartDate), xmax = as.Date(EndDate), ymin=-Inf, ymax=+Inf), fill='cadetblue4', alpha=0.4) +
  scale_colour_brewer(palette="Set1") +
  theme_bw() +
  CurtisGGplotTheme +
  #ggtitle("Trader Network Weighted Assortativity")  + # (low vs. high value trading, weighted network)
  xlab("") + 
  ylab("Assortativity") + 
  scale_x_date(labels = date_format(" %b  \n%Y "), 
               limits = as.Date(c(as.Date("2011-8-08"),as.Date("2013-07-01"))), 
               breaks = seq(as.Date("2012/1/1"), by = "6 month", length.out = 4), 
               minor_breaks = date_breaks("1 month")) +
  CurtisGGplotTheme +
  theme(legend.key.width = unit(2.1, "cm")) + 
  theme(
    legend.position = "top") +
  guides(col = guide_legend(ncol = 1)) +
  theme(legend.title=element_blank()) +
  geom_hline(yintercept=c(0.12, 0.2, 0, -0.18, -0.065), linetype = "dashed", colour = 'grey10')+
  geom_text(aes(x = as.Date("2011-7-1"), y = -0.18, label = " ", vjust = -0.5, hjust = 0), family = "serif",colour = "grey30") + 
  geom_text(aes(x = as.Date("2011-7-1"), y = -0.065, label = " ", vjust = -0.5, hjust = 0),family = "serif", colour = "grey30") + 
  geom_text(aes(x = as.Date("2011-7-1"), y = 0, label = " ", vjust = -0.5, hjust = 0),family = "serif",  colour = "grey30") + 
  geom_text(aes(x = as.Date("2011-7-1"), y = 0.2, label = " ", vjust = -0.5, hjust = 0), family = "serif", colour = "grey30") +
  geom_text(aes(x = as.Date("2011-7-1"), y = .12, label = " ", vjust = -.5, hjust = 0), family = "serif", colour = "grey30") 

# 10w x 6h for slides
