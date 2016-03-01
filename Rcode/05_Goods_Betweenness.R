#' Goods Betweeness 
#' many betweenness plots
#' all trading, 
#' low value, medium and high value trading 
#' 
#' 
#' 


# betweenness_TopMoneyItems_allItems.pdf ------


CurtisGGplotTheme =     theme_bw() + 
  theme(plot.title = element_text(lineheight=1.1, hjust = 0, family = "serif", size = 21),
        axis.title.x = element_text(hjust = 1, size = 13, family = "serif", color = "grey15"),
        axis.title.y = element_text( size = 13, family = "serif",color = "grey15"),
        axis.text.x =  element_text(size = 12, family = "serif",color = "grey15"),
        axis.text.y =  element_text(size = 12, family = "serif",color = "grey15"),
        legend.text = element_text(size = 12, family = "serif", hjust = 0),
        legend.title = element_text(size = 12, family = "serif", hjust = 0, face="bold"),
        panel.border = element_rect(color = "grey95"),
        axis.ticks = element_line(color = "white"),
        panel.grid.major.x = element_line(colour = 'grey50'),        
        panel.grid.minor.x = element_line(colour = 'grey90'),
        legend.key = element_rect(colour = NA)) 


load(file = "data/GoodsNetwork.nodeLevelStats.MoneyNode_V5_.RData")

GoodsNetwork.nodeLevelStats.MoneyOneNode <- GoodsNetwork.nodeLevelStats.MoneyOneNode %>%
  dplyr::group_by(Date) %>%
  dplyr::mutate(
    s_norm = s / sum(s),
    k_norm = k / max(k),
    C_Ds_alpha0.5_norm = C_Ds_alpha0.5 / max(C_Ds_alpha0.5)
  )

GoodsNetwork.nodeLevelStats.plot <- dplyr::filter(
  GoodsNetwork.nodeLevelStats.MoneyOneNode, 
  i == "Money"
)




load(file ="data/GoodsNetwork.nodeLevelStats_V5_.RData")

GoodsNetwork.nodeLevelStats <- GoodsNetwork.nodeLevelStats %>%
  dplyr::group_by(Date) %>%
  dplyr::mutate(
    s_norm = s / sum(s),
    k_norm = k / max(k),
    C_Ds_alpha0.5_norm = C_Ds_alpha0.5 / max(C_Ds_alpha0.5)
  )

GoodsNetwork.nodeLevelStats.plot <- rbind(
  GoodsNetwork.nodeLevelStats.plot, 
  GoodsNetwork.nodeLevelStats) %>% dplyr::ungroup()





MoneyDefIDs = read.table(text ="
                         i 'Item'
                         Money  'Money'
                         5021_440_6  'Key'
                         5002_440_6  'Refined Metal'
                         5001_440_6  'Reclaimed Metal'
                         5000_440_6  'Scrap Metal'
                         126_440_6  'Bills Hat'
                         143_440_6  'Earbuds'
                         5068_440_6_40  'Salvaged Mann Co. Supply Crate Series 40          ' 
                         5022_440_6  'Mann Co. Supply Crate'
                         5072_440_6  'All Other Items'
                         ", header = T)

# {             5072_440_6  'Naughty Winter Key 2011'
#               5070_440_6  'Naughty Winter Crate 2011'
#               5073_440_6  'Nice Winter Key 2011'
#               5071_440_6  'Nice Winter Crate 2011'
#               5079_440_6  'Scorched Key'
#               5078_440_6  'Scorched Crate'
#               5081_440_6  'Fall Key 2012'
#               5080_440_6  'Fall Crate 2012'
#               5628_440_6  'Eerie Key'
#               5627_440_6  'Eerie Crate'
#               5631_440_6  'Naughty Winter Key 2012'
#               5629_440_6  'Naughty Winter Crate 2012'
#               5632_440_6  'Nice Winter Key 2012'
#               5630_440_6  'Nice Winter Crate 2012'
#               5636_440_6  'Robo Key'
#               5635_440_6  'Robo Crate'}

MoneyDefIDs$Item <- factor(MoneyDefIDs$Item, levels = MoneyDefIDs$Item)

GoodsNetwork.nodeLevelStats.plot <- dplyr::left_join(
  GoodsNetwork.nodeLevelStats.plot, MoneyDefIDs, 
  by = "i"
)

colourCount = length(unique(MoneyDefIDs$i))
library("RColorBrewer")
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

ValveSales = data.frame(
  StartDate = as.Date(c("12/15/2011","6/27/2012","9/6/2012","10/26/2012","12/13/2012","5/14/2013"), "%m/%d/%Y"),
  EndDate   = as.Date(c("1/4/2012","7/11/2012","9/21/2012","11/8/2012","1/3/2013","5/28/2013"), "%m/%d/%Y")
)


betweenness_TopMoneyItems_allItems <- ggplot2::ggplot(
  GoodsNetwork.nodeLevelStats.plot %>%
    dplyr::filter(i %in% MoneyDefIDs$i)
) + 
  geom_rect(data = ValveSales, 
            aes(xmin = as.Date(StartDate), xmax = as.Date(EndDate), 
                ymin=-Inf, ymax=+Inf), fill='cadetblue4', alpha=0.25) +
  geom_line(
    data = (
      GoodsNetwork.nodeLevelStats.plot %>%
        dplyr::filter(
          (is.na(Item))
        )
    ),
    aes(
      x = as.Date(Date),
      y = betweenness_norm, 
      group = i
    ), 
    size = 1, colour = "grey") +
  geom_line(
    data = (
      GoodsNetwork.nodeLevelStats.plot %>%
        dplyr::filter(
          Date %in% as.character(as.Date(seq(0,93, by = 4)*7, origin = "2011-08-15"))
          & Item %in% MoneyDefIDs$Item[1:10]
        )
    ),
    aes(
      x = as.Date(Date), 
      y = betweenness_norm, 
      color = Item, 
      shape = Item,
      linetype = Item),
    size = 1.5) +
  geom_point(
    data = (
      GoodsNetwork.nodeLevelStats.plot %>%
        dplyr::filter(
          Date %in% as.character(as.Date(seq(0,93, by = 4)*7, origin = "2011-08-15"))
          & Item %in% MoneyDefIDs$Item[1:10]
        )
    ),
    aes(
      x = as.Date(Date), 
      y = betweenness_norm, 
      colour = Item,
      shape = Item),
    size = 4) +
  scale_colour_manual(values = getPalette(colourCount),
                      guide = guide_legend(nrow = 15))+
  theme_bw() + 
  CurtisGGplotTheme +
  scale_x_date(labels = date_format(" %b  \n%Y "), 
               limits = as.Date(c(as.Date("2011-8-01"),as.Date("2013-07-01"))), 
               breaks = seq(as.Date("2012/1/1"), by = "6 month", length.out = 5), 
               minor_breaks = date_breaks("1 month")) +
  xlab("Date") + 
  ylab("Betweenness (Normalized)") +
  #ggtitle("Item Betweeness, All Items") +
  theme(
    legend.position = "top") +
  guides(col = guide_legend(nrow = 4)) +
  theme(legend.title=element_blank()) +
  theme(
    legend.key = element_rect(colour = NA)
  ) + 
  theme(legend.key.width = unit(2.1, "cm")) +
  scale_shape_manual(values = c(16,17, 15,0,7, 1,18, 5,8,32)) +
  scale_linetype_manual(values = c(1, 2,3,4,5,6,2,3,4,1))

ggsave(
  file = "figs/betweenness_TopMoneyItems_allItems.pdf",
  plot = betweenness_TopMoneyItems_allItems,
  width = 11,
  height = 5,
  units = "in"
  
)



# Betweenness of all items, weekly. Betweenness is normalized by the maximum betweenness possible. 
# 
# "Money" is betweenness when combining the six money goods - keys (5021) metals (5000, 5001, 5002), bill's hat (126) and earbuds (143) - into a single node. 
# 
# 5002 is refined metal betweenness, it is the highest good-level betweeness at the end of the sample, just above keys.
# 
# All other items are relatively high betweenness items. These are mostly event keys. You can see the spikes down in keys around 2012 holidays and the end of the sample correspond to spikes in event key betweenness. 
# 
# All other items are excluded. 





load("data/GoodsNetwork.nodeLevelStats_HiLoPriceBreakdown_V1_.RData")

CurtisGGplotTheme =     theme_bw() + 
  theme(plot.title = element_text(lineheight=1.1, hjust = 0, family = "serif", size = 21),
        axis.title.x = element_text(hjust = 1, size = 14, family = "serif", color = "grey15"),
        axis.title.y = element_text( size = 15, family = "serif",color = "grey15"),
        axis.text.x =  element_text(size = 15, family = "serif",color = "grey15"),
        axis.text.y =  element_text(size = 12, family = "serif",color = "grey15"),
        legend.text = element_text(size = 14, family = "serif", hjust = 0),
        legend.title = element_text(size = 12, family = "serif", hjust = 0, face="bold"),
        panel.border = element_rect(color = "grey95"),
        axis.ticks = element_line(color = "white"),
        panel.grid.major.x = element_line(colour = 'grey50'),        
        panel.grid.minor.x = element_line(colour = 'grey90'),
        legend.key = element_rect(colour = NA)) 

MoneyDefIDs = read.table(text ="
                          i 'Item'
                          Money  'Money'
                          5021_440_6  'Key'
                          5002_440_6  'Refined Metal'
                          5001_440_6  'Reclaimed Metal'
                          5000_440_6  'Scrap Metal'
                          126_440_6  'Bills Hat'
                          143_440_6  'Earbuds'
                          5068_440_6_40  'Salvaged Mann Co. Supply Crate Series 40        ' 
                          5022_440_6  'Mann Co. Supply Crate'
                          5072_440_6  'All Other Items'
                            ", header = T)

#                          5070_440_6  'Naughty Winter Crate 2011'
#                          5073_440_6  'Nice Winter Key 2011'
#                          5071_440_6  'Nice Winter Crate 2011'
#                          5079_440_6  'Scorched Key'
#                          5078_440_6  'Scorched Crate'
#                          5081_440_6  'Fall Key 2012'
#                          5080_440_6  'Fall Crate 2012'
#                          5628_440_6  'Eerie Key'
#                          5627_440_6  'Eerie Crate'
#                          5631_440_6  'Naughty Winter Key 2012'
#                          5629_440_6  'Naughty Winter Crate 2012   '
#                          5632_440_6  'Nice Winter Key 2012'
#                          5630_440_6  'Nice Winter Crate 2012'
#                          5636_440_6  'Robo Key'
#                          5635_440_6  'Robo Crate'

MoneyDefIDs$Item <- factor(MoneyDefIDs$Item, levels = MoneyDefIDs$Item)

GoodsNetwork.nodeLevelStats.plot <- dplyr::left_join(
  GoodsNetwork.nodeLevelStats, MoneyDefIDs, 
  by = "i"
)

colourCount = length(unique(MoneyDefIDs$i))
library("RColorBrewer")
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

#unique(GoodsNetwork.nodeLevelStats$Price)
# [1] "Low, <0.95"              "high, > 7"               "mid, Price < 7 & > 0.95"

library(scales)

betweenness_TopMoneyItems_MedVa <- ggplot2::ggplot(
  GoodsNetwork.nodeLevelStats.plot %>%
    dplyr::filter(i %in% MoneyDefIDs$i &
                    Price %in% c("mid, Price < 5 & > 0.95"))
) + 
  geom_line(
    data = (GoodsNetwork.nodeLevelStats.plot %>%
              dplyr::filter(
                Price %in% c("mid, Price < 5 & > 0.95")
                & is.na(Item)
              )), 
    aes(x = as.Date(Date),
        y = betweenness_norm,
        group = i),
    size = 1, 
    colour = "grey"
  ) +
  geom_line(
    aes(
      x = as.Date(Date), 
      y = betweenness_norm, 
      color = Item, 
      shape = Item,
      linetype = Item), 
    size = 1.5) +
  geom_point(
    data = (
      GoodsNetwork.nodeLevelStats.plot %>%
        dplyr::filter(
          Date %in% as.character(as.Date(seq(0,93, by = 4)*7, origin = "2011-08-15"))
          & Price %in% c("mid, Price < 5 & > 0.95")
          & Item %in% MoneyDefIDs$Item[1:9]
        )
    ),
    aes(
      x = as.Date(Date), 
      y = betweenness_norm, 
      colour = Item,
      shape = Item),
    size = 4) +
  theme_bw() + 
  scale_x_date(labels = date_format(" %b  \n%Y "), 
               limits = as.Date(c(as.Date("2011-8-01"),as.Date("2013-07-01"))), 
               breaks = seq(as.Date("2012/1/1"), by = "6 month", length.out = 5), 
               minor_breaks = date_breaks("1 month")) +
  xlab("Date") + 
  ylab("Betweenness (Normalized)") +
  #ggtitle("Item Betweeness, medium value items") +
  CurtisGGplotTheme + 
  theme(
    legend.position = "top") +
  guides(col = guide_legend(ncol = 4)) +
  theme(legend.title=element_blank(),
        legend.key = element_rect(colour = NA)
  ) + 
  theme(legend.key.width = unit(2.1, "cm"))+
  scale_colour_manual(values = getPalette(colourCount)[2:11]) +
  scale_shape_manual(values = c(17, 15,0,7, 1,18, 5,8,32,32)) +
  scale_linetype_manual(values = c(2,3,4,5,6,2,3,4,1))

ggsave(
  file = "figs/betweenness_TopMoneyItems_MedVa.PDF",
  plot = betweenness_TopMoneyItems_MedVa,
  width = 10,
  height = 4.5,
  units = "in"
)

#pdf 10w x 4.5h for paper, not too important of a plot
#pdf 12w x 6h

```


```{r, echo=F}

load(paste(
"/home/rstudio/rstudio_shared/master/Valve_NetworkStructure/GoodsNetwork.nodeLevelStats_HiLoPriceBreakdown_V1_",
".RData",
sep=""))

MoneyDefIDs = read.table(text ="
i 'Item'
Money  'Money'
5021_440_6  'Key'
5002_440_6  'Refined Metal'
5001_440_6  'Reclaimed Metal'
5000_440_6  'Scrap Metal'
126_440_6  'Bills Hat'
143_440_6  'Earbuds'
5068_440_6_40  'Salvaged Mann Co. Supply Crate Series 40            ' 
5022_440_6  'Mann Co. Supply Crate'
5072_440_6  'All Other Items'
", header = T)
# 666_440_6     'B.O.M.C.'
#                          5001_440_6  'Reclaimed Metal'
#                          5000_440_6  'Scrap Metal'
#                          378_440_6   'Team Captain Hat'
#                          725_440_6   'Tour of Duty Ticket'

MoneyDefIDs$Item <- factor(MoneyDefIDs$Item, levels = MoneyDefIDs$Item)

GoodsNetwork.nodeLevelStats.plot <- dplyr::left_join(
GoodsNetwork.nodeLevelStats, MoneyDefIDs, 
by = "i"
)



colourCount = length(unique(MoneyDefIDs$i))
library("RColorBrewer")
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

#unique(GoodsNetwork.nodeLevelStats$Price)
# [1] "Low, <0.95"              "high, > 7"               "mid, Price < 7 & > 0.95"

library(scales)

ggplot2::ggplot(
GoodsNetwork.nodeLevelStats.plot %>%
dplyr::filter(i %in% MoneyDefIDs$i &
Price %in% c("high, > 5"))
) + 
geom_line(
data = (GoodsNetwork.nodeLevelStats.plot %>%
dplyr::filter(
Price %in% c("high, > 5")
& is.na(Item)
)), 
aes(x = as.Date(Date),
y = betweenness_norm,
group = i),
size = 1, 
colour = "grey"
) +
geom_line(
data = (
GoodsNetwork.nodeLevelStats.plot %>%
dplyr::filter(
Price %in% c("high, > 5")
& Item %in% MoneyDefIDs$Item[1:10]
)
),
aes(
x = as.Date(Date), 
y = betweenness_norm, 
color = Item, 
shape = Item,
linetype = Item), 
size = 1) +
geom_point(
data = (
GoodsNetwork.nodeLevelStats.plot %>%
dplyr::filter(
Date %in% as.character(as.Date(seq(0,93, by = 4)*7, origin = "2011-08-15"))
& Price %in% c("high, > 5")
& Item %in% MoneyDefIDs$Item[1:10]
)
),
aes(
x = as.Date(Date), 
y = betweenness_norm, 
colour = Item,
shape = Item),
size = 4) +
theme_bw() + 
scale_x_date(labels = date_format("%b-%Y"), 
breaks = "6 month", 
minor_breaks = "1 month") + 
xlab("Date") + 
ylab("Betweenness (Normalized)") +
#ggtitle("Item Betweeness, high value items") +
CurtisGGplotTheme + 
theme(
legend.position = "top") +
guides(col = guide_legend(ncol = 4)) +
theme(legend.title=element_blank()) + 
theme(legend.key.width = unit(2.1, "cm"))+
scale_colour_manual(values = getPalette(colourCount)[2:11]) +
scale_shape_manual(values = c(17, 15,0,7, 1,18, 5,8,32,32)) +
scale_linetype_manual(values = c(2,3,4,5,6,2,3,4,1))

# 10 x 5.5 for paper
# pdg 10w x 7h


```


High value goods network betweenness


```{r, echo=F}


MoneyDefIDs = read.table(text ="
i 'Item'
Money  'Money'
5021_440_6  'Key'
5002_440_6  'Refined Metal'
5001_440_6  'Reclaimed Metal'
5000_440_6  'Scrap Metal'
126_440_6  'Bills Hat'
143_440_6  'Earbuds'
5068_440_6_40  'Salvaged Mann Co. Supply Crate Series 40   ' 
5022_440_6  'Mann Co. Supply Crate'
5072_440_6  'All Other Items'
", header = T)

#                         5072_440_6  'Naughty Winter Key 2011'
#                         5073_440_6  'Nice Winter Key 2011'
#                          5071_440_6  'Nice Winter Crate 2011'
#                          5079_440_6  'Scorched Key'
#                          5078_440_6  'Scorched Crate'
#                          5081_440_6  'Fall Key 2012'
#                          5080_440_6  'Fall Crate 2012'
#                          5628_440_6  'Eerie Key'
#                          5627_440_6  'Eerie Crate'
#                          5631_440_6  'Naughty Winter Key 2012'
#                          5629_440_6  'Naughty Winter Crate 2012   '
#                          
#                          5630_440_6  'Nice Winter Crate 2012'
#                          5636_440_6  'Robo Key'
#                          5635_440_6  'Robo Crate'
# 

MoneyDefIDs$Item <- factor(MoneyDefIDs$Item, levels = MoneyDefIDs$Item)

GoodsNetwork.nodeLevelStats.plot <- dplyr::left_join(
GoodsNetwork.nodeLevelStats, MoneyDefIDs, 
by = "i"
)

colourCount = length(unique(MoneyDefIDs$i))
library("RColorBrewer")
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

#unique(GoodsNetwork.nodeLevelStats$Price)
# [1] Low, <0.95              high, > 5               mid, Price < 5 & > 0.95

library(scales)

ValveSales = data.frame(
StartDate = as.Date(c("12/15/2011","6/27/2012","9/6/2012","10/26/2012","12/13/2012","5/14/2013"), "%m/%d/%Y"),
EndDate   = as.Date(c("1/4/2012","7/11/2012","9/21/2012","11/8/2012","1/3/2013","5/28/2013"), "%m/%d/%Y")
)

ggplot2::ggplot(
GoodsNetwork.nodeLevelStats.plot %>%
dplyr::filter(i %in% MoneyDefIDs$i &
Price %in% c("Low, <0.95"))
) +
geom_rect(data = ValveSales, aes(xmin = as.Date(StartDate), xmax = as.Date(EndDate), ymin=-Inf, ymax=+Inf), fill='cadetblue4', alpha=0.15) +
geom_line(
data = (GoodsNetwork.nodeLevelStats.plot %>%
dplyr::filter(
Price %in% c("Low, <0.95" )
& is.na(Item)
)), 
aes(x = as.Date(Date),
y = betweenness_norm,
group = i),
size = 1, 
colour = "grey"
) +
geom_line(
data = (
GoodsNetwork.nodeLevelStats.plot %>%
dplyr::filter(
Price %in% c("Low, <0.95" )
& Item %in% MoneyDefIDs$Item[1:10]
)
),
aes(
x = as.Date(Date), 
y = betweenness_norm, 
color = Item, 
shape = Item,
linetype = Item), 
size = 1.2) +
geom_point(
data = (
GoodsNetwork.nodeLevelStats.plot %>%
dplyr::filter(
Date %in% as.character(as.Date(seq(0,93, by = 4)*7, origin = "2011-08-15"))
& Price %in% c("Low, <0.95" )
& Item %in% MoneyDefIDs$Item[1:10]
)
),
aes(
x = as.Date(Date), 
y = betweenness_norm, 
colour = Item,
shape = Item),
size = 4) +
theme_bw() + 
scale_x_date(labels = date_format("%b-%Y"), 
breaks = "6 month", 
minor_breaks = "1 month") + 
xlab("Date") + 
ylab("Betweenness (Normalized)") +
#ggtitle("Item Betweeness, low value items") +
CurtisGGplotTheme + 
theme(legend.position = "top") +
guides(col = guide_legend(ncol = 4)) +
theme(legend.title=element_blank())+ 
theme(legend.key.width = unit(2.1, "cm"))+
scale_colour_manual(values = getPalette(colourCount)[2:11]) +
scale_shape_manual(values = c(17, 15,0,7, 1,18, 5,8,32,32)) +
scale_linetype_manual(values = c(2,3,4,5,6,2,3,4,1))

# 10 x 5.5in for paper version
# pdg 10w x 5.8h


```
