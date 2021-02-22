################## Housekeeping#######################
rm(list = ls())  #Clear the workspace
invisible (cat("\014"))  #Clear the console

####Assignement 5#####

####Loading data and packages####
library(here)
library(ggplot2)

dat <- read.csv("./data/siscowet.csv", header=T)
head(dat)

####Exploratory plot####

(p1 <- ggplot(dat,aes(x=len, y=wgt, color = locID)) +
      geom_point(na.rm = T))

ggsave(p1, filename="exploratory.png",path="./figures/")

####Expository plot####

##new data
newdat <- dat[which(dat$wgt<15000),]

##ggplot theme
theme_example <- function (base_size = 12, base_family = "") {
  theme_gray(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.text = element_text(colour = "black",size=16),
          axis.title.x = element_text(colour = "black",size = 22),
          axis.title.y = element_text(colour = "black",size=22, angle = 90),
          panel.background = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major = element_blank(),
          plot.background = element_blank(),
          legend.key = element_blank(),
          legend.text = element_text(size=16),
          legend.title = element_text(size=18)
    )
}

library("viridis") 

Location <- unique(newdat$locID)
for(i in 1:length(Location)){
  sub<-subset(newdat,newdat$locID==Location[i])
  
  plots <- ggplot(sub, aes(x=len, y=wgt))+
    geom_jitter(na.rm=T, pch=21, cex=4, aes(fill=mesh))+
    scale_fill_viridis(discrete = F)+
    xlim(c(0, 800))+
    ylim(c(0,4300))+
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=Inf, yend=-Inf)+
    xlab("Total length (mm)")+
    ylab("Weight (g)")+
    theme_example()+
    ggtitle(Location[i])

  ggsave(plots, filename=paste("weightLength_",Location[i],".png",sep=""),path="./figures")
}


