library(gdxrrw)
library(reshape2)
igdx("C:/GAMS/win64/24.4/")

#gams('toy_model.gms')

location="./input/results.gdx"

res=rgdx.param(location,'report',compress=TRUE)


colnames(res) =c('land','Potential','num','country','item','biomass','value')

res$year = as.numeric(res$num)

library(dplyr)


output <-res %>%
  filter(land =='yes' & item =='area' & Potential =='mid')

output2 <-res %>%
  filter(land =='yes' & item =='landPr' & biomass =='ligno' )


library(ggplot2)
#######################
##figure for land area
#######################

#p <- ggplot(output,aes(x=year,y=value,group=biomass,colour=biomass))
#p <- p + geom_line(size=1.8)
#p <- p + theme_bw()

#p <- p + theme(axis.text.x = element_text(angle = 0, hjust = 1, size=15,face="bold"))
#p <- p + theme(axis.text.y = element_text(angle = 0, hjust = 1, size=15,face="bold"))

#p <- p+ ggtitle("Land area") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=29,vjust = 2))

#p <- p + ylim(0, 17000)

#p <- p + xlab("Years") + ylab("Area (1000ha)")
#p <- p + theme(axis.title.y = element_text(size = rel(1.8), angle = 90,vjust = 2))
#p <- p + theme(axis.title.x = element_text(size = rel(1.8), angle = 00,vjust = 0.5))

#print(p)

#######################
##figure for land price
#######################

#p2 <- ggplot(output2,aes(x=year,y=value,group=Potential,colour=Potential))
#p2 <- p2+ geom_line(size=1.8)
#p2 <- p2 + theme_bw()

#p2 <- p2 + theme(axis.text.x = element_text(angle = 0, hjust = 1, size=15,face="bold"))
#p2 <- p2 + theme(axis.text.y = element_text(angle = 0, hjust = 1, size=15,face="bold"))

#p2 <- p2+ ggtitle("Land price") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=29,vjust = 2))

#p2 <- p2+ xlab("Years") + ylab("land rent (euro/ha)")
#p2 <- p2 + theme(axis.title.y = element_text(size = rel(1.8), angle = 90,vjust = 2))
#p2 <- p2 + theme(axis.title.x = element_text(size = rel(1.8), angle = 00,vjust = 0.5))

#print(p2)





#######################
##(1) figure for land area in bar plot multiple plot
#######################


###data for 30 years
output3 <-res %>%
  filter( item =='area'  & num =='30' & biomass !='gap' ) %>%
  mutate(value = value*0.001)
#  mutate(new.land= factor(ifelse(land =='Land_exp','Land expansion','Land stagnation')))

  
p <- ggplot(output3,aes(x=biomass,y=value,fill=Potential))

p <- p + facet_grid(. ~ land)
p <- p +  geom_bar(stat="identity",position="dodge")
p <- p + theme_bw()


p <- p + theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=15,face="bold"))

p <- p + theme(axis.text.y = element_text(angle = 0, hjust = 0.5, size=15,face="bold"))
### label in the facet grid
p <- p + theme(strip.text.x = element_text(size = 15,face="bold",colour = "black", angle = 0))


p <- p+ ggtitle("Land area change in Germany") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=29,vjust = 2))

p <- p+ xlab("") + ylab("Area (mil. ha)")
p <- p + theme(axis.title.y = element_text(size = rel(1.8), angle = 90,vjust = 2))
p <- p + theme(axis.title.x = element_text(size = rel(1.8), angle = 00,vjust = 0.5))

p + guides(fill = guide_legend(title = "Ligno.Potential", title.theme = element_text(size=15, face="bold",angle = 0)))

#ggsave(p, file="fig1.tiff",dpi=1000, compression = "lzw", height=5, width=10, units="in")




#######################
##(2) figure for land price in bar plot
#######################
###data for 30 years
output4 <-res %>%
  filter( item =='landPr'  & num =='30' & biomass =='crop' ) %>%
  mutate(value = value) 
#  mutate(new.land= factor(ifelse(land =='Land_exp','Land expansion','Land stagnation')))
#  mutate(value = (ifelse(land =='no',0,value)))
  

p <- ggplot(output4,aes(x=Potential,y=value,fill=Potential))
p <- p + facet_grid(. ~ land)
p <- p+  geom_bar(stat="identity",position="dodge")
p <- p + theme_bw()

p <- p + theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=15,face="bold"))
p <- p + theme(axis.text.y = element_text(angle = 0, hjust = 0.5, size=15,face="bold"))

### label in the facet grid
p <- p + theme(strip.text.x = element_text(size = 15,face="bold",colour = "black", angle = 0))


p <- p+ ggtitle("Land price change in Germany") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=29,vjust = 2))

p <- p+ xlab("") + ylab("land price (euro/ha)")
p <- p + theme(axis.title.y = element_text(size = rel(1.8), angle = 90,vjust = 2))
p <- p + theme(axis.title.x = element_text(size = rel(1.8), angle = 00,vjust = 0.5))

p + guides(fill = guide_legend(title = "Ligno.Potential", title.theme = element_text(size=15, face="bold",angle = 0)))


#ggsave(p, file="fig2.tiff",dpi=1000, compression = "lzw", height=5, width=10, units="in")



#######################
##(3) crop price index in bar plot
#######################
###data for 30 years
output5 <-res %>%
  filter(item =='price'   &  (biomass =='index')) %>%
  mutate(value = value)
#  mutate(new.land= factor(ifelse(land =='Land_exp','Land expansion','Land stagnation')))

#  mutate(value = (ifelse(land =='no',0,value)))


p <- ggplot(output5,aes(x=num,y=value,group=Potential,colour=Potential))
p <- p + facet_grid(. ~ land)
p <- p + geom_line( size = 2)
p <- p + theme_bw()

p <- p + theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=15,face="bold"))
p <- p + theme(axis.text.y = element_text(angle = 0, hjust = 0.5, size=15,face="bold"))

### label in the facet grid
p <- p + theme(strip.text.x = element_text(size = 15,face="bold",colour = "black", angle = 0))


p <- p+ ggtitle("Crop price index change in Germany") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=29,vjust = 2))

p <- p+ xlab("") + ylab("crop price index (base year = 100)")
p <- p + theme(axis.title.y = element_text(size = rel(1.8), angle = 90,vjust = 2))
p <- p + theme(axis.title.x = element_text(size = rel(1.8), angle = 00,vjust = 0.5))

p <- p + scale_x_discrete(limits=c("1","5","10","15","20","25","30"),labels=c("1","5","10","15","20","25","30"))


p + guides(fill = guide_legend(title = "Ligno.Potential", title.theme = element_text(size=15, face="bold",angle = 0)))


#ggsave(p, file="fig3.tiff",dpi=1000, compression = "lzw", height=5, width=10, units="in")


#######################
##(4) ligno biomass price in bar plot
#######################
###data for 30 years
output6 <-res %>%
  filter( item =='price'   &  (biomass =='ligno')) %>%
  mutate(value = value) 
#  mutate(new.land= factor(ifelse(land =='Land_exp','Land expansion','Land stagnation')))

#  mutate(value = (ifelse(land =='no',0,value)))


p <- ggplot(output6,aes(x=num,y=value,group=Potential,colour=Potential))
p <- p + facet_grid(. ~ land)
p <- p+  geom_line(size = 2)
p <- p + theme_bw()

p <- p + theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=15,face="bold"))
p <- p + theme(axis.text.y = element_text(angle = 0, hjust = 0.5, size=15,face="bold"))

### label in the facet grid
p <- p + theme(strip.text.x = element_text(size = 15,face="bold",colour = "black", angle = 0))


p <- p+ ggtitle("Ligno biomass price change in Germany") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=29,vjust = 2))

p <- p+ xlab("Years") + ylab("Price (euro/ton)")
p <- p + theme(axis.title.y = element_text(size = rel(1.8), angle = 90,vjust = 2))
p <- p + theme(axis.title.x = element_text(size = rel(1.8), angle = 00,vjust = 0.5))
p <- p + scale_x_discrete(limits=c("1","5","10","15","20","25","30"),labels=c("1","5","10","15","20","25","30"))

p <- p + scale_y_continuous(limits=c(0, 500))

p + guides(fill = guide_legend(title = "Ligno.Potential", title.theme = element_text(size=15, face="bold",angle = 0)))



#ggsave(p, file="fig4.tiff",dpi=1000, compression = "lzw", height=5, width=10, units="in")




#######################
##(5) ligno biomass price in bar plot
#######################
###data for 30 years
output7 <-res %>%
  filter( item =='price'   &  (biomass =='ligno') & (land =='Land_stag') ) %>%
  mutate(value = value) 
#  mutate(new.land= factor(ifelse(land =='Land_exp','Land expansion','Land stagnation')))

#  mutate(value = (ifelse(land =='no',0,value)))


p <- ggplot(output7,aes(x=num,y=value,group=Potential,colour=Potential))
p <- p + facet_grid(. ~ land)
p <- p+  geom_line(size = 2)
p <- p + theme_bw()




