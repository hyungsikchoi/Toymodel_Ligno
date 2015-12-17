library(ggplot2)


PI.miscan  <- 80      # 80 euro/ton
PI.land    <- 200
elast      <- 0.5     # price elasticity for area miscantus or SRC
elast.land <- 0.05    # price elasticity for area miscantus or SRC

area.miscan <- 10      #  1000 ha for Germany  2mil ha for

tzero <- 16
alpha <- 0.3          # diffusion rate
time <- 1:30    
adop.rate <- 1/(1+exp(-alpha*(time-tzero)))
plot(time,adop.rate,type='l')
##test for adoption rate plot

Peak.area <- 200

out <- data.frame()

adop.rate <- NULL

for (i in 1:5) {
adop.rate[i] <- 1/(1+exp(-alpha*(6*(i-1)-tzero)))
}
a <- adop.rate
adop.rate <-rep(a,each=50)

price.mis  <- seq(1,500,10)
price.mis <- rep(price.mis,5)


ini <- log(area.miscan)-elast*log(PI.miscan )-elast.land*log(PI.land)

area.mis <- Peak.area*adop.rate*exp(ini + elast*log(price.mis)+elast.land*log(PI.land))


y <- c(6,12,18,24,30)
year <- rep(y,each=50)
year <- as.factor(year)
out  <- data.frame(price.mis, area.mis, year) 



p <- ggplot(out,aes(x=area.mis,y=price.mis,group=year,colour=year)) 
p <- p + geom_line(size=0.8)
p <- p + theme_bw()

p <- p + theme(axis.text.x = element_text(angle = 0, hjust = 1, size=15,face="bold"))
p <- p + theme(axis.text.y = element_text(angle = 0, hjust = 1, size=15,face="bold"))

p <- p+ ggtitle("LB suppy shift for 30years") + theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=29,vjust = 2))

p <- p+ xlab("Area (1000 ha)") + ylab("biomass price (euro/ton)")
p <- p + theme(axis.title.y = element_text(size = rel(1.8), angle = 90,vjust = 2))
p <- p + theme(axis.title.x = element_text(size = rel(1.8), angle = 00,vjust = 0.5))

p + ylim(0,700)
 
print(p)
