require(ggplot2)
require(dplyr)
#Sums up Emissions by year and by type and plots using ggplot2.
#Color coded for each Emission type

#ALL EMISSION TYPES EXCEPT 'POINT' HAVE SEEN DECREASE IN PM2.5
#EMISSIONS. 'POINT' SOURCES HAVE SEEN SLIGHT INCREASE IN EMISSIONS.
plot3 <- function() {
	NEI <- readRDS("./data/summarySCC_PM25.rds")
	#subset to only baltimore city data
	NEI_baltimore_city <- subset(NEI, NEI$fips == "24510")
	#sum Emissions by year and by type, returns data.frame
	mdata <- aggregate(Emissions ~ year + type, data=NEI_baltimore_city, sum)
	png("./Plot3.png",width = 550, height = 550)

	mplot <- qplot(year, Emissions, data=mdata, color=type) + geom_line() + 
		ylab("Emissions (tons)") + xlab("Year") + scale_x_continuous(breaks=seq(1999,2008,3),
		labels=as.character(seq(1999,2008,3))) + ggtitle("Contributions of PM25 Emissions\nfrom different sources from 1999-2008 in Baltimore City") + scale_color_discrete(name = "Emission type")

	print(mplot)
	dev.off()	
}
