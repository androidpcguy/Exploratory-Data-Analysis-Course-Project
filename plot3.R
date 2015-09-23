require(ggplot2)
require(dplyr)

plot3 <- function() {
	NEI <- readRDS("./data/summarySCC_PM25.rds")
	NEI_baltimore_city <- subset(NEI, NEI$fips == "24510")
	mdata <- aggregate(Emissions ~ year + type, data=NEI_baltimore_city, sum)
	png("./Plot3.png",width = 550, height = 550)

	mplot <- qplot(year, Emissions, data=mdata, color=type) + geom_line() + 
		ylab("Emissions (tons)") + xlab("Year") + scale_x_continuous(breaks=seq(1999,2008,3),
		labels=as.character(seq(1999,2008,3))) + ggtitle("Contributions of PM25 Emissions\nfrom different sources from 1999-2008 in Baltimore City") + scale_color_discrete(name = "Emission type")

	print(mplot)
	dev.off()	
}
