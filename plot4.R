require(ggplot2)
require(dplyr)

plot4 <- function() {
	NEI <- readRDS("./data/summarySCC_PM25.rds")
	SCC <- readRDS("./data/Source_Classification_Code.rds")

	good_SCC <- as.numeric(as.character(SCC$SCC[grep("Coal",SCC$EI.Sector)]))
	good_NEI <- subset(NEI, NEI$SCC %in% good_SCC)
	
	mdata <- aggregate(Emissions ~ year, data=good_NEI, sum)
	mdata$Emissions <- mdata$Emissions / 1000
	png("./Plot4.png",width = 550, height = 550)

	mplot <- qplot(year, Emissions, data=mdata) + geom_line() + ylab("Emissions (1000 tons)") + xlab("Year") + scale_x_continuous(breaks=seq(1999,2008,3), labels=as.character(seq(1999,2008,3))) + ggtitle("PM25 Emissions from Coal combustion\nRelated sources from 1999-2008 across US")

	print(mplot)
	dev.off()

}
