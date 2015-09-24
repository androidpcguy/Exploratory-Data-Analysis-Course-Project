require(ggplot2)
require(dplyr)


#Adds up emissions from combusting coal across US by year. I only
#consider the specific act of combusting coal in my analysis (no
#mining, etc.). 

#FROM THE PLOT, WE CAN SEE THAT PM25 EMISSIONS FROM COAL COMBUSTION
#HAS DECREASED FROM 1999 TO 2008.
plot4 <- function() {
	NEI <- readRDS("./data/summarySCC_PM25.rds")
	SCC <- readRDS("./data/Source_Classification_Code.rds")
	
	#Searching for 'Coal' in EI.Sector variable yields all combustion
	#relating to Coal, which is what I want. I do not consider mining for Coal
	#as combustion. I only consider the act of combusting coal in this plot.
	good_SCC <- as.character(SCC$SCC[grep("Coal",SCC$EI.Sector)])
	good_NEI <- subset(NEI, NEI$SCC %in% good_SCC)
	
	#sum emissions by year
	mdata <- aggregate(Emissions ~ year, data=good_NEI, sum)

	#divide by 1000 to prevent R from choosing scientific notation for axis labels
	mdata$Emissions <- mdata$Emissions / 1000
	png("./Plot4.png",width = 550, height = 550)
	
	#NOTE: y axis units are (1000 tons)
	mplot <- qplot(year, Emissions, data=mdata) + geom_line() + ylab("Emissions (1000 tons)") + xlab("Year") + scale_x_continuous(breaks=seq(1999,2008,3), labels=as.character(seq(1999,2008,3))) + ggtitle("PM25 Emissions from Coal combustion\nRelated sources from 1999-2008 across US")

	print(mplot)
	dev.off()

}
