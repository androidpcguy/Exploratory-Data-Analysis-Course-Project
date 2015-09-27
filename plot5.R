require(dplyr)
require(ggplot2)

#TODO: add comments

#Sums up emssions by ALL mobile sources (cars, trucks, ships, aircraft, etc...) by year and displays data on a line graph.

#EMISSIONS DECREASED FROM 1999 TO 2002 AND STAYED ABOUT THE SAME TILL 2005 BUT INCREASED FROM 2005 TO 2008.
plot5 <- function() {
	NEI <- readRDS("./data/summarySCC_PM25.rds")
	NEI_baltimore_city <- subset(NEI, NEI$fips == "24510")
	SCC <- readRDS("./data/Source_Classification_Code.rds")


	#searching for "Mobile -" in order to account for all types
        #of motorized vehicles (ships, cars, trucks, tractors, aircraft, etc.)
        #essentially anything that burns fuel to move
	good_SCC <- as.character(SCC$SCC[grep("Mobile -",SCC$EI.Sector)])
	
	#get good NEI data by subsetting good SCC codes within baltimore city-only
	#data
	good_NEI <- subset(NEI_baltimore_city, NEI_baltimore_city$SCC %in% good_SCC)
	
	#aggregate by summing Emissions by year
	mdata <- aggregate(Emissions ~ year, data=good_NEI, sum)
	png("./Plot5.png",width = 550, height = 550)

	mplot <- qplot(year, Emissions, data=mdata) + geom_line() + ylab("Emissions (tons)") + xlab("Year") + scale_x_continuous(breaks=seq(1999,2008,3), labels=as.character(seq(1999,2008,3))) + ggtitle("PM25 Emissions from Motor Vehicle sources from 1999-2008 in Baltimore City")

	print(mplot)
	dev.off()
	#TESTING return(mdata)
}
