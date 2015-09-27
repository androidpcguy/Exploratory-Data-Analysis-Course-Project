require(ggplot2)
require(dplyr)


#aggregates emissions from motor sources (cars, trucks, tractors, ships, aircraft, etc...) by year and city and displays both sets of data on one plot
#with different colors for LA and Baltimore City

#LA HAS SEEN A BIGGER CHANGES IN EMISSIONS OVER TIME THAN BALTIMORE CITY. LA HAS HIGHER EMISSIONS FROM MOTOR SOURCES OVERALL THAN BALTIMORE CITY.
plot6 <- function() {

        NEI <- readRDS("./data/summarySCC_PM25.rds")
	#Subset LA and Baltimore City
	NEI_sub <- subset(NEI, NEI$fips == "24510" | NEI$fips == "06037")

	SCC <- readRDS("./data/Source_Classification_Code.rds")
	
	#searching for "Mobile -" in order to account for all types
	#of motorized vehicles (ships, cars, trucks, tractors, aircraft, etc.)
	#essentially anything that burns fuel to move
	good_SCC <- as.character(SCC$SCC[grep("Mobile -",SCC$EI.Sector)])
	good_NEI <- subset(NEI_sub, NEI_sub$SCC %in% good_SCC)
	
	#sum Emissions by year and city
	mdata <- aggregate(Emissions ~ year + fips, data=good_NEI, sum)

        png("./Plot6.png",width = 550, height = 550)

	#display data from both cities on one plot, different colors for each city
        mplot <- qplot(year, Emissions, data=mdata, color=fips) + geom_line() +
                ylab("Emissions (tons)") + xlab("Year") + scale_x_continuous(breaks=seq(1999,2008,3),
                labels=as.character(seq(1999,2008,3))) + ggtitle("Comparison of PM25 Emissions\nfrom Motor Vehicle sources from 1999-2008 in\nLos Angeles, CA and Baltimore City, MD") + scale_color_discrete(name = "Emission type", labels=c("Los Angeles", "Baltimore City"))

        print(mplot)
        dev.off()
}

