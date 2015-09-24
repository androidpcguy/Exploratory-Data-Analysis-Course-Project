require(dplyr)

#Calculates sum of PM2.5 emissions for each year and plots it
#TOTAL EMISSIONS OF PM2.5 HAVE SEEN A DECREASE FROM 1999 TO 2008 IN
#BALTIMORE CITY
plot2 <- function() {
	NEI <- readRDS("./data/summarySCC_PM25.rds")
	
	#subset to only baltimore city data
	NEI_balt_city <- subset(NEI, NEI$fips == "24510")
	emissions <- aggregate(Emissions ~ year, data=NEI_balt_city, sum)
        png("./Plot2.png",width = 500, height = 500)
        par(mfrow=c(1,1))
	
	mplot <- plot(x=emissions$year, y=emissions$Emissions, main="Total Emissions of PM25 in Baltimore City from Years 1999-2008", xlab="Year", ylab="PM25 Emissions (tons)", xaxt="n",ylim=c(1300,3300),pch=19)
	lines(emissions$year, emissions$Emissions)
	axis(1, at=seq(1999,2008,3), labels=seq(1999,2008,3))
	print(mplot)
	dev.off()
}
