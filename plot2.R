plot2 <- function() {
	NEI <- readRDS("./data/summarySCC_PM25.rds")

	NEI <- subset(NEI, NEI$fips == "24510")
	total_emissions <- tapply(NEI$Emissions, NEI$year, sum)
        emissions <- data.frame(total_emissions)
        emissions$Year <- c(1999, 2002, 2005, 2008)
        png("./Plot2.png",width = 500, height = 500)
        par(mfrow=c(1,1))
	
	mplot <- plot(x=emissions$Year, y=emissions$total_emissions, main="Total Emissions of PM25 in Baltimore City from Years 1999-2008", xlab="Year", ylab="PM25 Emissions (tons)", xaxt="n",ylim=c(1300,3300),pch=19)
	lines(emissions$Year, emissions$total_emissions)
	axis(1, at=seq(1999,2008,3), labels=seq(1999,2008,3))
	print(mplot)
	dev.off()
}
