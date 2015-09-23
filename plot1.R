plot1 <- function() {
	NEI <- readRDS("./data/summarySCC_PM25.rds")
	total_emissions <- tapply(NEI$Emissions, NEI$year, sum)
	
	emissions <- data.frame(total_emissions)
	emissions$Year <- c(1999, 2002, 2005, 2008)
	emissions$total_emissions <- emissions$total_emissions / 1000
	png("./Plot1.png",width = 500, height = 500)
	par(mfrow=c(1,1))

	mplot <- plot(emissions$Year, emissions$total_emissions, ylab="PM 2.5 Emissions (1000 tons)", xlab = "Year",ylim=c(3000,7500), xlim=c(1999,2008),xaxt="n", main="PM 2.5 Emissions during years 1999-2008")
	lines(emissions$Year, emissions$total_emissions)

	axis(1,at = seq(1999,2008,3),labels=seq(1999,2008,3))
	
	print(mplot)
	dev.off()
}
