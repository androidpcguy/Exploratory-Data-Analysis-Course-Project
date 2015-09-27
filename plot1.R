require(dplyr)


#Creates plot to show toal PM2.5 emission from all sources across US
#during years 1999-2008.
#TOTAL EMISSION OF PM2.5 CLEARLY DECREASES DURING 1999-2008 ACROSS US
plot1 <- function() {
	
	NEI <- readRDS("./data/summarySCC_PM25.rds")
	
	#Sum emissions by year
	total_emissions <- aggregate(Emissions ~ year, data=NEI, sum)
	
	#divide by 1000 to show on graph in units of (1000s tons)
	total_emissions$Emissions <- total_emissions$Emissions / 1000
	png("./Plot1.png",width = 500, height = 500)
	par(mfrow=c(1,1))
	
	#Note the y axis units are in (1000s tons)
	mplot <- plot(total_emissions$year, total_emissions$Emissions, ylab="PM 2.5 Emissions (1000 tons)", xlab = "Year",ylim=c(3000,7500), xlim=c(1999,2008),xaxt="n", main="PM 25 Emissions during years 1999-2008 across US")
	lines(total_emissions$year, total_emissions$Emissions)

	axis(1,at = seq(1999,2008,3),labels=seq(1999,2008,3))
	
	print(mplot)
	dev.off()
}
