# data was provided as working material for in chapter 5 of Railsback and Grimm 2011
# Railsback, S. and Grimm, V. (2012). Agent-Based and Individual-Based Modeling. Princeton: Princeton University Press.

library(usethis)

rawData <- read.delim(url("http://www.railsback-grimm-abm-book.com/E2-Downloads/Chapter05/Exercise10_ElevationData.txt"))

butterflyData_Original <- rawData
names(butterflyData_Original) <- c("x", "y", "elev")

butterflyData_Netlogo <- data.frame("x"=(rawData$Easting-min(rawData$Easting))/10,
                  "y"=(rawData$Northing-min(rawData$Northing))/10,
                  "elev"=rawData$Elevation)


write.table(butterflyData_Netlogo, file = "butterflyData_Netlogo.txt", sep = "\t", col.names = F, row.names = F)
write.csv(butterflyData_Original, file = "butterflyData_Original.csv", col.names = T, row.names = F)

usethis::use_data(butterflyData_Original, overwrite = TRUE)
usethis::use_data(butterflyData_Netlogo, overwrite = TRUE)
