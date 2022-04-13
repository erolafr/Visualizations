#### Contour plot for climatic niche for Lantana camara #####
# Author: Erola Fenollosa
# Data sources:
### GBIF: https://www.gbif.org/occurrence/map?taxon_key=2925303 [Access 13/04/2022]
### WorldClim Bioclimatic variables at res = 5: https://www.worldclim.org/data/bioclim.html


# Packages:
library(rgbif)
library(maps)
library ("dismo")
library(MASS)

# Define the species name and download its occurrence data from GBIF:
myspecies <- c("Lantana camara")
gbif_data <- occ_data(scientificName = myspecies, hasCoordinate = TRUE, limit= 70000)
gbif_data

# get the columns that matter for mapping and cleaning the occurrence data:
myspecies_coords <- gbif_data$data[ , c("decimalLongitude", "decimalLatitude", "individualCount", "occurrenceStatus", "coordinateUncertaintyInMeters", "institutionCode", "references")]
head(myspecies_coords)

# map the occurrence data:
map("world", xlim = range(myspecies_coords$decimalLongitude), ylim = range(myspecies_coords$decimalLatitude))  
points(myspecies_coords[ , c("decimalLongitude", "decimalLatitude")], pch = ".")

# Store only the coords:
cord <- data.frame (lon = myspecies_coords$decimalLongitude, lat=myspecies_coords$decimalLatitude) 
cord<- cord[complete.cases(cord), ]

# Download WorldClim bioclimatic variables at res = 5
r5 <- getData("worldclim",var="bio",res=5)

# Extract bioclimatic values for each species occurence:
points <- SpatialPoints(cord, proj4string = r5@crs)  # transform coords to points
values <- extract(r5,points) 
df <- cbind.data.frame(coordinates(points),values) 

# Save data
write.csv(df, file="lantana.csv") 
lantana<- df[complete.cases(df), ]

# Data with only bioclimatic variables:
lantana_bios <- lantana[,3:21]
head(lantana_bios)

# Prepare the contour plot considering BIO1 (annual temperature) and BIO12 (annual precipitation). Remember that temperature is x10
# Visualize the plot:
z <- kde2d(lantana_bios$bio1/10, lantana_bios$bio12, n = 150, lims=c(0, 30, 0, 8000))
plot(lantana_bios$bio1/10, lantana_bios$bio12, xlim = c(0,30), ylim = c(0,8000), pch = 19, xlab="Annual mean temperature (ºC)", ylab="Annual precipitation (mm)", col= "gray", main=substitute(paste(italic("Lantana camara")," climatic niche")))
text(2.3, 8000, "GBIF Access 13/4/22", cex = .7 )
text(1.8, 7800, "WorldClim res = 5" , cex = .7 )
contour(z, lwd = 2, add = TRUE, col = hcl.colors(30, "Spectral"), drawlabels = FALSE)

# Save the plot:
pdf("LantanaCamara.pdf") 
plot(lantana_bios$bio1/10, lantana_bios$bio12, xlim = c(0,30), ylim = c(0,8000), pch = 19, xlab="Annual mean temperature (ºC)", ylab="Annual precipitation (mm)", col= "gray", main=substitute(paste(italic("Lantana camara")," climatic niche")))
text(2.3, 8000, "GBIF Access 13/4/22", cex = .7 )
text(1.8, 7800, "WorldClim res = 5" , cex = .7 )
contour(z, lwd = 2, add = TRUE, col = hcl.colors(10, "Spectral"), drawlabels = FALSE)
dev.off()
