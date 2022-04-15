#### Circular dendogram for functional traits differenciation among mediterranean species #####
# Author: Erola Fenollosa
# Data sources:
### BROT 2.0 Database: https://www.uv.es/jgpausas/brot.htm [Downloaded 15/04/2022]
## Published by Tavşanoğlu and Pausas 2018 Sci Data: https://www.nature.com/articles/sdata2018135

# Note: The downloaded database has been preprocessed selecting only three traits: SLA (Specific leaf area), Seed Mass and plant height, averaging them if multiple values were registred in the database. This dendogram will be plotted with species that have data of those three functional traits.
# The three selected funcional traits correspond to the three axis approach to classify species revised by Laughlin et al 2010 (Functional Ecology), the Leaf-Height-Seed (HLS) plant strategy scheme.

# Packages:
library(readxl)
library(ape)

#Load the preprocessed data:
BROT2_dat <- read_excel("C:/Users/Erola/Desktop/BROT2_dat_3traits.xlsx", na = "NA")

# Delete species with NAs
brot_cl <-  as.data.frame(BROT2_dat[complete.cases(BROT2_dat), ])
rownames(brot_cl) <- brot_cl$Species
  
# Distance matrix
d <- dist(brot_cl[,2:4])

# Hierarchical clustering dendrogram
hc <- as.dendrogram(hclust(d))

# Circular dendrogram with 5 clusters for all species:
colors = c("firebrick3", "blue", "darkgreen", "black", "gold")
clus4 = cutree(hc, 5)
plot(as.phylo(hc), type = "fan", tip.color = colors[clus4], edge.color = "darkgray", edge.width = 1.5,  edge.lty = 2,
     label.offset = 0.5, cex = 0.6)
# Save the plot:
pdf("CircularDendogram_BROT_FunctionalTraits_ALL.pdf") 
plot(as.phylo(hc), type = "fan", tip.color = colors[clus4], edge.color = "darkgray", edge.width = 1.5,  edge.lty = 2,
     label.offset = 0.5, cex = 0.6)
dev.off()


# If we want a better visualization we could randomly sample some species (30):
brot_cl_sample <- brot_cl[sample(nrow(brot_cl), 30), ]
d <- dist(brot_cl_sample[,2:4])
hc <- as.dendrogram(hclust(d))
colors = c("firebrick3", "blue", "darkgreen", "black", "gold")
clus4 = cutree(hc, 5)
plot(as.phylo(hc), type = "fan", tip.color = colors[clus4], edge.color = "darkgray", edge.width = 1.5,  edge.lty = 2,
     label.offset = 0.5, cex = 0.6)
# Save the plot:
pdf("CircularDendogram_BROT_FunctionalTraits_Subset.pdf") 
plot(as.phylo(hc), type = "fan", tip.color = colors[clus4], edge.color = "darkgray", edge.width = 1.5,  edge.lty = 2,
     label.offset = 0.5, cex = 0.6)
dev.off()


