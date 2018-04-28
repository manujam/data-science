library(reshape2)
library(ggplot2)

currencies <- read.csv("currencies.csv", header = TRUE)
currencies <- as.data.frame(sapply(currencies, as.numeric))
curr_cor <- round(cor(currencies[,c(-1,-21,-22,-27)], use = "pairwise.complete.obs"),2)

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
curr_cor <- reorder_cormat(curr_cor)
melted_cor <- melt(curr_cor, na.rm = TRUE)

# Heatmap
ggheatmap <- ggplot(data = melted_cor, aes(Var2, Var1, fill = value)) + 
  geom_tile(color = "black") +
  scale_fill_gradientn(colours = terrain.colors(10), 
                       limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1)) +
  coord_fixed()

# ggheatmap + 
#   geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
#   theme(
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.border = element_blank(),
#     panel.background = element_blank(),
#     axis.ticks = element_blank(),
#     legend.justification = c(1, 0),
#     legend.position = c(0.6, 0.7),
#     legend.direction = "horizontal")+
#   guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
#                                title.position = "top", title.hjust = 0.5))

print(ggheatmap)
