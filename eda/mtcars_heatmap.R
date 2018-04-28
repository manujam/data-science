library(reshape2)
library(ggplot2)

mycars <- mtcars
mtcor <- cor(mycars) # Get correlation matrix
mtcor <- round(mtcor, 2) # Round correlation matrix to 2 decimals

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
# Reorder the correlation matrix according to the correlation coefficient
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
mtcor <- reorder_cormat(mtcor)
upper_tri <- get_upper_tri(mtcor)

# Melt the correlation matrix
melted_mtcor <- melt(upper_tri, na.rm = TRUE)

# Heatmap
ggheatmap <- ggplot(data = melted_mtcor, aes(Var2, Var1, fill = value)) + 
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "yellow", high = "red", mid = "orange", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1)) +
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
