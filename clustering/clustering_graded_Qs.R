cricket <- read.csv("Cricket.csv", stringsAsFactors = F)
cric <- cricket[,c(1,8,10)]

cricN <- cric[,-1]
cricN$Ave <- scale(cricN$Ave)
cricN$SR <- scale(cricN$SR)

clus2 <- kmeans(cricN, centers = 2, iter.max = 50, nstart = 50)
clus3 <- kmeans(cricN, centers = 3, iter.max = 50, nstart = 50)
clus4 <- kmeans(cricN, centers = 4, iter.max = 50, nstart = 50)
clus5 <- kmeans(cricN, centers = 5, iter.max = 50, nstart = 50)

r_sq<- rnorm(20)

for (number in 1:20){clus <- kmeans(cricN, centers = number, nstart = 50)
r_sq[number]<- clus$betweenss/clus$totss
}

plot(r_sq)


cricKM <-cbind(cric,clus4$cluster)
colnames(cricKM)[4]<- "ClusterID"

ggplot(cricKM, aes(x = SR, y = Ave, colour = as.factor(cricKM$ClusterID), label = Player)) + geom_point() + geom_text(size = 3)
