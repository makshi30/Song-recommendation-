library(dplyr)
library(ggplot2)
library(stringr)
glimpse(Spotify)

# Remove NA's
spot <- na.omit(Spotify)

# Filter unique song
spot <- spot[!duplicated(spot$track_id),]

# Adjust some row data type
spot <- spot %>% 
  mutate(time_signature = as.numeric(time_signature),
         popularity = as.numeric(popularity),
         duration_ms = as.numeric(duration_ms))
#Popularity Distribution
hist(spot$popularity)
hist(spot$popularity,breaks = 4)

#Dividing popularity into 4 groups
spot <- spot %>% 
  mutate(pop_gr = as.factor(case_when(((popularity > 0) & (popularity < 20)) ~ "1",
                                      ((popularity >= 20) & (popularity < 40))~ "2",
                                      ((popularity >= 40) & (popularity < 60)) ~ "3",
                                      TRUE ~ "4")))
table(spot$pop_gr)


colnames(spot)

#Selection of Variables
spot1 <- spot %>% 
  select(acousticness,loudness,danceability,energy,liveness,valence,popularity,track_name,artist_name,genre,pop_gr)
glimpse(spot1)

#Variable scaling
spot1z <- as.data.frame(scale(spot1[,c(1:7)]))
summary(spot1z)

#Elbow method to identify the number of clusters
wss <- function(data, maxCluster = 9) {
  SSw <- (nrow(data) - 1) * sum(apply(data, 2, var))
  SSw <- vector()
  for (i in 2:maxCluster) {
    SSw[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:maxCluster, SSw, type = "o", xlab = "Number of Clusters", ylab = "Within groups sum of squares", pch=19)
}
wss(spot1z)

#K-means
set.seed(212)
km1 <- kmeans(spot1z,3,nstart = 20)
km_cl <- km1$cluster
km_ct <- data.frame(km1$centers,clust = rownames(km1$centers))

#PCA
pc1 <- prcomp(spot1z[,1:7],center = T)
summary(pc1)

#Combining result with PCA and Inital Data
spot1z <- data.frame(spot1z,spot1[,-c(1:7)])
spot1z$clust <- as.factor(km_cl)
pr1 <- data.frame(pc1$x, clust = factor(km_cl), pop_gr = spot1$pop_gr, genre = spot$genre)

#Resulting Biplot
datapc <- data.frame(varnames = rownames(pc1$rotation), pc1$rotation)
x <- "PC1"
y <- "PC2"
data <- data.frame(obsnames=seq(nrow(pc1$x)), pc1$x)
mult <- min(
  (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
  (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x]))))
datapc <- transform(datapc,
                    v1 = .9 * mult * (get(x)),
                    v2 = .9 * mult * (get(y)))
ggplot(pr1, aes(x=PC1, y=PC2)) +
  geom_hline(aes(yintercept=0), size=.2) + 
  geom_vline(aes(xintercept=0), size=.2) +
  coord_equal() +
  geom_point(aes(color = clust),size = 0.2) +
  geom_segment(data = datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow = arrow(length=unit(0.2, "cm"))) +
  geom_text_repel(data = datapc, aes(label=str_to_title(varnames)),point.padding = -10,segment.size = 0.5) +
  theme_dark()+
  scale_color_brewer(palette = "Pastel1")+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(title = "3 Clusters with PCA and Factor Loading",color = "Cluster")

#Scaling the data with min max to convert data range to 0-1
normalize <- function(x){
  return ( 
    (x - min(x))/(max(x) - min(x)) 
  )
}
spot1gg <- normalize(spot1z[,c(1:7)])
spot1gg <- cbind(spot1gg,spot1z[,-c(1:7)])

spot1gg %>% 
  group_by(clust) %>% 
  summarise(Acoustic = mean(acousticness),
            Loud = mean(loudness),
            Dance = mean(danceability),
            Energy = mean(energy),
            Liveness = mean(liveness),
            Valence = mean(valence)) %>% 
  select(Loud,Acoustic,Dance,Energy,Liveness,Valence,clust) %>% 
  gather("Name","Value",-clust) %>% 
  ggplot(aes(y=Value,x = clust,col=Name,group = Name)) +
  geom_point()+
  geom_line()+
  facet_wrap(~Name,scales = "free_y")+
  scale_color_brewer(palette = "Pastel1")+
  theme_dark()+
  labs(x="Cluster",col = "Attributes",title = "Attributes for each cluster")

#Cluster Popularity Attribute
spot1gg %>% 
  group_by(clust) %>% 
  summarise(Popularity = mean(popularity)) %>% 
  select(Popularity,clust) %>% 
  gather("name","value",-clust) %>% 
  ggplot(aes(y=value,x = clust,group = name))+
  geom_point()+
  geom_line()+
  theme_dark()+
  labs(y = "Popularity Value",x="Cluster",title = "Average Popularity for each cluster")




