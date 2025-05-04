library(readxl)
library(cluster)
library(factoextra)

data_PK5 = read_excel("OneDrive - Quinnipiac University/24-25/Spring/DS480/Project/DSCapstone/RankingTables/PK5_ranking.xlsx")

df_PK5 = scale(data_PK5)

fviz_nbclust(df_PK5, kmeans, method = "wss", k.max = 5)

set.seed(123)
km_PK5 <- kmeans(data_PK5, 3, nstart = 25)

fviz_cluster(km_PK5,df_PK5)

print(km_PK5)

#plot(data_PK5$scale_1_5,data_PK5$min_sum)