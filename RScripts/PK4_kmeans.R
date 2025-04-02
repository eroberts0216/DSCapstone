library(readxl)
library(cluster)
library(factoextra)

data_PK4 = read_excel("OneDrive - Quinnipiac University/24-25/Spring/DS480/Project/DSCapstone/RankingTables/PK4_ranking.xlsx")

df_PK4 = scale(data_PK4)

fviz_nbclust(df_PK4, kmeans, method = "wss", k.max = 3)

set.seed(123)
km_PK4 <- kmeans(data_PK4, 3, nstart = 25)

fviz_cluster(km_PK4,df_PK4)

print(km_PK4)

plot(data_PK4$scale_1_5,data_PK4$min_sum)