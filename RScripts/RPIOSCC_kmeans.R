library(readxl)
library(cluster)
library(factoextra)

data_RPIOSCC = read_excel("OneDrive - Quinnipiac University/24-25/Spring/DS480/Project/DSCapstone/RankingTables/RPIOSCC_ranking.xlsx")

df_RPIOSCC = scale(data_RPIOSCC)

fviz_nbclust(df_RPIOSCC, kmeans, method = "wss", k.max = 5)

set.seed(123)
km_RPIOSCC <- kmeans(data_RPIOSCC, 4, nstart = 25)

fviz_cluster(km_RPIOSCC,df_RPIOSCC)

print(km_RPIOSCC)

plot(data_RPIOSCC$scale_1_5,data_RPIOSCC$min_sum)