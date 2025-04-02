library(readxl)
library(cluster)
library(factoextra)

data_SLP = read_excel("OneDrive - Quinnipiac University/24-25/Spring/DS480/Project/DSCapstone/RankingTables/SLP_ranking.xlsx")

df_SLP = scale(data_SLP)

fviz_nbclust(df_SLP, kmeans, method = "wss", k.max = 5)

set.seed(123)
km_SLP <- kmeans(data_SLP, 3, nstart = 25)

fviz_cluster(km_SLP,df_SLP)

print(km_SLP)

plot(data_SLP$scale_1_5,data_SLP$min_sum)