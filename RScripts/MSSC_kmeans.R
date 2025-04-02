library(readxl)
library(cluster)
library(factoextra)

data_MSSC = read_excel("OneDrive - Quinnipiac University/24-25/Spring/DS480/Project/DSCapstone/RankingTables/MSSC_ranking.xlsx")

df_MSSC = scale(data_MSSC)

fviz_nbclust(df_MSSC, kmeans, method = "wss", k.max = 5)

set.seed(123)
km_MSSC <- kmeans(data_MSSC, 4, nstart = 25)

fviz_cluster(km_MSSC,df_MSSC)

print(km_MSSC)

plot(data_MSSC$scale_1_5,data_MSSC$min_sum)