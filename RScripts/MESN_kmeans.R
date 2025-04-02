library(readxl)
library(cluster)
library(factoextra)

data_MESN = read_excel("OneDrive - Quinnipiac University/24-25/Spring/DS480/Project/DSCapstone/RankingTables/MESN_ranking.xlsx")

df_MESN = scale(data_MESN)

fviz_nbclust(df_MESN, kmeans, method = "wss", k.max = 5)

set.seed(123)
km_MESN <- kmeans(data_MESN, 4, nstart = 25)

fviz_cluster(km_MESN,df_MESN)

print(km_MESN)

plot(data_MESN$scale_1_5,data_MESN$min_sum)