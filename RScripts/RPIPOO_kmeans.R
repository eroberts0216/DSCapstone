library(readxl)
library(cluster)
library(factoextra)

data_RPIPOO = read_excel("OneDrive - Quinnipiac University/24-25/Spring/DS480/Project/DSCapstone/RankingTables/RPIPOO_ranking.xlsx")

df_RPIPOO = scale(data_RPIPOO)

fviz_nbclust(df_RPIPOO, kmeans, method = "wss", k.max = 5)

set.seed(123)
km_RPIPOO <- kmeans(data_RPIPOO, 3, nstart = 25)

fviz_cluster(km_RPIPOO,df_RPIPOO)

print(km_RPIPOO)

#plot(data_RPIPOO$scale_1_5,data_RPIPOO$min_sum)