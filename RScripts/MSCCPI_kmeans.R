library(readxl)
library(cluster)
library(factoextra)

data_MSCCPI = read_excel("OneDrive - Quinnipiac University/24-25/Spring/DS480/Project/DSCapstone/RankingTables/MSCCPI_ranking.xlsx")

df_MSCCPI = scale(data_MSCCPI)

fviz_nbclust(df_MSCCPI, kmeans, method = "wss", k.max = 5)

set.seed(123)
km_MSCCPI <- kmeans(data_MSCCPI, 3, nstart = 25)

fviz_cluster(km_MSCCPI,df_MSCCPI)

print(km_MSCCPI)

plot(data_MSCCPI$scale_1_5,data_MSCCPI$min_sum)