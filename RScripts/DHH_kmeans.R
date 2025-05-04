library(readxl)
library(cluster)
library(factoextra)

data_DHH = read_excel("OneDrive - Quinnipiac University/24-25/Spring/DS480/Project/DSCapstone/RankingTables/DHH_ranking.xlsx")

df_DHH = scale(data_DHH)

fviz_nbclust(df_DHH, kmeans, method = "wss", k.max = 2)

set.seed(123)
km_DHH <- kmeans(data_DHH, 2, nstart = 25)

fviz_cluster(km_DHH,df_DHH)

print(km_DHH)

#plot(data_DHH$scale_1_5,data_DHH$min_sum)