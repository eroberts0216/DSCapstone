library(readxl)
library(cluster)
library(factoextra)

data_VI = read_excel("OneDrive - Quinnipiac University/24-25/Spring/DS480/Project/DSCapstone/RankingTables/VI_ranking.xlsx")

df_VI = scale(data_VI)


set.seed(123)
km_VI <- kmeans(data_VI, 1, nstart = 25)

fviz_cluster(km_VI,df_VI)

print(km_VI)

plot(data_VI$scale_1_5,data_VI$min_sum)