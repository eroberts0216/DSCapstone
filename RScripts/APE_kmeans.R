library(readxl)
library(factoextra)

data_APE = read_excel("OneDrive - Quinnipiac University/24-25/Spring/DS480/Project/DSCapstone/RankingTables/APE_ranking.xlsx")

set.seed(123)
km_APE <- kmeans(data_APE, 5, nstart = 25)

fviz_cluster(km_APE,data_APE)

print(km_APE)

plot(data_APE$scale_1_5,data_APE$min_sum)