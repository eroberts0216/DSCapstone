library(readxl)
library(factoextra)

data_GenEd = read_excel("OneDrive - Quinnipiac University/24-25/Spring/DS480/Project/DSCapstone/RankingTables/GenEd_ranking.xlsx")

set.seed(123)
km_GenEd <- kmeans(data_GenEd, 5, nstart = 25)

fviz_cluster(km_GenEd,data_GenEd)

print(km_GenEd)

plot(data_GenEd$scale_1_5,data_GenEd$min_sum)