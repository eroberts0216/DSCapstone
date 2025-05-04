library(readxl)
library(cluster)
library(factoextra)

data_GenEd = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/DSCapstone/RankingTables/GenEd_ranking.xlsx")

df_GenEd = scale(data_GenEd)

fviz_nbclust(df_GenEd, kmeans, method = "wss", k.max = 5)

set.seed(123)
km_GenEd <- kmeans(data_GenEd, 4, nstart = 25)

fviz_cluster(km_GenEd,df_GenEd, main = "General Education k-Means", xlab = "Scale 1 - 5", ylab = "Sum of Minutes")

print(km_GenEd)

#plot(data_GenEd$scale_1_5,data_GenEd$min_sum)