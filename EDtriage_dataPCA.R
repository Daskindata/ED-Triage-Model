install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")

triageData <- read.csv("G:/David/JCU/MA5854/ED_Patients_Data.csv", header = T, sep = ",")

triageData[] <- lapply(triageData, as.numeric)
#convert NA to 0
triageData[is.na(triageData)] = 0
str(triageData)

#remove the dependent and identifier variables
ED_triage <- triageData[-c(1,5,69,71,73,75,77)]
head(ED_triage[, 1:70], 10)

library("FactoMineR")
res.pca <- PCA(ED_triage, graph = FALSE)
print(res.pca)

library("factoextra")
eig.val <- get_eigenvalue(res.pca)
eig.val

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(res.pca)
var

# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

# Coordinates of variables
head(var$coord, 70)

fviz_pca_var(res.pca, col.var = "black")

head(var$cos2, 70)

library("corrplot")
corrplot(var$cos2, is.corr = FALSE)

# Total cos2 of variables on Dim.1 to Dim.5
fviz_cos2(res.pca, choice = "var", axes = 1:5)

#A high cos2 indicates a good representation of the variable on the principal component. 
#In this case the variable is positioned close to the circumference of the correlation circle.

#A low cos2 indicates that the variable is not perfectly represented by the PCs.\
#In this case the variable is close to the center of the circle.

# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
             )

var_cont <- head(var$contrib, 70)
install.packages("openxlsx")
library(openxlsx)
write.xlsx(var_cont,"G:/David/JCU/MA5854/var_cont.xlsx")
#The larger the value of the contribution, the more the variable contributes to the component. 