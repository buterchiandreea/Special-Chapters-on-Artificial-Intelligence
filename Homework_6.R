# --- Packages installation --- 
install.packages('factoextra')
install.packages('FactoMineR')
library(factoextra)
library(FactoMineR)

# --- Reading the swiss dataset ---
swiss_df = read.table("https://profs.info.uaic.ro/~cgatu/csia/res/swiss.txt, header = TRUE)

# --- Getting variable names ---
colnames(swiss_df)

# --- Printing the contents of the dataset ---
swiss_df

# --- (1) Simple statistics ---
summary(swiss_df)

# --- (2) Correlations for the initial dataset ---
cor(swiss_df)

# --- (3) Covariances for the initial dataset ---
cov(swiss_df)

# --- Computing the PCA ---
PCA = prcomp(swiss_df, scale = TRUE)

# --- (4) Importance of the components ---
summary(PCA)

# --- (5) Getting the eigenvectors ---
eigen(cov(swiss_df))$vectors

# --- (6) Getting the eigenvalues ---
get_eigenvalue(PCA)

# --- (7) Displaying the scree plot ---
fviz_screeplot(PCA, addlabels = TRUE, ylim = c(0, 50))

# --- (8) Scores ---
get_pca_ind(PCA)$contrib

# --- (9) Loadings ---
PCA$rotation

# --- (10) Graph of individuals ---
fviz_pca_ind(
			PCA, 
			col.ind = "cos2", 
			repel = TRUE)   

# --- (11) Correlation circle --- 
fviz_pca_var(PCA, col.var = "black")