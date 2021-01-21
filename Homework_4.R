# --- Installing packages ---
install.packages("magrittr")
install.packages("dplyr")
install.packages("lmSubsets")
library(magrittr)
library(dplyr) 
library(lmSubsets) 


# --- Reading the house.dat dataset ---
house_df = read.table("https://profs.info.uaic.ro/~cgatu/csia/res/house.dat", header=TRUE)

# --- Printing the data within the house.dat dataset ---
house_df

# --- Helper functions ---

compute_beta_hat = function(X, Y) {
	return (solve(t(X) %*% X) %*% t(X) %*% Y)
}

compute_y_hat = function(X, Y) {
	return (X %*% compute_beta_hat(X, Y))
}

compute_RSS = function(Y, Y_hat) {
	return (sum ((Y - Y_hat) ** 2))
}

compute_TSS = function(Y) {
	return (sum ((Y - mean(Y)) ** 2))
}

compute_R_2 = function(Y, Y_hat) {
	return (1 - (compute_RSS(Y, Y_hat) / compute_TSS(Y)))
}

compute_adjusted_R_2 = function(Y, Y_hat, p) {
	m = length(Y)
	return (1 - (1 - compute_R_2(Y, Y_hat) * ((m - 1) / (m - p - 1))))
}

compute_Mallows_C = function(Y, Y_hat_p, Y_hat, p) {
	return ((compute_RSS(Y, Y_hat_p) / compute_RSS(Y, Y_hat)) * (length(Y) - 13 - 1) - (length(Y) - 2 * (p + 1)))
}

compute_level_of_bias = function(Y, Y_hat_p, Y_hat, p) {
	mallows_cp_coeff = compute_Mallows_C(Y, Y_hat_p, Y_hat, p)
	return (abs(mallows_cp_coeff - p - 1))
}

# --- Functions IDs ---
# RSS ~ 1
# R^2 ~ 2
# R^2_a ~ 3
# C_p ~ 4


compute_regression_model = function (dataset, function_ID) {
	to_return_matrix = matrix(nrow = 13, ncol = 13)
	X = house_df[, names(house_df) != "PRICE"]
  	Y = house_df$PRICE
	Y_hat = compute_y_hat(as.matrix(X), Y)
	optimum_scores = c()
	
	for (p in seq(1, 13)) {
		combinations_of_p = combn(2:length(dataset), p)
		score_vec = c()
		
		for (index in 1:dim(combinations_of_p)[2]) {
			selected_indeces = combinations_of_p[, index]
			current_X = house_df[selected_indeces]
			current_Y_hat = compute_y_hat(as.matrix(current_X), Y)
			if (function_ID == 1) {
				score_vec = append(score_vec, compute_RSS(Y, current_Y_hat))
			} else if (function_ID == 2) {
				score_vec = append(score_vec, compute_R_2(Y, current_Y_hat))
			} else if (function_ID == 3) {
				score_vec = append(score_vec, compute_adjusted_R_2(Y, current_Y_hat, p))
			} else {
				compute_level_of_bias(Y, current_Y_hat, Y_hat, p)
				score_vec = append(score_vec, compute_level_of_bias(Y, current_Y_hat, p))
			}
		}
		
		if (function_ID == 1) {
			optimum_score_subset_index = which.min(score_vec)
     			optimum_scores = append(optimum_scores, min(score_vec))
		} else if (function_ID == 2) {
			optimum_score_subset_index = which.max(score_vec)
     			optimum_scores = append(optimum_scores, max(score_vec))
		} else if (function_ID == 3) {
			optimum_score_subset_index = which.max(score_vec)
     			optimum_scores = append(optimum_scores, max(score_vec))
		} else {
			optimum_score_subset_index = which.min(score_vec)
     			optimum_scores = append(optimum_scores, min(score_vec))
		}

		features_combination = combinations_of_p[, optimum_score_subset_index]
   		
		for(i in seq(1, p)){
      		to_return_matrix[p, i] = features_combination[i]
    		}
  	}
  	return(c(optimum_scores, to_return_matrix))
}


compute_regression_model_fnc = function (function_ID) {
	to_return_matrix = matrix(nrow = 13, ncol = 13)
	X = house_df[, names(house_df) != "PRICE"]
  	Y = house_df$PRICE
	Y_hat = lm(as.formula(paste(colnames(house_df)[1], "~",
                            paste(colnames(house_df)[c(2:14)], collapse = "+"), 
                            sep = "")),data=house_df)


	optimum_scores = c()
	
	for (p in seq(1, 13)) {
		combinations_of_p = combn(2:length(house_df), p)
		score_vec = c()
		
		for (index in 1:dim(combinations_of_p)[2]) {
			selected_indeces = combinations_of_p[, index]
			current_Y_hat = lm(as.formula(paste(colnames(house_df)[1], "~",
      							paste(colnames(house_df)[selected_indeces], collapse = "+"), 
      							sep = "")), data=house_df)

			table_of_values = summary(current_Y_hat)
			if (function_ID == 1) {
				score_vec = append(score_vec, sum(current_Y_hat$residuals ** 2))
			} else if (function_ID == 2) {
				score_vec = append(score_vec, table_of_values$r.squared)
			} else if (function_ID == 3) {
				score_vec = append(score_vec, table_of_values$adj.r.squared)
			} else {
				mallows_cp_coeff = ols_mallows_cp(current_Y_hat, Y_hat)
				score_vec = append(score_vec, mallows_cp_coeff)
			}
		}
		
		if (function_ID == 2 || function_ID == 3) {
			optimum_score_subset_index = which.min(score_vec)
     			optimum_scores = append(optimum_scores, max(score_vec))
		
		} else {
			optimum_score_subset_index = which.min(score_vec)
     			optimum_scores = append(optimum_scores, min(score_vec))
		}

		features_combination = combinations_of_p[, optimum_score_subset_index]
   		
		for(i in seq(1, p)){
      		to_return_matrix[p, i] = features_combination[i]
    		}
  	}
	print(optimum_scores)
	print(to_return_matrix)
  	return(c(optimum_scores, to_return_matrix))
}

par(mfrow=c(2,2))

result_rss = compute_regression_model_fnc(1)
result_rss = matrix(result_rss, 13, 14)
print(result_rss)
rss_scores = result_rss[,1]
plot(
	rss_scores, 
	xlab="Number of features", 
	ylab='RSS', 
	main='Residual Sum of Squares', 
	type='o', 
	col='red', 
	lwd=2.5
)

result_r2 = compute_regression_model_fnc(2)
result_r2 = matrix(result_r2, 13, 14)
print(result_r2)
r_squared_scores = result_r2[,1]
plot(
	r_squared_scores, 
	xlab="Number of features",
 	ylab='Value R ^ 2', 
	main='R Squared', 
	type='o', 
	col='green',
	lwd=2.5
)

result_r2_adj = compute_regression_model_fnc(3)
result_r2_adj = matrix(result_r2_adj, 13, 14)
print(result_r2_adj)
r_squared_adj_scores = result_r2_adj[,1]
plot(
	r_squared_adj_scores,
	xlab="Number of features", 
	ylab='Value R ^ 2_adj', 
	main='R Squared Adjusted', 
	type='o', 
	col='purple', 
	lwd=2.5
)


result_bias = compute_regression_model_fnc(4)
result_bias = matrix(result_bias, 13, 14)
print("Result bias")
print(result_bias)
bias_scores = result_bias[,1]
print("Bias scores")
print(bias_scores)
plot(
	bias_scores, 
	xlab="Number of features", 
	ylab='Value |Cp - p|', 
	main='Absolute value of Mallows Cp - p', 
	type='o', 
	col='orange', 
	lwd=2.5
)
abline(0,0)

print(paste("Best submodel is = "))
print(result_rss[which.max(result_r2_adj[,1]),])

