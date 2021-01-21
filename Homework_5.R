# --- Reading the house.dat dataset ---
house_df = read.table("https://profs.info.uaic.ro/~cgatu/csia/res/house.dat", header=TRUE)

# --- Printing the data within the house.dat dataset ---
house_df

get_p_value = function(model, variable_name) {
	# f = summary(model)$fstatistic
    	# p_value = unname(pf(f[1], f[2], f[3], lower.tail=F))
   	p_value = summary(model)$coefficients[, 4][variable_name]
	return (p_value)
}

forward_selection = function(dataset) {
	# Setting the significance level to 5%
	alpha = 0.05
	# Initializing p-values array
	p_values = c()
	# Array which contains the indices of the variables that will represent the best (sub)model
	submodel_variables_indices = c()
	# Indices of the variables with a p-value smaller than alpha
	indices_p_value = c()
	# Indices of all variables from the entire model
	entire_model_variables_indices = c(2:ncol(dataset))	

	for (i in (2:ncol(dataset))) {
		y_hat = lm(as.formula(paste(colnames(dataset)[1], "~",
                            paste(colnames(dataset)[c(i)], collapse = "+"), 
                            sep = "")), data=dataset)
		variable_name = colnames(dataset[i])
   		p_value = get_p_value(y_hat, variable_name)
		if(p_value < alpha) {
			p_values = append(p_values, p_value)
			indices_p_value = append(indices_p_value, i)
		}
	}
	
	if(length(p_values) == 0) {
		return (submodel_variables_indices)
	} else {
		submodel_variables_indices = append(submodel_variables_indices, indices_p_value[which.min(p_values)])

		while(TRUE) {
			minimum_p_value = Inf
			variable_index = NULL
			variables_indeces_left = setdiff(entire_model_variables_indices, submodel_variables_indices)
			
			for (index in variables_indeces_left) {
				current_submodel_variables_indices = c(submodel_variables_indices, 
										   index)
				y_hat = lm(as.formula(paste(colnames(dataset)[1], "~",
                            			paste(colnames(dataset)[current_submodel_variables_indices],
											collapse = "+"), sep = "")), data=dataset)
				variable_name = colnames(dataset[index])
				p_value = get_p_value(y_hat, variable_name)

				if (p_value < alpha) {
					if (is.null(variable_index) || (p_value < minimum_p_value)) {
						minimum_p_value = p_value
						variable_index = index
					}	
				}
			}
						
			if (is.null(variable_index) || (length(variables_indeces_left) == 0)) {
				return (submodel_variables_indices)
				break
			}

			submodel_variables_indices = c(submodel_variables_indices, variable_index)
		}
		return (submodel_variables_indices)
	}
}

# --- The result of the forward selection algorithm ---
print(forward_selection(house_df))


backward_selection = function(dataset) {
	# Setting the significance level to 5%
	alpha = 0.05
	# Indices of all variables from the entire model
	entire_model_variables_indices = c(2:ncol(dataset))
	# Indices of the (sub)model
	submodel_indices = entire_model_variables_indices

	while(TRUE) {
		maximum_p_value = -Inf
		variable_index = NULL

		y_hat = lm(as.formula(paste(colnames(dataset)[1], "~",
                            			paste(colnames(dataset)[submodel_indices],
							collapse = "+"), sep = "")), data=dataset)

		for (index in submodel_indices) {
			variable_name = colnames(dataset[index])
			p_value = get_p_value(y_hat, variable_name)

			if (p_value > alpha) {
				if (is.null(variable_index) || (p_value > maximum_p_value)) {
					maximum_p_value = p_value
					variable_index = index
				}	
			}
		}
		if (is.null(variable_index) || (length(submodel_indices) == 0)) {
			return (submodel_indices)
			break
		}
		submodel_indices = setdiff(submodel_indices, c(variable_index))
	}	
}

# --- The result of the backwards selection algorithm ---
print(backward_selection(house_df))


stepwise_selection = function(dataset) {
	# Setting the significance level to 5%
	alpha = 0.05
	# Initializing p-values array
	p_values = c()
	# Array which contains the indices of the variables that will represent the best (sub)model
	submodel_variables_indices = c()
	# Indices of the variables with a p-value smaller than alpha
	indices_p_value = c()
	# Indices of all variables from the entire model
	entire_model_variables_indices = c(2:ncol(dataset))	

	for (i in (2:ncol(dataset))) {
		y_hat = lm(as.formula(paste(colnames(dataset)[1], "~",
                            paste(colnames(dataset)[c(i)], collapse = "+"), 
                            sep = "")), data=dataset)
   		p_value = get_p_value(y_hat)
		if(p_value < alpha) {
			p_values = append(p_values, p_value)
			indices_p_value = append(indices_p_value, i)
		}
	}
	
	if(length(p_values) == 0) {
		return (submodel_variables_indices)
	} else {
		submodel_variables_indices = append(submodel_variables_indices, indices_p_value[which.min(p_values)])

		while(TRUE) {
			minimum_p_value = Inf
			variable_index = NULL
			variables_indeces_left = setdiff(entire_model_variables_indices, 
								   submodel_variables_indices)
			p_values = c()
			indices_p_value = c()
			
			# Test each variable in the model given all other variables currently in the model
			y_hat = lm(as.formula(paste(colnames(dataset)[1], "~",
                       			paste(colnames(dataset)[submodel_variables_indices], collapse = "+"), 
                        		sep = "")), data=dataset)
   			
			for (i in submodel_variables_indices) {
				variable_name = colnames(dataset[i])
				p_value = get_p_value(y_hat, variable_name)
				if (p_value < alpha) {
					p_values = append(p_values, p_value)
					indices_p_value = append(indices_p_value, i)
				}
			}

			if (length(submodel_variables_indices) != length(p_values)) {
				# Drop the most non-significant variable
				submodel_variables_indices = setdiff(submodel_variables_indices, 
										 indices_p_value[which.max(p_values)])
				next
			} else {
				for (index in variables_indeces_left) {
					current_submodel_variables_indices = c(submodel_variables_indices, 
											   index)
					y_hat = lm(as.formula(paste(colnames(dataset)[1], "~",
                            				paste(colnames(dataset)[current_submodel_variables_indices],
								collapse = "+"), sep = "")), data=dataset)
					variable_name = colnames(dataset[index])
					p_value = get_p_value(y_hat, variable_name)

					if (p_value < alpha) {
						if (is.null(variable_index) || (p_value < minimum_p_value)) {
							minimum_p_value = p_value
							variable_index = index
						}	
					}
				}
						
				if (is.null(variable_index) || (length(variables_indeces_left) == 0)) {
					return (submodel_variables_indices)
					break
				}

				submodel_variables_indices = c(submodel_variables_indices, variable_index)
			}			
		}
		return (submodel_variables_indices)
	}
}


# --- The result of the stepwise selection algorithm ---
print(stepwise_selection(house_df))


# --- Plotting the results ---

Y = house_df$PRICE

best_submodel_forward_selection = forward_selection(house_df)
best_submodel_backward_selection = backward_selection(house_df)
best_submodel_stepwise_selection = stepwise_selection(house_df)
best_submodel_exhaustive_selection = c(3, 6, 7, 10, 11, 14)

forward_y_hat = predict(lm(Y ~., data=house_df[best_submodel_forward_selection]), 
					  house_df[best_submodel_forward_selection])
backward_y_hat = predict(lm(Y ~., data=house_df[best_submodel_backward_selection]), 
					   house_df[best_submodel_backward_selection])
stepwise_y_hat = predict(lm(Y ~., data=house_df[best_submodel_stepwise_selection]), 
					   house_df[best_submodel_stepwise_selection])
exhaustive_y_hat = predict(lm(Y ~., data=house_df[best_submodel_exhaustive_selection]), 
					   house_df[best_submodel_exhaustive_selection])

plot(0, type="b", main="Variable selection methods", xlab="y ", ylab="y_hat", pch=19, xlim=c(38, 90), ylim=c(38, 90))
abline(0,1, col='blue', lwd=2)

points(Y, forward_y_hat, col=rgb(1, 0, 0), bg=rgb(1, 0, 0), pch=23, lty=1)
points(Y, backward_y_hat, col=rgb(0, 1, 0), bg=rgb(0, 1, 0), pch=21, lty=2)
points(Y, stepwise_y_hat, col=rgb(0, 0, 1, 0.3), bg=rgb(0 , 0, 1, 0.3), pch=22, lty=3)
points(Y, exhaustive_y_hat, col=rgb(1, 0.5, 0.8, 0.4), bg=rgb(1, 0.5, 0.8, 0.4), pch=25, lty=4)
legend("topleft", 95, legend=c("Forward", "Backward", "Stepwise", "Exhaustive"),
       col=c(rgb(1, 0, 0), rgb(0, 1, 0), rgb(0, 0, 1, 0.3), rgb(1, 0.5, 0.8, 0.4)), lty=1:4, cex=0.8)

# Colors representation: red for forward, green for backward, light blue for stepwise and light lila for exhaustive
