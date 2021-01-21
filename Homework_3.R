# --- Ex. 1 ---

# --- Initial setup ---
install.packages("dplyr") 
library(dplyr) 


# --- Reading the alcohol.dat data set ---
alcohol_df = read.table("https://profs.info.uaic.ro/~cgatu/csia/res/alcool.dat", header=TRUE)
summary(alcohol_df) 

# (1) 
plot(
	alcohol_df%>%select(2:3),
	main="Dispersion diagram",
     	xlab="The average annual alcohol consumption in liters / person", 
     	ylab="The annual number of deaths / 100000 (heart disease)", 
	type='p',
	col='purple',
	pch=19
)

# (2)
# --- Displaying the regression line ---
lm_result = lm(alcohol_df$Decese_datorate_afectiunilor_cardiace ~ alcohol_df$Alcool_din_vin)
abline(
	lm_result, 
	lwd=2, 
	col="orange"
)

# --- Computing the correlation coefficient ---
correlation_coefficient = cor(alcohol_df[2], alcohol_df[3])
sprintf("The correlation coefficient is: %f", correlation_coefficient)

sprintf("We can conclude, based on the value of the correlation coefficient of %f, 
	   that there is a negative linear relationship between variables.", correlation_coefficient)

# --- Ex. 2 ---

IQ_df = read.table("https://profs.info.uaic.ro/~cgatu/csia/res/iq.dat", header=TRUE)
summary(IQ_df) 

# (1)
plot(
	IQ_df%>%select(2:3),
	main="Dispersion diagram",
     	xlab="IQ value", 
     	ylab="Grade", 
	type='p',
	col='blue',
	pch=19
)

# (2)
# --- Displaying the regression line ---
lm_result = lm(IQ_df$Nota ~ IQ_df$IQ)
print(lm_result)
abline(
	lm_result, 
	lwd=2, 
	col="red"
)

# --- The variable coefficients contains the alpha(intercept) and beta(slope) values ---
coefficients = coef(lm_result)

# --- Estimating the grades for the IQ values 115 and 130 ---
input_data = data.frame(
				"Unit"=c(1, 1), 
				"IQ values"=c(115, 130)
		 )

y_hat = as.matrix(input_data) %*% as.matrix(coefficients)

y_hat_1 = y_hat[1]
sprintf("The estimated value for the grade of the student with the IQ of 115 is: %f", y_hat_1)

y_hat_2 = y_hat[2]
sprintf("The estimated value for the grade of the student with the IQ of 130 is: %f", y_hat_2)


# --- Ex. 3 ---

# --- We are generating m observations based on the formula y_i = a + b * x_i + e_i, where i in {1, ..., m}

generate_observations = function(m, a, b, xmin, xmax, sigma) {
	y = c()
	e = rnorm(m, 0, sigma ** 2)
	x = runif(m, xmin, xmax)
	for (i in 1:length(x)) {
		y_i = a + b * x[i] + e[i]
		y = append(y, y_i)
	}
	data.frame(x=x, y=y)
}


# --- Ex.4 ---

# --- Helper functions ---

sum_squared_errors = function(y, y_hat) {
	SSE = 0
	for (i in 1:length(y)) {
		SEE = SSE + (y[i] - y_hat[i]) ** 2
	}
	SEE
}

compute_variance_Y = function(n, y, y_hat) {
	SSE = sum_squared_errors(y, y_hat)
	df = n - 2
	S_2 = SSE / df
	S_2
}

compute_b_hat = function(x, y) {
	b_hat = (x - mean(x)) %*% as.matrix(y - mean(y)) / ((x - mean(x)) %*% as.matrix(x - mean(x)))
	b_hat
}

compute_a_hat = function(x, y, b_hat) {
	a_hat = mean(y) - b_hat * mean(x) 
	a_hat
}

compute_reg_coeffs_conf_int = function(m, a, b, xmin, xmax, sigma) {
	df = generate_observations(m, a, b, xmin, xmax, sigma)
	x = df$x
	y = df$y

	# --- Computing the values for a_hat and b_hat ---
	b_hat = compute_b_hat(x, y)
	a_hat = compute_a_hat(x, y, b_hat)
	
	# --- Computing the value of y_hat based on a_hat and b_hat values ---
	y_hat = a_hat + b_hat * x
	
	# --- Computing the variance of Y ---
	S_2 = compute_variance_Y(m, y, y_hat)
		
	# --- Computing the confidence interval ---
	t = abs(qt(0.025, m - 2))
	SE_a = t * sqrt(S_2 * ((1 / m + mean(x) ** 2) / sum(x ** 2)))
	SE_b = t * sqrt(S_2 / sum(x * x))

	return(c(a_hat, b_hat, SE_a, SE_b, df))
}


$ --- Ex. 5 ---

plot_real_and_predicted_values = function(x, y, model, a, b, i) {
	pdf(file = sprintf("regression%d.pdf", i))
 	
	# --- Plotting the real values ---
	plot(	
		y~x, 
		col="orange", 
		main=sprintf(
				"%f < a_hat < %f and %f < b_hat < %f", 
				(as.double(model[1]) - as.double(model[3])),
				(as.double(model[1]) + as.double(model[3])), 
				(as.double(model[2]) - as.double(model[4])), 
				(as.double(model[2]) + as.double(model[4]))
				)
	) 
	abline(a, b, col="purple", lw=6)

	# --- Plotting the estimated values ---
  	a_hat = model[1]
  	b_hat = model[2]
  	abline(a_hat, b_hat, col="green", lw=2)

  	dev.off()
}

plot_and_regression = function(m, a, b, xmin, xmax, sigma, i) {
  	model = compute_reg_coeffs_conf_int(m, a, b, xmin, xmax, sigma)
  	x = model$x
  	y = model$y
  	plot_real_and_predicted_values(x, y, model, a, b, i)
}

# --- Testing against the following values ----

plot_and_regression(100, 10, 0.8, -200, 200, 1.5, 1)
plot_and_regression(10, 10, 0.8, -5, 5, 1, 2)
plot_and_regression(10000, 10, 0.8, -5, 5, 1, 3)
plot_and_regression(10, 10, 0.8, 5, 5.2, 1, 4)
plot_and_regression(10000, 10, 0.8, 5, 5.2, 1, 5)
plot_and_regression(10, 10, 0.8, 5, 5.2, 0.01, 6)

	
