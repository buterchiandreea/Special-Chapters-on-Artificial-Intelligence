# --- Ex. 1 ---

compute_x_power_2 = function (x) {
	x ** 2
}

display_graph_using_plot = function (a, b) {
	plot( 
		compute_x_power_2(seq(a, b, by = 0.01)),
		type = "l",
		col = "red",
		lwd = 2,
		main = sprintf("f(x)=xˆ2, x in [%.2f, %.2f]", a, b)
	)
}

display_graph_using_curve = function(a, b) {
    	curve(
        	x ** 2, 
        	from = a, 
       	to = b, 
        	type = "l",
        	col = "red",
        	lwd = 2,
        	main = sprintf("f(x)=xˆ2, x in [%d, %d]", a, b)
    	)
}

display_graph_using_plot(1.01, 1.05)
display_graph_using_curve(1.01, 1.05)


# --- Ex. 2 ---

display_binomial_distributions = function(n) {
	x = seq(0, n, by = 1)
	p = seq(0.1, 0.9, by = 0.1)
	for (i in 1:length(p)) {
		y = dbinom(x, n, p[i])
		pdf(file = sprintf("bd_%d.pdf", i))
		plot(
			x, 
			y, 
			main = sprintf("B(x, 20, p), where p is %.1f", p[i]), 
			type = "l"
		)
		points(x, y, pch = 4)
		dev.off()
	} 
}

display_binomial_distributions(20)


# --- Ex. 3 ---

display_normal_distributions = function() {
	x = seq(-5, 5, by = 1)
	mean = 0
	sd_1 = 0.5
	sd_2 = 1
	sd_3 = 3
	y_1 = dnorm(x, mean, sd_1)
	y_2 = dnorm(x, mean, sd_2)
	y_3 = dnorm(x, mean, sd_3)
	
	pdf(file = "nd.pdf")

	plot(
		x, 
		y_1,  
		type = "l",
		lwd=2, 
		lty = 1, 
		col = "purple" 
	)
	lines(
		x, 
		y_2,
		col="orange",
		lwd=2, 
		lty = 2
	)
	lines(
		x, 
		y_3,
		col="green",
		lwd=2, 
		lty = 3
	)
	legend(
		"topleft", 
		legend = c("N(0, 0.5)", "N(0, 1)", "N(0, 2)"),
       	col = c("purple", "orange", "green"), 
		lty=1:3,
	      cex=0.8
	)	

	dev.off()
}

display_normal_distributions()


# --- Ex. 4 ---

CLT_uniform = function(n) {
	means = c()
	for (i in 1:1000) {
		v = runif(n, 0, 20)
		means = append(means, mean(v))
	}
	means
}

display_histograms_uniform = function() {
	n_values = c(1, 5, 10, 100)
	for (i in 1:length(n_values)) {
		means = CLT_uniform(n_values[i]) 
		pdf(file = sprintf("hist_unif_%d.pdf", n_values[i]))
		hist(
			means,
			main = sprintf("Histogram of means, where n = %d", n_values[i]), 
			col = "purple", 
			breaks = 50 
		)		
		dev.off()
	} 
}

display_histograms()

CLT_binomial = function(n) {
	means = c()
	for (i in 1:1000) {
		v = rbinom(n, 20, 0.1)
		means = append(means, mean(v))
	}
	means
}

display_histograms_binomial = function() {
	n_values = c(1, 5, 10, 100)
	for (i in 1:length(n_values)) {
		means = CLT_binomial(n_values[i]) 
		pdf(file = sprintf("hist_binom_%d.pdf", n_values[i]))
		hist(
			means,
			main = sprintf("Histogram of means, where n = %d", n_values[i]), 
			col = "orange" 
		)		
		dev.off()
	} 
}

display_histograms_binomial()

