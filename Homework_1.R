# --- Ex. 2 ---

# --- Data initialization ---
x = c(1, 8, 2, 6, 3, 8, 8, 5, 5, 5)

# (a)
compute_mean = function(arr) {
	sum_of_elements = sum(arr)
	number_of_elements = length(arr)
	mean = sum_of_elements / number_of_elements	
}

print("--- Ex.2 (a) ---")
sprintf("The mean of the elements of x is: %f", compute_mean(x))

# (b)
compute_log_2 = function(arr) {
	log2_values = log2(arr)
}

print("--- Ex.2 (b) ---")
print("The elements of x are: ")
print(x)
print("The log2 values of the elements of x are: ")
print(compute_log_2(x))

# (c)
compute_diff_max_min = function(arr) {
	max_value = max(arr)
	min_value = min(arr)
	difference_max_min = max_value - min_value
	difference_max_min
}

print("--- Ex.2 (c) ---")
sprintf("The different between the maximum and the minimum values of x is: %d", compute_diff_max_min(x))

# (d)
normalize_x = function(arr) {
	y = c()
	for(i in 1:length(arr)) {
		y = append(y, (arr[i] - 5.1) / 2.514403)
	}
	y
}

print("--- Ex.2 (d) ---")
print("The normalized values of x are: ")
print(normalize_x(x))

# (e)
normalized_x = normalize_x(x)
mean_normalized_x = mean(normalized_x)
sd_normalized_x = sd(normalized_x)

sprintf("The mean is: %f", mean_normalized_x)
sprintf("The standard deviation is: %f", sd_normalized_x)

# --- Ex. 3 ---

# --- Data initialization ---
bills = c(46, 33, 39, 37, 46, 30, 48, 32, 49, 35, 30, 48)

# --- Total cost ---
total_cost = sum(bills)
sprintf("The total cost is: %d", total_cost)

# --- Minimum value paid for a bill ---
min_value = min(bills)
sprintf("The minimum value paid for a bill is: %d", min_value)

# --- Maximum value paid for a bill ---
max_value = max(bills)
sprintf("The maximum value paid for a bill is: %d", max_value)

# --- Number of months with the bill's value greater than 40 ---
no_of_bills_gt_40 = length(bills[bills > 40])
sprintf("The number of months when the bill's value exceeded 40: %d", no_of_bills_gt_40)

# --- The percentaje of months with the bill's value greater than 40 ---
percentage_bills_gt_40 = (number_of_bills_gt_40 / length(bills)) * 100
sprintf("The percentaje of months with the bill's value greater than 40 is: %f", percentage_bills_gt_40)

# --- Ex. 4 ---

# --- Data initialization ---
arr = scan()

# --- Maximum value ---
sprintf("The maximum value is: %f", max(arr))

# --- Minimum value ---
sprintf("The minimum value is: %f", min(arr))

# --- Mean ---
sprintf("The mean's value is: %f", mean(arr))

# --- Median ---
sprintf("The median's value is: %f", median(arr))

# --- Standard deviation ---
sprintf("The standard deviation's value is: ", sd(arr))

# --- Sorted array ---
print("The sorted array is: ")
print(sorted_array)

# --- The function for data standardization ---
standardize_data = function(arr) {
	standardized_arr = c()
	for(i in 1:length(arr)) {
		standardized_arr = append(standardized_arr, (arr[i] - mean(arr)) / sd(arr))
	}
	standardized_arr
}

standardized_arr = standardize_data(arr)
print("The standardized array is: ")
print(standardized_arr)

# --- The mean and the standard deviation ---
sprintf("The mean of the standardized array is: %f", mean(standardized_arr))
sprintf("The standard deviation of the standardized array is: %f", sd(standardized_arr))