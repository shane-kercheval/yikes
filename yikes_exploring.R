recent_grads <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-16/recent-grads.csv")

library(moments)
categoric_summary <- function(data){
	t <- data %>%
		select_if(negate(is.numeric))
	
	num_na <- sapply(t, function(x) sum(is.na(x)))
	unique_values_table <- sapply(t, function(x) table(x))
	unique_values <- map_int(unique_values_table, ~ length(.))
	top_values <-  map_chr(unique_values_table, ~ names(sort(-.)[1]))
	
	data.frame(
		list(count = sapply(t, function(x) sum(!is.na(x))),
			 na = num_na,
			 perc_na =  round(num_na / nrow(t), 3),
			 unique_values = unique_values,
			 perc_unique = round(unique_values / nrow(t), 3),
			 top_value = top_values
		))
	
}
categoric_summary(recent_grads)

numeric_summary <- function(data, round_by=1){
	t <- data %>%
		select_if(is.numeric)
	num_na <- sapply(t, function(x) sum(is.na(x)))
	num_zero <- sapply(t, function(x) sum(x == 0, na.rm = TRUE))
	mean <- sapply(t, mean, na.rm=TRUE)
	st_dev <- sapply(t, sd, na.rm=TRUE)

	data.frame(
		list(count = sapply(t, function(x) sum(!is.na(x))),
			 na = num_na,
			 perc_na =  round(num_na / nrow(t), 3),
			 num_zero = num_zero,
			 perc_zero = round(sapply(t, function(x) sum(x == 0, na.rm = TRUE) / nrow(t)), 3),
			 mean = round(mean, round_by),
			 st_dev = round(st_dev, round_by),
			 coef_of_var = round(st_dev / mean, 3),
			 skewness = round(sapply(t, skewness, na.rm=TRUE), round_by),
			 kurtosis = round(sapply(t, kurtosis, na.rm=TRUE), round_by),
			 min = round(sapply(t, min, na.rm=TRUE), round_by),
			 percentile_25th = round(sapply(t, function(x) as.numeric(quantile(x, 0.25, na.rm=TRUE))), round_by),
			 median = round(sapply(t, median, na.rm=TRUE), round_by),
			 percentile_75th = round(sapply(t, function(x) as.numeric(quantile(x, 0.75, na.rm=TRUE))), round_by),
			 max = round(sapply(t, max, na.rm=TRUE), round_by)
		))
	
}

numeric_summary(recent_grads)
