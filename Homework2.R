library(dplyr); library(tidyr); library(tibble)
library(readr); library(stringr); library(forcats); library(purrr)

#Problem 1

sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)

#Part A

sales_clean <- sales_raw |>
  mutate(item_clean = str_to_lower(str_squish(item_raw)),
         amount = parse_number(amount_raw),
         discount_rate = parse_number(discount_raw)/100,
         net_amount = amount * (1 - discount_rate),
         size = case_when(
           is.na(net_amount) ~ "unrecorded",
           net_amount >= 20 ~ "large",
           .default = "small"
         ))

sales_clean |>
  select(sale_id, item_clean, amount, discount_rate, net_amount, size)

#Part B

code_pattern <- "^(ST|ON)-[0-9]{3}$"

sales_codes <- sales_clean |>
  mutate(code_clean = str_to_upper(str_squish(code_raw)),
         valid_code = str_detect(code_clean, code_pattern)
        )

invalid_codes <- sales_codes |>
  filter(!valid_code) |>
  select(sale_id, code_raw, code_clean)
invalid_codes

valid_code_parts <- sales_codes |>
  filter(valid_code) |>
  separate_wider_delim(code_clean, delim = "-",
                         names = c("channel", "number"), cols_remove = FALSE)|>
  select(valid_code,code_clean, channel, number)
valid_code_parts

sale_tags <- sales_clean |>
  select (sale_id, tags) |>
  separate_longer_delim(tags, delim = ";")
sale_tags

#Separate_wider_delim() splits values into multiple columns, increasing the number 
#of columns, not rows, while separate_longer_delim() splits values into multiple rows
#within one column.

# Part C

customer_summary <- sales_clean |>
  summarise(sales_n = n(), known_n = sum(!is.na(net_amount)), 
            large_n = sum(net_amount >= 20, na.rm = TRUE), 
            large_share = if (known_n == 0) {
              NA_real_ }
              else {
              mean(net_amount >= 20,na.rm = TRUE)}, .by = customer_id) |>
  arrange(customer_id)
          
customer_summary

# For C1, the numerator is the large_n (known net amounts at least 20) 
# for that ID which is 0 and the denominator is known_n ( non-missing net amounts)
# which is 1. The calculate large share is 0/1, equal to 0.

sales_factors <- sales_clean |>
  mutate(item_factor = fct_infreq(factor(item_clean)),
         item_group = fct_lump_n(factor(item_clean), n = 2, other_level = "Other"))

levels(sales_factors$item_factor)
levels(sales_factors$item_group)

sales_factors |>
  count(item_factor, .drop = FALSE)
sales_factors |>
  count(item_group, .drop = FALSE)

# fct_infreq works to order factors by frequency, with the most common levels 
# first and does not change existing categories. fct_lump_n works by actually
# changing and combining less frequent categories, here the 2 most frequent
# categories are kept and the rest were collapsed into "Other".

# Problem 2

reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)
customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)

# Part A

reports_labeled <- reports_raw |>
  arrange(form, line) |>
  group_by(form) |>
  fill(customer_id, .direction = "down") |>
  ungroup()
reports_labeled 

reports_clean <- reports_labeled |>
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount = coalesce(primary_clean, backup_amount))

reports_clean |>
  select(form, line, customer_id, round,
         primary_amount, backup_amount, amount)
#coalesce() chooses the first non-missing value at each position, so the first
#choice is the non-missing amount from primary_clean, including 0 as it a recorded value.

reports_complete <- reports_clean |>
  select(customer_id, round, amount) |>
  mutate(recorded_row = TRUE) |>
  complete(customer_id, round = 1:3, fill = list(recorded_row = FALSE)) |>
  arrange(customer_id, round)
reports_complete

#There were 6 rows with 1 missing value originally and then 6 rows were added
#with a total of 7 missing values now.
#C3, round 2 was one existing missing amount (explicit) and C1, round 2 
#was one absent customer-round row (implicit).

#Part B

customers |>
  summarise(
    missing_ids = sum(is.na(customer_id)),
    duplicate_ids = sum(duplicated(customer_id[!is.na(customer_id)])))

customers |>
  filter(!is.na(customer_id)) |>
  count(customer_id) |>
  filter(n > 1)

reports_clean|>
  count(customer_id, round) |>
  filter(n > 1)


customers_marked <- customers |>
  mutate(customer_found = TRUE)

reports_joined <- reports_clean |>
  left_join(
    customers_marked,
    by = join_by(customer_id),
    relationship = "many-to-one",
    na_matches = "never") |>
  mutate(customer_found = coalesce(customer_found, FALSE))

reports_joined |>
  select(customer_id, round, amount, region, customer_found)


reports_only <- reports_clean |>
  distinct(customer_id) |>
  anti_join(
    customers |> distinct(customer_id),
    by = join_by(customer_id),
    na_matches = "never")

reports_only


customers_only <- customers |>
  distinct(customer_id) |>
  anti_join(
    reports_clean|> distinct(customer_id),
    by = join_by(customer_id),
    na_matches = "never")

customers_only

#Part C

reports_clean |>
  summarise(
    rows = n(),
    total_amount = sum(amount, na.rm = TRUE))

reports_duplicated <- reports_clean |>
  left_join(
    customers_bad,
    by = join_by(customer_id),
    relationship = "many-to-many" )

reports_duplicated |>
  summarise(
    rows = n(),
    total_amount = sum(amount, na.rm = TRUE))

reports_duplicated |>
  count(customer_id, round) |>
  filter(n > 1)

#Relationship = "many-to-one" would trigger an error 
#if a report's customer ID matched multiple rows in customers_bad. 
#This relationship requires each row in the left table to match 
#at most one row in the right table.
#Allowing "many-to-many" permits the join to proceed, 
#but it does not resolve conflicting customer information. 
#If a customer has multiple metadata records, their reports can be 
#duplicated, potentially inflating row counts and totals. 

reports_wide <- reports_complete |>
  select(customer_id, round, amount) |>
  pivot_wider(
    names_from = round,
    values_from = amount,
    names_prefix = "round_"
  ) %>%
  select(customer_id, round_1, round_2, round_3)

reports_wide

#Problem 3

two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  

  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  
  n_x <- length(x)
  n_y <- length(y)
  
  mean_x <- mean(x)
  mean_y <- mean(y)
  var_x <- var(x)
  var_y <- var(y)
  
  difference <- mean_x - mean_y
  
  if (equal_var) {
    

    df <- n_x + n_y - 2
    
    pooled_var <- ((n_x - 1) * var_x +
                     (n_y - 1) * var_y) / df
    
    standard_error <- sqrt(pooled_var * (1 / n_x + 1 / n_y))
    
  } else {
    

    standard_error <- sqrt(var_x / n_x + var_y / n_y)
    
    df <- (var_x / n_x + var_y / n_y)^2 /
      ((var_x / n_x)^2 / (n_x - 1) +
         (var_y / n_y)^2 / (n_y - 1))
  }
  

  t_stat <- difference / standard_error
  

  p_value <- 2 * pt(-abs(t_stat), df = df)

  t_critical <- qt(1 - alpha / 2, df = df)
  

  ci_lower <- difference - t_critical * standard_error
  ci_upper <- difference + t_critical * standard_error
  

  reject_h0 <- p_value < alpha
  

  return(list(
    mean_x = mean_x,
    mean_y = mean_y,
    difference = difference,
    n_x = n_x,
    n_y = n_y,
    standard_error = standard_error,
    t_statistic = t_stat,
    degrees_freedom = df,
    p_value = p_value,
    alpha = alpha,
    confidence_interval = c(ci_lower, ci_upper),
    reject_h0 = reject_h0
  ))
}


result_welch <- two_sample_t(x, y)
result_pooled <- two_sample_t(x, y, equal_var = TRUE)


ref_welch <- t.test(x, y)
ref_pooled <- t.test(x, y, var.equal = TRUE)


compare_results <- function(ours, reference) {
  differences <- c(
    t_stat = abs(ours$t_stat - unname(reference$statistic)),
    df = abs(ours$df - unname(reference$parameter)),
    p_value = abs(ours$p_value - reference$p.value)
  )
  
  list(
    absolute_differences = differences,
    all_below_1e_8 = all(differences < 1e-8)
  )
}


print(result_welch)
print(result_pooled)

print(compare_results(result_welch, ref_welch))
print(compare_results(result_pooled, ref_pooled))

x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)
# Reference calculations; compare with your own function.
t.test(x_check, y_check, var.equal = FALSE)
t.test(x_check, y_check, var.equal = TRUE)

B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05
scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null",
               "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)
# Run once immediately before the simulation, not inside a loop.
set.seed(2026)


B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05

scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null",
               "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)

# Set the seed once, immediately before the simulation
set.seed(2026)



simulation_results <- data.frame(
  scenario = rep(NA_character_, 4 * B * 2),
  replicate = integer(4 * B * 2),
  method = rep(NA_character_, 4 * B * 2),
  t_stat = numeric(4 * B * 2),
  df = numeric(4 * B * 2),
  p_value = numeric(4 * B * 2),
  reject = logical(4 * B * 2)
)

# Nested loops: scenarios, repetitions, and methods
row_index <- 1

for (i in seq_len(nrow(scenarios))) {
  
  for (b in seq_len(B)) {
    
    # Generate fresh, independent samples
    x <- rnorm(
      n_x,
      mean = scenarios$mean_x[i],
      sd = scenarios$sd_x[i]
    )
    
    y <- rnorm(
      n_y,
      mean = scenarios$mean_y[i],
      sd = scenarios$sd_y[i]
    )
    
    # Apply both methods to the same samples
    for (equal_var in c(FALSE, TRUE)) {
      
      result <- two_sample_t(
        x, y,
        equal_var = equal_var,
        alpha = alpha
      )
      
      simulation_results[row_index, ] <- list(
        scenarios$scenario[i],
        b,
        result$method,
        result$t_stat,
        result$df,
        result$p_value,
        result$reject
      )
      
      row_index <- row_index + 1
    }
  }
}

head(simulation_results)
nrow(simulation_results)

simulation_summary <- simulation_results %>%
  group_by(scenario, method) %>%
  summarise(
    mean_t = mean(t_stat),
    sd_t = sd(t_stat),
    rejection_rate = mean(reject),
    .groups = "drop"
  )


simulation_summary_display <- simulation_summary %>%
  mutate(
    across(
      c(mean_t, sd_t, rejection_rate),
      ~ round(.x, 3)
    )
  )

print(simulation_summary_display, n = 8)
