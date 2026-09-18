# Problem 1

student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

# Part A

section <- factor(
  c("A", "B"),
  levels = c("A", "B"),
  ordered = TRUE)
section

students <- data.frame(
    student_id = student_id,
    section = section,
    quiz1 = quiz1,
    quiz2 = quiz2,
    passed = passed)

students

score_matrix <- as.matrix(students[, c("quiz1", "quiz2")])
rownames(score_matrix) <- c("S01", "S02", "S03", "S04", "S05", "S06")
colnames(score_matrix) <- c("quiz1", "quiz2")

score_matrix

course_record <- list(course = "R Programming",
                      scores = "students",
                      cutoffs = c(pass = 70, excellent = 90))

course_record


typeof(section)
class(section)
length(section)
str(section)

typeof(students)
class(students)
length(students)
str(students)
dim(students)

typeof(score_matrix)
class(score_matrix)
length(score_matrix)
str(score_matrix)
dim(score_matrix)

typeof(course_record)
class(course_record)
length(course_record)
str(course_record) 

#A vector is a one-dimensional collection of values and a list
#can contain different types of objects, including vectors, matrices, and other lists. 
#A matrix is a two-dimensional structure where all elements must have the same data type. 
#A factor is used to represent categorical data with defined levels, 
#while a data frame is a two-dimensional table whose columns can contain different data types.

# Part B

score_matrix["S04", "quiz2"]

score_matrix[1:2,,drop = F]


course_record["course"]

course_record[["course"]]

course_record$course

# Using a single bracket ([) returns a list and is used for subsets while
# using a double bracket ([[) returns the one element and
# using $ returns a named element

# Part C

students["average"] <- rowMeans(students[, c("quiz1", "quiz2")])

students["excellent"] <- students["average"] >= 90

students[students$section == "A" & students$average >= 80,]


student_averages <- students$average
names(student_averages) <- students$student_id
student_averages

# Calculations are vectorized as it this calclulates
# entire sets of data at once instead each item individually

#Problem 2

csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"

#Part A

measurements <- read.csv(
  text = csv_text,
  na.strings = c("", "NA")
)

head(measurements)
str(measurements)
dim(measurements)
names(measurements)

colSums(is.na(measurements))

measurements_complete <- complete.cases(measurements)
measurements_complete

removed_ids <- measurements$sample_id[
  !complete.cases(measurements)]
removed_ids

# x == NA is not a valid missing-value test as NA (missing value) means there is no 
# stored value and this test would keep producing another NA, not TRUE or FALSE.

# Part B

measurements$site <- factor(measurements$site)
measurements$status <- factor(measurements$status)

levels(measurements$site)
levels(measurements$status)

measurements["temp_f"] <- measurements$temp_c * 9 / 5 + 32


measurements ["ph_below_7"] <- ifelse(
  is.na(measurements$ph),
  NA,
  measurements$ph < 7)


measurements2 <- measurements[
  complete.cases(measurements) &
    measurements$site %in% c("North", "South") &
    measurements$status == "ok",
  c("sample_id", "site", "temp_c", "temp_f", "ph")]

measurements[, c("sample_id", "site", "temp_c", "temp_f","ph")]

overall_mean_c <- mean(measurements$temp_c, na.rm = TRUE)
overall_mean_c

south_mean_c <- mean(
  measurements$temp_c[measurements$site == "South"],
  na.rm = TRUE
)
south_mean_c

#Part C

A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

A * B

A %*% B

dim(A * B)
dim(A %*% B)

#  A * B multiplies corresponding elements of the two matrices,
# so the result has the same dimensions as A and B: 2 x 2.
# A %*% B performs matrix multiplication using row-by-column
# multiplication and addition. Because both matrices are 2 x 2,
# the resulting matrix is also 2 x 2.

#Problem 3

student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)

#Part A

grade_one <- function(
    score,
    a_min = 90,
    b_min = 80,
    c_min = 70,
    d_min = 60
) {
  if (is.na(score)) {
    return(NA_character_)
  } else if (score >= a_min) {
    return("A")
  } else if (score >= b_min) {
    return("B")
  } else if (score >= c_min) {
    return("C")
  } else if (score >= d_min) {
    return("D")
  } else {
    return("F")
  }
}

grade_one(NA)
grade_one(90)
grade_one(80)
grade_one(85)
grade_one(74)

#Part B

grades <- rep(NA_character_, length(scores))
for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}
names(grades) <- student_id

grades

#i is the current position in the scores vector during each loop.


#Part C-1

summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  
  total_count <- length(x)
missing_count <- sum(is.na(x))

mean_value <- mean(x, na.rm = na.rm)
sd_value <- sd(x, na.rm = na.rm)
min_value <- min(x, na.rm = na.rm)
max_value <- max(x, na.rm = na.rm)

c(
  total_count = total_count,
  missing_count = missing_count,
  mean = round(mean_value, digits),
  sd = round(sd_value, digits),
  minimum = round(min_value, digits),
  maximum = round(max_value, digits)
)
}

summarize_scores(scores)

summarize_scores(
  x = scores,
  na.rm = TRUE,
  digits = 2)

#Part C-2

plot_scores <- function(x, ...) {
  plot(seq_along(x), x, ...)
}

plot_scores(scores, type = "b", pch = 19, xlab = "Position", ylab = "Score", main = "Student Scores" )

