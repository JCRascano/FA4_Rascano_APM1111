#Initializing the values of data sets
Normal <- c(67, 69, 70, 62, 63, 67, 65, 59, 68, 66, 60, 65, 70, 63, 64, 65, 69, 60, 61, 67, 66, 64, 65, 68, 71, 61, 62, 69, 
            66, 65, 68, 62, 64, 67, 67, 70, 62, 64, 66, 63, 65, 68, 63, 64, 66, 65, 65, 61, 63, 66)

Skewed_Right <- c(31, 40, 43, 24, 30, 29, 30 ,24, 38, 27, 26, 35, 29, 33, 55, 75, 46, 38, 26, 34, 29, 85, 57, 29, 34, 40,
                  34, 41, 36, 35, 40, 26, 28, 34, 26, 19, 66, 23, 63, 28, 30, 26, 33, 31, 24, 25, 35, 22, 34, 28)
Skewed_Left <- c(102, 87, 55, 104, 70, 75, 95, 80, 73, 66, 79, 93, 60, 90, 73, 84, 89, 73, 85, 98, 72, 79, 92, 35, 76, 71, 
                 93, 90, 76, 71, 97, 63, 10, 58, 70, 82, 85, 72, 25, 93, 53, 44, 58, 65, 10, 77, 92, 81, 82, 77)

Uniform <- c(12.1, 12.1, 12.4, 12.1, 11.6, 11.6, 12.0, 11.6, 12.1, 12.2, 12.2, 12.2, 11.6, 11.7, 12.3, 11.7, 11.9, 11.7, 12.2, 11.7, 12.3, 11.8, 12.3, 
             12.5, 11.7, 11.8, 12.3, 11.8, 12.3, 11.8, 12.4, 11.9, 12.4, 11.9, 12.1, 11.9, 12.4, 12.2, 12.4, 11.9, 12.5, 12.0, 11.8, 11.9, 12.5, 12.0, 12.5, 12.0, 12.5, 12.0)

# Function to calculate the raw moments
Calculate_Moments <- function(data, order) {
  n <- length(data)
  moments <- sum(data^order)/n
  return(moments)
}

# Calculate moments for each set of data
Normal_Moments <- sapply(1:4, function(order) Calculate_Moments(Normal, order))
Skewed_Right_Moments <- sapply(1:4, function(order) Calculate_Moments(Skewed_Right, order))
Skewed_Left_Moments <- sapply(1:4, function(order) Calculate_Moments(Skewed_Left, order))
Uniform_Moments <- sapply(1:4, function(order) Calculate_Moments(Uniform, order))

# Display results
cat("Normal Moments:", Normal_Moments, "\n")
cat("Skewed-Right Moments:", Skewed_Right_Moments, "\n")
cat("Skewed-Left Moments:", Skewed_Left_Moments, "\n")
cat("Uniform Moments:", Uniform_Moments, "\n")

# FOR NUMBER 2
# Function to calculate moments about the mean
Calculate_Moments_About_Mean <- function(data, order) {
  mean_value <- mean(data)
  moments_about_mean <- sum((data - mean_value)^order) / length(data)
  return(moments_about_mean)
}

# Calculate moments about the mean for each set of data
Normal_Moments_About_Mean <- sapply(c(1, 2, 3, 4), function(order) Calculate_Moments_About_Mean(normal, order))
Skewed_Right_Moments_About_Mean <- sapply(c(1, 2, 3, 4), function(order) Calculate_Moments_About_Mean(skewed_right, order))
Skewed_Left_Moments_About_Mean <- sapply(c(1, 2, 3, 4), function(order) Calculate_Moments_About_Mean(skewed_left, order))
Uniform_Moments_About_Mean <- sapply(c(1, 2, 3, 4), function(order) Calculate_Moments_About_Mean(uniform, order))

# Display results
cat("Normal Moments about Mean:", Normal_Moments_About_Mean, "\n")
cat("Skewed-Right Moments about Mean:", Skewed_Right_Moments_About_Mean, "\n")
cat("Skewed-Left Moments about Mean:", Skewed_Left_Moments_About_Mean, "\n")
cat("Uniform Moments about Mean:", Uniform_Moments_About_Mean, "\n")

# FOR NUMBER 3
# Function to calculate moments about number 75
Calculate_Moments_About_75 <- function(data, order) {
  value_75 <- 75
  moments_about_75 <- sum((data - value_75)^order) / length(data)
  return(moments_about_75)
}

Normal_Moments_About_75 <- sapply(c(1, 2, 3, 4), function(order) Calculate_Moments_About_75(normal, order))
cat("Normal Moments about number 75:", Normal_Moments_About_75, "\n")

#FOR NUMBER 4 
#Using the results of Problems 5.2 and 5.3 we verify relations between the moments
m1 <- -9.88
m2 <- 105.92
m3 <- -1211.08
m4 <- 14572.64

calculate_m2 <- m2 - (m1)**2
cat("m2 = ", calculate_m2, "\n")

calculate_m3 <- m3 - 3*m1*m2 + 2*(m1)**3
cat("m3 = ", calculate_m3, "\n")

calculate_m4 <- m4 - 4*m1*m3 + 6*((m1)**2)*m2-3*(m1)**4
cat("m4 = ", calculate_m4, "\n")
