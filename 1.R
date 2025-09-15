df <- read.csv("/Users/alirezabj/Desktop/HEC - Master/Statistical Modelling/bixi5_part1.csv")

#head(df)

# adding weekend
df$weekend <- ifelse(df$jj %in% c(6, 7), 1, 0)

#head(df)

# adding season
#Winter : 1
#Spring : 2
#Summer : 3
#Fall : 4
df$season <- 0
df$season[df$mm %in% c(1, 2, 3)] <- 1
df$season[df$mm %in% c(4, 5, 6)] <- 2
df$season[df$mm %in% c(7, 8, 9)] <- 3
df$season[df$mm %in% c(10, 11, 12)] <- 4

#head(df)

# adding territory
df$territory <- as.integer(factor(df$arrondissement))

head(df)


# outliers
cols_for_outliers <- c("precip", "dur", "temp")

for (col in cols_for_outliers) {
  df[[col]] <- as.numeric(df[[col]])
  Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  outliers <- df[[col]][df[[col]] < lower | df[[col]] > upper]
  cat("\nOutliers in", col, ":\n")
  dev.new()  
  boxplot(df[[col]], main = paste("Boxplot of", col), ylab = col)
}

# converting to csv
write.csv(df, "/Users/alirezabj/Desktop/HEC - Master/Statistical Modelling/bixi5_part1_clean.csv",
          row.names = FALSE)


