library(e1071)
library(psych)
library(qcc)
library(readxl)
library(ggpubr)

new.function <- function(data, title) {
  
  boxplot(data, main=title,
          ylab="Hours",
          xlab="Training",
          col.main="red", col.lab="blue")
  
  sapply(data, mean)
  sapply(data, var)
  sapply(data, sd)
  
}

prelim_data <- read_excel("Data Table.xlsx")

data <- data.frame(Office_Tour = prelim_data$`Hours Spent...10`,
                   Site_Tour = prelim_data$`Hours Spent...13`,
                   General_Questions = prelim_data$`Hours Spent...16`,
                   Time_Recording = prelim_data$`Hours Spent...19`,
                   OOO_Procedures = prelim_data$`Hours Spent...22`,
                   Cmp_Resources = prelim_data$`Hours Spent...25`)

boxplot(data, main="General Onboarding",
ylab="Hours",
xlab="Training",
col.main="red", col.lab="blue")

gen_mean <- sapply(data, mean)
sapply(data, var)
sapply(data, sd)

data1 <- data.frame(Jira = prelim_data$`Hours Spent...29`,
                   Confluence = prelim_data$`Hours Spent...32`,
                   Sharepoint = prelim_data$`Hours Spent...35`,
                   Microsoft_Office = prelim_data$`Hours Spent...38`,
                   Microsoft_Project = prelim_data$`Hours Spent...41`,
                   Other = prelim_data$`Hours Spent...44`)

boxplot(data1, main="Department Specific Training",
        ylab="Hours",
        xlab="Training",
        col.main="red", col.lab="blue")

dept_mean <- sapply(data1, mean)
sapply(data1, var)
sapply(data1, sd)

data2 <- data.frame(ThreeD_Model = prelim_data$`Hours Spent...48`,
                    Circuit_Sim = prelim_data$`Hours Spent...51`,
                    Config_Manage = prelim_data$`Hours Spent...54`,
                    PDF_Reader = prelim_data$`Hours Spent...57`,
                    Success_Estimator = prelim_data$`Hours Spent...60`,
                    IDE_Tools = prelim_data$`Hours Spent...63`,
                    Software_CM = prelim_data$`Hours Spent...66`,
                    Other = prelim_data$`Hours Spent...69`)

boxplot(data2, main="Role Specific Training",
        ylab="Hours",
        xlab="Training",
        col.main="red", col.lab="blue")

role_mean <- sapply(data2, mean)
sapply(data2, var)
sapply(data2, sd)

gen_estimate = c(1.00, 3.00, 2.00,
                 0.50, 0.50, 2.00)

gen_cor <- cor(gen_mean, gen_estimate, method = "pearson", use = "complete.obs")
cor.test(data1$Jira, y, method = "pearson")

# Creating the plot
plot(gen_mean, gen_estimate, pch = 19, col = "lightblue")

# Regression line
abline(lm(gen_estimate ~ gen_mean), col = "red", lwd = 3)

# Pearson correlation
text(paste("Correlation:", round(gen_cor, 0.2)), x = 25, y = 95)
