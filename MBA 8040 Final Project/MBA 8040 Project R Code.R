# Read the Data into R:
library(readxl)
setwd("C:/Users/tzhan/Google Drive/GSU Graduate School/MBA 8040 Data Driven Decision Making/Project")
my_data <- read_excel("Freight.xlsx")

library(plyr)
my_data <- rename(my_data, c("Invoiced Amount" = "Invoiced_Amount", "Quoted Price" = "Quoted_Price", "Total weight"="Total_Weight", "Price per pound" = "Price_per_Pound", 
                             "Freight class 1" = "Freight_Class", "Height 1" = "Height", "Product Cost" = "Product_Cost"))
my_data$Carrier <- NULL
str(my_data)

attach(my_data)

# Specify Categorical Variables, convert to Factors:
Freight_Class <- factor(Freight_Class)
summary(Freight_Class)
# Optional Data Exploration:
plot(Freight_Class, main="Freight Classes", xlab="Freight Class", ylab="Count", col="red")

# Make "300" the reference level in the Freight_Class categorical variable:
Freight_Class <- relevel(Freight_Class, "300")

# Multiple Regression, Initial Model:
fit.1 <- lm(Invoiced_Amount ~ Quoted_Price + Total_Weight + Price_per_Pound + Volume + Density + Freight_Class 
            + Height + Product_Cost)
summary(fit.1)

# Remove Freight_Class factor level 60, p-value of 0.434
Freight_Class <- factor(Freight_Class, exclude = "60")
fit.2 <- lm(Invoiced_Amount ~ Quoted_Price + Total_Weight + Price_per_Pound + Volume + Density + Freight_Class 
            + Height + Product_Cost)
summary(fit.2)

# Remove Product_Cost, p-value of 0.520
fit.3 <- lm(Invoiced_Amount ~ Quoted_Price + Total_Weight + Price_per_Pound + Volume + Density + Freight_Class 
            + Height)
summary(fit.3)

# Remove Height, p-value of 0.185
fit.4 <- lm(Invoiced_Amount ~ Quoted_Price + Total_Weight + Price_per_Pound + Volume + Density + Freight_Class)
summary(fit.4)

# Remove Density, p-value of 0.175
fit.5 <- lm(Invoiced_Amount ~ Quoted_Price + Total_Weight + Price_per_Pound + Volume + Freight_Class)
summary(fit.5)

# Example Prediction
77.469597 + 1.098254*mean(Quoted_Price) + 0.018728*mean(Total_Weight) - 50.094*mean(Price_per_Pound) - 0.212646*mean(Volume)
        