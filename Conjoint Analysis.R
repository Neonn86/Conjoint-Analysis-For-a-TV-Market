library(readxl)
library(dplyr)
library(ggplot2)

setwd("~/Documents/UC Davis/BAX - 442/Class2/Conjoint")

asha <- c(21,20,19,24,23,22,9,8,7,12,11,10,15,14,13,18,17,16,3,2,1,6,5,4)
kavish <- c(11,10,12,23,24,22,7,8,9,19,21,20,5,4,6,17,18,16,2,3,1,14,15,13)
keshore <- c(17,11,16,22,20,24,14,10,13,23,21,18,3,5,12,1,2,19,7,9,4,15,8,6)
neon <- c(14,15,12,23,21,19,18,17,16,24,22,20,4,5,1,11,6,8,7,2,3,13,9,10) 
vinit <- c(15,13,7,23,24,14,10,12,11,21,22,9,6,16,8,19,20,4,2,3,1,17,18,5)

hw1 = function(pref_vector) {
  
  design_matrix <- read_excel("Design Matrix.xlsx")
  names(design_matrix) <- c("Screen_75", "Screen_85", "Resolution", "Brand", "Price", "Profile_Num", "Profile")
  
  preference <- pref_vector
  
  cost <- data.frame(intercept = 1000, screen_75_inch = 500, screen_85_inch = 1000, resolution = 250, brand = 250)
  own_brand <- data.frame(intercept = 1, screen_75_inch = 0, screen_85_inch = 1, resolution = 0, brand = 0, price = 1500)
  sony_brand <- data.frame(intercept = 1, screen_75_inch = 1, screen_85_inch = 0, resolution = 1, brand = 1, price = 2500)
  sharp_brand <- data.frame(intercept = 1, screen_75_inch = 0, screen_85_inch = 1, resolution = 1, brand = 0, price = 2000)
  market_size <- 100
  net_cost <- sum(subset(own_brand, select = -c(price))*cost)
  
  screen_75 <- as.matrix(design_matrix["Screen_75"])
  screen_85 <- as.matrix(design_matrix["Screen_85"])
  resolution <- as.matrix(design_matrix["Resolution"])
  brand <- as.matrix(design_matrix["Brand"])
  price <- as.matrix(design_matrix["Price"])
  
  lm_model = lm(preference ~ screen_75 + screen_85 + resolution + brand + price)
  
  partworths <- data.frame(t(summary(lm_model)$coefficients[,1]))
  colnames(partworths) <- c("Intercept", "Screen_75", "Screen_85", "Resolution", "Brand", "Price")
  
  print("1. Partworths:")
  print(partworths)
  
  attributes <- data.frame(Attributes = c("Screen Size", "Screen Resolution", "Brand Name", "Price"),
                           Range = c(pmax(0, partworths["Screen_75"], partworths["Screen_85"]) - pmin(0, partworths["Screen_75"], partworths["Screen_85"]),
                                     pmax(0, partworths["Resolution"]) - pmin(0, partworths["Resolution"]),
                                     pmax(0, partworths["Brand"]) - pmin(0, partworths["Brand"]),
                                     pmax(0, partworths["Price"]) - pmin(0, partworths["Price"])))
  attributes["Importance"] <- round(attributes["Range"] * 100 / sum(attributes["Range"]),0)
  attributes["Importance"] <- lapply(attributes["Importance"], paste0, '%')
  
  print("2. Attribute Importance:") 
  print(attributes["Importance"])
  
  cost_saving <- pmax(sony_brand$price, sharp_brand$price) - pmin(sony_brand$price, sharp_brand$price)
  price_per_util <- cost_saving/abs(partworths["Price"])
  wtp <- subset(partworths, select = -c(Price)) * c(price_per_util)
  
  print("3. Willingness to Pay:")
  print(wtp)
  
  utility_func <- function(ob_price, brand=own_brand) {
    return(sum(subset(brand, select = -c(price)) * subset(partworths, select = -c(Price))) 
              + (partworths["Price"][1,] * 
                (ob_price - pmin(sony_brand$price, sharp_brand$price)) /
                (pmax(sony_brand$price, sharp_brand$price) - pmin(sony_brand$price, sharp_brand$price))))
  }
  
  utility <- rbind(utility_func(sony_brand$price, sony_brand), utility_func(sharp_brand$price, sharp_brand))
  attractiveness <- exp(utility)
  
  final_df <- data.frame(Price = seq(1500, 2600, 100))
  final_df["Attractiveness"] <- exp(unlist(lapply(final_df$Price, utility_func)))
  total_attractiveness <- final_df["Attractiveness"] + attractiveness[1,] + attractiveness[2,]
  final_df["Market_Share"] <- round(final_df["Attractiveness"] / total_attractiveness, 6)
  final_df["Sales"] <- final_df["Market_Share"] * market_size
  final_df["Margin"] <- final_df["Price"]  - net_cost                
  final_df["Profit"] <- final_df["Margin"] * final_df["Sales"]
  
  max_profit <- max(final_df$Profit)
  
  optimal_price <- final_df[final_df$Profit == max_profit, "Price"]
  print(paste("4. Optimal Price:", optimal_price))
  
  print(paste("5. Maximum Profit:", max_profit))
  
  q6_plot <- ggplot() +
    geom_line(data = final_df, mapping = aes(x = Price , y = Market_Share), color = 'blue') +
    geom_point(data = final_df, mapping = aes(x = Price , y = Market_Share), color = 'blue') + 
    labs(title = "6. Market Share vs Price")
  
  q7_plot <- ggplot() +
    geom_line(data = final_df, mapping = aes(x = Price , y = Profit), color = 'red') +
    geom_point(data = final_df, mapping = aes(x = Price , y = Profit), color = 'red') +
    labs(title = "7. Profit (Margin x Sales) vs Price")
  
  list(q6_plot, q7_plot)
  
}

hw1(asha)
hw1(kavish)
hw1(keshore)
hw1(neon)
hw1(vinit)

hw1