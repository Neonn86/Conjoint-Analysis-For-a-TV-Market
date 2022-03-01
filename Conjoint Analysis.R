library(readxl)
library(dplyr)
library(ggplot2)


customer <- c(15,13,7,23,24,14,10,12,11,21,22,9,6,16,8,19,20,4,2,3,1,17,18,5)

hw1 = function(pref_vector) {
  
  # 1.Product Attributes Information
  
  design_matrix <- read_excel("Design Matrix.xlsx")
  names(design_matrix) <- c("Screen_75", "Screen_85", "Resolution", "Brand", "Price", "Profile_Num", "Profile")
  
  # 2. Save the preference Score
  
  preference <- pref_vector
  
  # 3. Set cost, market_size, brand information
  
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
  
  # 4. Use Linear Regression Model to identify the relationship between Customer Preference and Product's Attributes
  
  lm_model = lm(preference ~ screen_75 + screen_85 + resolution + brand + price)
  
  # 5. Use coefficient as Partworths
  
  partworths <- data.frame(t(summary(lm_model)$coefficients[,1]))
  colnames(partworths) <- c("Intercept", "Screen_75", "Screen_85", "Resolution", "Brand", "Price")
  
  print("1. Partworths:")
  print(partworths)
  
  # 6. Calculate Attribute Importance to Customers
  
  attributes <- data.frame(Attributes = c("Screen Size", "Screen Resolution", "Brand Name", "Price"),
                           Range = c(pmax(0, partworths["Screen_75"], partworths["Screen_85"]) - pmin(0, partworths["Screen_75"], partworths["Screen_85"]),
                                     pmax(0, partworths["Resolution"]) - pmin(0, partworths["Resolution"]),
                                     pmax(0, partworths["Brand"]) - pmin(0, partworths["Brand"]),
                                     pmax(0, partworths["Price"]) - pmin(0, partworths["Price"])))
  attributes["Importance"] <- round(attributes["Range"] * 100 / sum(attributes["Range"]),0)
  attributes["Importance"] <- lapply(attributes["Importance"], paste0, '%')
  
  print("2. Attribute Importance:") 
  print(attributes["Importance"])
  
  # 7. Calculate how much customers want to spend on each attribute--- Willingness to Pay
  
  cost_saving <- pmax(sony_brand$price, sharp_brand$price) - pmin(sony_brand$price, sharp_brand$price)
  price_per_util <- cost_saving/abs(partworths["Price"])
  wtp <- subset(partworths, select = -c(Price)) * c(price_per_util)
  
  print("3. Willingness to Pay:")
  print(wtp)
  
  # 8. Calculate each attribute's utility, preparing for the Market Share Calculation
  
  utility_func <- function(ob_price, brand=own_brand) {
    return(sum(subset(brand, select = -c(price)) * subset(partworths, select = -c(Price))) 
              + (partworths["Price"][1,] * 
                (ob_price - pmin(sony_brand$price, sharp_brand$price)) /
                (pmax(sony_brand$price, sharp_brand$price) - pmin(sony_brand$price, sharp_brand$price))))
  }
  
  utility <- rbind(utility_func(sony_brand$price, sony_brand), utility_func(sharp_brand$price, sharp_brand))
  attractiveness <- exp(utility)
  
  # 9. Calculate Market Share, Sales based on market share, Margin and Profit
  
  final_df <- data.frame(Price = seq(1500, 2600, 100))
  final_df["Attractiveness"] <- exp(unlist(lapply(final_df$Price, utility_func)))
  total_attractiveness <- final_df["Attractiveness"] + attractiveness[1,] + attractiveness[2,]
  final_df["Market_Share"] <- round(final_df["Attractiveness"] / total_attractiveness, 6)
  final_df["Sales"] <- final_df["Market_Share"] * market_size
  final_df["Margin"] <- final_df["Price"]  - net_cost                
  final_df["Profit"] <- final_df["Margin"] * final_df["Sales"]
  
  # 10. Find the Optimal Price which has the Maximum Profit
  
  max_profit <- max(final_df$Profit)
  
  optimal_price <- final_df[final_df$Profit == max_profit, "Price"]
  print(paste("4. Optimal Price:", optimal_price))
  
  print(paste("5. Maximum Profit:", max_profit))
  
  # 11. Plot the relationship between Price ~ Market Share, Price ~ Profit
  
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

hw1(customer)
