library(dplyr)
library(tidyr)


data <- read.csv("refine.csv")
df <- tbl_df(data)

# ****************
# Local functions
# ****************
#
#
# Returns dataframe with normalized company name field
#
clean_up_brands <- function ( rawdata) {
  result <- mutate(rawdata, company = ifelse(grepl("^ph", tolower(df$ï..company)),"philips", 
                                     
                                        ifelse(grepl("^fill", tolower(df$ï..company)), "philips",
                                            
                                            ifelse(grepl("^ak", tolower(df$ï..company)),"akzo", 
                                                   
                                                   ifelse(grepl("^van", tolower(df$ï..company)), "van houten",
                                                          
                                                          ifelse(grepl("^uni", tolower(df$ï..company)), "unilever","unknown"))))))
 return(result)  
}

# ==============================================================================================================================================
# Returns product category
#
add_category_field <- function (rawdata) {
  
  result <- mutate(rawdata, category = ifelse(product_code == "p","Smartphone", 
                                       ifelse(product_code == "v","TV", 
                                              ifelse(product_code == "x", "Laptop", 
                                                     ifelse(product_code == "q", "Tablet", "Unknown")))))
  return(result)
}

# ==============================================================================================================================================
# Add dummy variables
# ==============================================================================================================================================
add_dummy_fields <- function(rawdata) {
  
  company_philips <- as.numeric(rawdata$company == "philips")
  company_akzo <- as.numeric(rawdata$company == "akzo")
  company_van_houten <- as.numeric(rawdata$company == "van houten")
  company_unilever <- as.numeric(rawdata$company == "unilever")
  
  product_smartphone <- as.numeric(rawdata$product_code == "p")
  product_tv <- as.numeric(rawdata$product_code == "v")
  product_laptop <- as.numeric(rawdata$product_code == "x")
  product_tablet <- as.numeric(rawdata$product_code == "q")
  
  dummyvars <- data.frame(company_philips, company_akzo, company_van_houten, company_unilever, product_smartphone, product_tv, product_laptop, product_tablet)
  
  result <- cbind(rawdata, dummyvars)
  
  return(result)
}



# ***********************************
# Main
# ***********************************

#
# Normalize company name
#
df2 <- clean_up_brands(df)

#
# Seperate product code from product number
#
df3 <- separate(df2, Product.code...number, c("product_code","product_number"), sep = "-")

#
# Add product category field
#
df4 <- add_category_field(df3)

#
# Add full address field for Geolocation use
#
df5 <- unite(df4, full_address, address, city, country, sep = ",")

#
# Add dummy fields
#
df6 <- add_dummy_fields(df5)

#
# Create cleaned output file
#
write.csv(df6, "refine_clean.csv")

