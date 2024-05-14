# Import libraries
library(tidyverse)
library(ggplot2)
library(RCurl)
library(jsonlite)
library(ggmap)
library(maps)
library(mapproj)
library(imputeTS)
library(RColorBrewer)
library(stringr)
library(e1071)
library(rpart)
library(rpart.plot)
library(kernlab)
library(caret)
library(corrplot)

# install.packages('corrplot')

# Read in Ames Data
df <- read.csv(file = 'C:/Users/nolan_fur2pfn/OneDrive/Desktop/Syracuse Masters Program/IST 687 - Introduction to Data Science/Final Project/Data/AmesHousing.csv')

# Visualize first 5 rows of dataset
head(df, 5)

# Look at structure of dataset
str(df)

# Glimpse (tidyverse)
glimpse(df)

# Subsetting selected columns
df <- subset(df, select = c(PID, MS.SubClass, Neighborhood, Year.Built, Exter.Qual, Bsmt.Qual, Heating.QC, Kitchen.Qual, Garage.Qual, Wood.Deck.SF, Lot.Area, X1st.Flr.SF, X2nd.Flr.SF, Full.Bath, Half.Bath, SalePrice))

# Removing duplex
df <- df[df$MS.SubClass != 090,]

# Dummifying Neighborhood column
df$Neighborhood <- ifelse(df$Neighborhood == "Blmngtn", 1,
                          ifelse(df$Neighborhood == "Blueste", 2,
                                 ifelse(df$Neighborhood == "BrDale", 3,
                                        ifelse(df$Neighborhood == "BrkSide", 4,
                                               ifelse(df$Neighborhood == "ClearCr", 5,
                                                      ifelse(df$Neighborhood == "CollgCr", 6,
                                                             ifelse(df$Neighborhood == "Crawfor", 7,
                                                                    ifelse(df$Neighborhood == "Edwards", 8,
                                                                           ifelse(df$Neighborhood == "Gilbert", 9,
                                                                                  ifelse(df$Neighborhood == "Greens", 10,
                                                                                         ifelse(df$Neighborhood == "GrnHill", 11,
                                                                                                ifelse(df$Neighborhood == "IDOTRR", 12,
                                                                                                       ifelse(df$Neighborhood == "Landmrk", 13,
                                                                                                              ifelse(df$Neighborhood == "MeadowV", 14,
                                                                                                                     ifelse(df$Neighborhood == "Mitchel", 15,
                                                                                                                            ifelse(df$Neighborhood == "NAmes", 16,
                                                                                                                                   ifelse(df$Neighborhood == "NoRidge", 17,
                                                                                                                                          ifelse(df$Neighborhood == "NPkVill", 18,
                                                                                                                                                 ifelse(df$Neighborhood == "NridgHt", 19,
                                                                                                                                                        ifelse(df$Neighborhood == "NWAmes", 20,
                                                                                                                                                               ifelse(df$Neighborhood == "OldTown", 21,
                                                                                                                                                                      ifelse(df$Neighborhood == "SWISU", 22,
                                                                                                                                                                             ifelse(df$Neighborhood == "Sawyer", 23,
                                                                                                                                                                                    ifelse(df$Neighborhood == "Somerst", 24,
                                                                                                                                                                                           ifelse(df$Neighborhood == "StoneBr", 25,
                                                                                                                                                                                                  ifelse(df$Neighborhood == "Timber", 26,
                                                                                                                                                                                                         ifelse(df$Neighborhood == "Veenker", 27,
                                                                                                                                                                                                                ifelse(df$Neighborhood == "SawyerW", 28,
                                                                                                                                                                                                                       0))))))))))))))))))))))))))))
# Dummifying exter.qual
df$Exter.Qual <- ifelse(df$Exter.Qual == "Ex", 5,
                        ifelse(df$Exter.Qual == "Gd", 4,
                               ifelse(df$Exter.Qual == "TA", 3,
                                      ifelse(df$Exter.Qual == "Fa", 2,
                                             ifelse(df$Exter.Qual == "Po", 1,
                                                    0)))))

# Removing any nulls in exter.qual
df$Exter.Qual[is.na(df$Exter.Qual)] <- 0

# Dummifying bsmt.qual
df$Bsmt.Qual <- ifelse(df$Bsmt.Qual == "Ex", 5,
                       ifelse(df$Bsmt.Qual == "Gd", 4,
                              ifelse(df$Bsmt.Qual == "TA", 3,
                                     ifelse(df$Bsmt.Qual == "Fa", 2,
                                            ifelse(df$Bsmt.Qual == "Po", 1,
                                                   0)))))

# Removing any nulls in bsmt.qual
df$Bsmt.Qual[is.na(df$Bsmt.Qual)] <- 0

# Dummifying heating.qc
df$Heating.QC <- ifelse(df$Heating.QC == "Ex", 5,
                        ifelse(df$Heating.QC == "Gd", 4,
                               ifelse(df$Heating.QC == "TA", 3,
                                      ifelse(df$Heating.QC == "Fa", 2,
                                             ifelse(df$Heating.QC == "Po", 1,
                                                    0)))))
# Removing any nulls in heating.qc
df$Heating.QC[is.na(df$Bsmt.Qual)] <- 0

# Dummifying kitchen.qual
df$Kitchen.Qual <- ifelse(df$Kitchen.Qual == "Ex", 5,
                          ifelse(df$Kitchen.Qual == "Gd", 4,
                                 ifelse(df$Kitchen.Qual == "TA", 3,
                                        ifelse(df$Kitchen.Qual == "Fa", 2,
                                               ifelse(df$Kitchen.Qual == "Po", 1,
                                                      0)))))

# Removing any nulls in kitchen.qual
df$Kitchen.Qual[is.na(df$Kitchen.Qual)] <- 0

# Dummifying garage.qual
df$Garage.Qual <- ifelse(df$Garage.Qual == "Ex", 5,
                         ifelse(df$Garage.Qual == "Gd", 4,
                                ifelse(df$Garage.Qual == "TA", 3,
                                       ifelse(df$Garage.Qual == "Fa", 2,
                                              ifelse(df$Garage.Qual == "Po", 1,
                                                     0)))))

# Removing any nulls in garage.qual
df$Garage.Qual[is.na(df$Garage.Qual)] <- 0

# Combine full bath and half bath. To combine, all half baths will be equal to 0.1. 
df$Half.Bath <- df$Half.Bath * 0.1

# Create new column, total_baths. 2.1 will mean two full, one half bathroom. 3.4 means three full, four half bathrooms.
df$Total.Baths <- df$Full.Bath + df$Half.Bath

# Create new column to look at first and second floor square footage.
df$First.Second.SF <- df$X1st.Flr.SF + df$X2nd.Flr.SF

# Remove Full.Bath and Half.Bath now that we used the two columns to create Total_Baths. Also remove X1st/X2nd Flr SF now that we used the two columns to create First_Second_SF
df <- subset(df, select = -c(Full.Bath, Half.Bath, X1st.Flr.SF, X2nd.Flr.SF))

# Organization of columns
df <- subset(df, select = c(PID, MS.SubClass, Neighborhood, Year.Built, Exter.Qual, Bsmt.Qual, Heating.QC, Kitchen.Qual, Garage.Qual, Wood.Deck.SF, Lot.Area, First.Second.SF, Total.Baths, SalePrice))

head(df, 5)

# BUSINESS QUESTION: WHAT VARIABLES HAVE THE GREATEST IMPACT ON SALE PRICE?
# Create correlation matrix to identify most correlated features with SalePrice, both negatively and positively correlated
res <- cor(df)
corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.sort = 45)

correlation_matrix <- cor(df)
correlation_matrix

# BUSINESS QUESTION: WHAT IMPROVEMENTS CAN BE MADE WITH THE GREATEST ROI?
# Quality of exterior grade by sale price

exter.qual.df <- df %>% group_by(Exter.Qual) %>% 
  summarise(mean_salary=mean(SalePrice),
            .groups = 'drop')

# Almost 100K jump from 3-4, <100k jump from 4-5
# Maybe a condo because you have professionals assist, or condos will be more expensive to new homebuyers because of upkeep
exter.qual.df

# Quality of kitchen grade by sale price
kitchen.qual.df <- df %>% group_by(Kitchen.Qual) %>% 
  summarise(mean_salary=mean(SalePrice),
            .groups = 'drop')

# 1 AND 2 NO DIFFERENT
# Investment wise or first time home buyer, buy a 3 and improve to 4
kitchen.qual.df

# Quality of basement grade by sale price
bsmt.qual.df <- df %>% group_by(Bsmt.Qual) %>% 
  summarise(mean_salary=mean(SalePrice),
            .groups = 'drop')

# IT IS BETTER TO HAVE NO BASEMENT THAN A BAD BASEMENT
# 3-5 jump. I would spend extra money to get a 3 basement grade then improve
bsmt.qual.df

total.baths.df <- df %>% group_by(Total.Baths) %>% 
  summarise(mean_salary=mean(SalePrice),
            .groups = 'drop')

# Half.Baths add about 25k
# Adding a second half bath only worked once, stick with 1
total.baths.df


# BUSINESS QUESTION: HOW DOES SQUARE FOOTAGE IMPACT SALES PRICE?
# Strongest correlated feature: First.Second.SF

# Outliers identified, will query and remove
df %>% 
  ggplot(aes(SalePrice, First.Second.SF)) + 
  geom_point() + geom_smooth(method = "lm") + 
  labs(title = "Linear Model", 
       subtitle = "Price per Square Foot",  
       x = "Sales Price",
       y = "1st + 2nd Floor Square Footage")

# Removal of outliers in First.Second.SF
df <- df[df$First.Second.SF < 4500,]

# Strongest correlated feature: First.Second.SF with outliers removed
# Distinct correlation, as sales price increases, First.Second.SF also increases
df %>% 
  ggplot(aes(SalePrice, First.Second.SF)) + 
  geom_point() + geom_smooth(method = "lm") + 
  labs(title = "Linear Model", 
       subtitle = "Price per Square Foot",  
       x = "Sales Price",
       y = "1st + 2nd Floor Square Footage")

# BUSINESS QUESTION: WHAT EXTERNAL MODIFICATIONS CAN BE MADE WITH THE GREATEST ROI?
# Neighborhood, Exter.Qual, Wood.Deck.SF

# Neighborhood by Sale Price
neighborhood.df <- df %>% group_by(Exter.Qual, Neighborhood) %>% 
  summarise(Average_SalePrice=mean(SalePrice),
            .groups = 'drop')

# If I were looking to find the cheapest possible neighborhood, I would select neighborhood 14.
print(neighborhood.df, n = 30)

#Neighborhood by Average Sale Price
neighborhood.avg <- df %>% group_by(Neighborhood) %>%
  summarise(Average_SalePrice = mean(SalePrice),
            .groups = 'drop')
#Print results
print(neighborhood.avg, n = 28)

# Quality of exterior grade by sale price
# Almost 100K jump from 3-4, <100k jump from 4-5
# Maybe a condo because you have professionals assist, or condos will be more expensive to new homebuyers because of upkeep
# 2 --> 3 $55k
# 3 --> 4 $87k
# 4 --> 5 $147k
exter.qual.df

exter.qual.ggplot <- df %>%
  group_by(Exter.Qual) %>%
  summarise(mean_price = mean(SalePrice))

# Create ggplot object with means plotted by letter grade and data labels
ggplot(exter.qual.ggplot, aes(x = Exter.Qual, y = mean_price)) +
  geom_bar(stat = "identity", fill = "red", color = "black") +
  geom_text(aes(label = round(mean_price, 2)), vjust = -0.5) +
  labs(title = "Avg. Sale Price by Exter.Qual", x = "Exter.Qual", y = "Average Sale Price")

min(df$Wood.Deck.SF)
max(df$Wood.Deck.SF)

# Dummifying wood deck into categories to compare
df$Wood.Deck.SF.Category <- ifelse(df$Wood.Deck.SF >= 1000, 4,
                                   ifelse(df$Wood.Deck.SF >= 750, 3,
                                          ifelse(df$Wood.Deck.SF >= 500, 2,
                                                 ifelse(df$Wood.Deck.SF >= 250, 1,
                                                        0))))

# Create visual to view wood deck sf in categories by sale price
wood.deck.df <- df %>% group_by(Wood.Deck.SF.Category) %>% 
  summarise(Average_SalePrice=mean(SalePrice),
            .groups = 'drop')

# Improvement from a 2-3 in terms of deck square footage is an increase in over $200k
wood.deck.df

wood.deck.ggplot <- df %>%
  group_by(Wood.Deck.SF.Category) %>%
  summarise(mean_price = mean(SalePrice))

# Create ggplot object with means plotted by letter grade and data labels
ggplot(wood.deck.ggplot, aes(x = Wood.Deck.SF.Category, y = mean_price)) +
  geom_bar(stat = "identity", fill = "red", color = "black") +
  geom_text(aes(label = round(mean_price, 2)), vjust = -0.5) +
  labs(title = "Avg. Sale Price by Wood Deck SF Category", x = "Wood Deck SF Category", y = "Average Sale Price")

# Dummifying First.Second.SF into categories to compare
df$First.Second.SF.Categories <- ifelse(df$First.Second.SF >= 4000, 4,
                                        ifelse(df$First.Second.SF >= 3000, 3,
                                               ifelse(df$First.Second.SF >= 2000, 2,
                                                      ifelse(df$First.Second.SF >= 1000, 1,
                                                             0))))

first.second.sf.categories <- df %>% group_by(First.Second.SF.Categories) %>% 
  summarise(Average_SalePrice=mean(SalePrice),
            .groups = 'drop')

first.second.sf.categories

first.second.sf.ggplot <- df %>%
  group_by(First.Second.SF.Categories) %>%
  summarise(mean_price = mean(SalePrice))

# Create ggplot object with means plotted by letter grade and data labels
ggplot(first.second.sf.ggplot, aes(x = First.Second.SF.Categories, y = mean_price)) +
  geom_bar(stat = "identity", fill = "red", color = "black") +
  geom_text(aes(label = round(mean_price, 2)), vjust = -0.5) +
  labs(title = "Avg. Sale Price by First & Second Floor SQFT. Category", x = "First & Second Floor SQFT. Category", y = "Average Sale Price")

# BUSINESS QUESTION: WHAT INTERNAL MODIFICATIONS CAN BE MADE WITH THE GREATEST ROI?

# 1 -> 2 -754
# 2 -> 3 32k
# 3 -> 4 71k
# 4 -> 5 126k
kitchen.qual.df

kitchen.qual.ggplot <- df %>%
  group_by(Kitchen.Qual) %>%
  summarise(mean_price = mean(SalePrice))

# Create ggplot object with means plotted by letter grade and data labels
ggplot(kitchen.qual.ggplot, aes(x = Kitchen.Qual, y = mean_price)) +
  geom_bar(stat = "identity", fill = "red", color = "black") +
  geom_text(aes(label = round(mean_price, 2)), vjust = -0.5) +
  labs(title = "Avg. Sale Price by Kitchen Quality", x = "Kitchen Quality", y = "Average Sale Price")

# 0 -> 1 -21k
# 1 -> 2 25k
# 2 -> 3 30k
# 3 -> 4 63k
# 4 -> 5 134k
bsmt.qual.df

bsmt.qual.ggplot <- df %>%
  group_by(Bsmt.Qual) %>%
  summarise(mean_price = mean(SalePrice))

# Create ggplot object with means plotted by letter grade and data labels
ggplot(bsmt.qual.ggplot, aes(x = Bsmt.Qual, y = mean_price)) +
  geom_bar(stat = "identity", fill = "red", color = "black") +
  geom_text(aes(label = round(mean_price, 2)), vjust = -0.5) +
  labs(title = "Avg. Sale Price by Basement Quality", x = "Basement Quality", y = "Average Sale Price")

heating.qc.df <- df %>% group_by(Heating.QC) %>% 
  summarise(Average_SalePrice=mean(SalePrice),
            .groups = 'drop')

heating.qc.df

heating.qc.ggplot <- df %>%
  group_by(Heating.QC) %>%
  summarise(mean_price = mean(SalePrice))

# Create ggplot object with means plotted by letter grade and data labels
ggplot(heating.qc.ggplot, aes(x = Heating.QC, y = mean_price)) +
  geom_bar(stat = "identity", fill = "red", color = "black") +
  geom_text(aes(label = round(mean_price, 2)), vjust = -0.5) +
  labs(title = "Avg. Sale Price by Heating Quality", x = "Heating Quality", y = "Average Sale Price")

# 1 -> 2 55k !!!! BIG ROI, LARGEST JUMP OUTSIDE 4 -> 5
# 2 -> 3 15k
# 3 -> 4 18k
# 4 -> 5 59k
heating.qc.df

# Adding a second half bath decreased cost every single time except for homes with only one full
# KEY POINT: IF YOU HAVE MORE THAN ONE FULL BATHROOM, DO NOT ADD MORE THAN ONE FULL BATH.
total.baths.df

total.baths.ggplot <- df %>%
  group_by(Total.Baths) %>%
  summarise(mean_price = mean(SalePrice))

# Create ggplot object with means plotted by letter grade and data labels
# DONT BUILD A SECOND HALF BATH UNLESS ONLY 1 FULL
ggplot(total.baths.ggplot, aes(x = Total.Baths, y = mean_price)) +
  geom_bar(stat = "identity", fill = "red", color = "black") +
  geom_text(aes(label = round(mean_price, 2)), vjust = -0.5) +
  labs(title = "Avg. Sale Price by Total Number of Baths (First digit - Full Bathrooms, Second digit - Half baths)", x = "Number of Baths", y = "Average Sale Price")

# BUSINESS QUESTION: OF THE SELECTED VARIABLES WHICH HAS THE LEAST IMPACT ON SALES PRICE?
# When looking at the correlation matrix, the MS.SubClass has the least impact on sales price. MS.SubClass in the correlation matrix has a correlation of -0.07.
SalePrice_Corr <- cor(df[-1], df$SalePrice) 
SalePrice_Corr


# BUSINESS QUESTION: FOR VARIOUS AVAILABLE BUDGETS, WHAT RECOMMENDATIONS COULD BE MADE FOR CURRENT HOUSE CONFIGURATION AND POTENTIAL IMPROVEMENTS FOR GREATEST ROI?

# Exterior: For the greatest ROI potential
# Wood.Deck.SF.Category --> 2, 2-3 232k increase
# Exterior.Qual --> 4, 4-5 147k increase
# Heating.QC --> 1, 1-2 55k
# First.Second.SF.Category --> 3, 3-4 330k increase
# Bsmt.Qual --> 4, 4-5 134k increase
# Kitchen.Qual --> 4-5, 126k increase

model_one <- lm(SalePrice ~ Wood.Deck.SF.Category + Exter.Qual + Heating.QC + First.Second.SF.Categories + Bsmt.Qual + Kitchen.Qual, data = df)

# View summary of the model
summary(model_one)

model_one

# Use input data to test against the model
perfect_roi_home = data.frame(Wood.Deck.SF.Category = 2,
                              Exter.Qual = 4,
                              Heating.QC = 1,
                              First.Second.SF.Categories = 3,
                              Bsmt.Qual = 4,
                              Kitchen.Qual = 4)

perfect_roi_home_finished = data.frame(Wood.Deck.SF.Category = 3,
                                       Exter.Qual = 5,
                                       Heating.QC = 2,
                                       First.Second.SF.Categories = 4,
                                       Bsmt.Qual = 5,
                                       Kitchen.Qual = 5)

# Utilize predict function to call the model you have created, a test dataframe (test), and type="response"
# Predicted price to get the "perfect ROI potential home"
predict(model_one, perfect_roi_home, type="response")

# Predicted price once the "perfect ROI potential home" is completed
predict(model_one, perfect_roi_home_finished, type="response")

# Investment profit (ROI) of $156597, now to account for labor and materials.
print(517200-360603)
