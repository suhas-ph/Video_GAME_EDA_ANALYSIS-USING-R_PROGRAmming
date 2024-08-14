library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lattice)

# Reading the CSV file
vg <- read.csv("C:/Users/phsuh/vgsales.csv")
head(vg)
str(vg)
summary(vg)

# Converting year character to integer type
vg$Year <- as.integer(as.character(vg$Year))
is.integer(vg$Year)
# Checking for null values in year
is.null(vg)
# Removing N/A values from the dataset
vgs <- vg %>% drop_na(Year)

dim(vgs)

# Saving the data set
vgs %>% write.csv("vgs")

# 1. Genre Popularity by Count
Genre <- vgs %>% group_by(Genre) %>% summarize(count=n()) %>% arrange(desc(count))
ggplot((data = Genre), aes(x = reorder(Genre, -count), y = count)) + 
  geom_col(fill = "Dark Green") + 
  labs(title = "Genre Popularity", x = "Genre") + 
  theme(axis.text.x= element_text(size =10, angle = 90))

# 2. Genre Popularity Over the Years by Sales (Top 5 Genres) with Outliers
top_genres <- vgs %>%
  group_by(Genre) %>%
  summarize(total_sales = sum(Global_Sales)) %>%
  arrange(desc(total_sales)) %>%
  slice(1:5) %>%
  pull(Genre)

vgs_top_genres <- vgs %>%
  filter(Genre %in% top_genres)

vgs_top_genres_outliers <- vgs_top_genres %>%
  filter(Year < 2017) %>%
  group_by(Genre, Year) %>%
  summarize(total_sales = sum(Global_Sales)) %>%
  ungroup() %>%
  group_by(Genre) %>%
  mutate(
    Q1 = quantile(total_sales, 0.25),
    Q3 = quantile(total_sales, 0.75),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR,
    Outlier = ifelse(total_sales < Lower_Bound | total_sales > Upper_Bound, "Outlier", "Normal")
  )

vgs_top_genres_outliers %>%
  ggplot(aes(x = Year, y = total_sales, color = Genre)) +
  geom_line(size = 1) +
  geom_point(aes(shape = Outlier), size = 2) +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c("Normal" = 16, "Outlier" = 17)) +
  theme_classic() +
  labs(title = "Genre Popularity over the years by Sales (Top 5 Genres) with Outliers", y = "Global Sales (Millions)") +
  facet_wrap(~ Genre)

# 3. Box Plot of Count of Games Released Over the Years (Top 5 Genres)
top_genres_by_count <- vgs %>%
  group_by(Genre) %>%
  summarize(total_count = n()) %>%
  arrange(desc(total_count)) %>%
  slice(1:5) %>%
  pull(Genre)

vgs_top_genres_count <- vgs %>%
  filter(Genre %in% top_genres_by_count)

genres <- subset(vgs_top_genres_count, select = c(Year, Genre)) %>%
  filter(Year < 2017) %>%
  group_by(Year, Genre) %>%
  summarize(total = n())

genres <- genres %>%
  group_by(Genre) %>%
  mutate(
    Q1 = quantile(total, 0.25),
    Q3 = quantile(total, 0.75),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR,
    Outlier = ifelse(total < Lower_Bound | total > Upper_Bound, "Outlier", "Normal")
  )

ggplot(genres, aes(x = Genre, y = total, fill = Genre)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 2) +
  geom_jitter(aes(color = Outlier), width = 0.2, size = 2) +
  scale_color_manual(values = c("Normal" = "black", "Outlier" = "red")) +
  theme_classic() +
  labs(title = "Box Plot of Count of Games Released Over the Years (Top 5 Genres)", y = "Count of Games Released", x = "Genre")

# 4. Top 10 Games by Global Sales
vgs %>% group_by(Name) %>% summarize(total = sum(Global_Sales)) %>% arrange(desc(total)) %>% slice(1:10) %>% 
  ggplot(aes(x = reorder(Name, -total), y = total)) + 
  geom_col(fill = "Dark Green") + 
  labs(title = "Top 10 Games by Global Sale", x= "Name", y = "Total Sales (Millions)") + 
  theme_classic() + 
  theme(axis.text.x= element_text(size =10, angle = 90))

# 5. Top 10 Publishers by Global Sales
vgs %>% group_by(Publisher) %>% summarize(total = sum(Global_Sales)) %>% arrange(desc(total)) %>% slice(1:10) %>% 
  ggplot(aes(x = reorder(Publisher, -total), y = total)) + 
  geom_col(fill = "Dark Green") + 
  labs(title = "Top 10 Publishers by Global Sale", x= "Publishers", y = "Total Sales (Millions)") + 
  theme_classic() + 
  theme(axis.text.x= element_text(size =10, angle = 90))

# 6. Top 10 Platforms by Global Sales
vgs %>% group_by(Platform) %>% summarize(total = sum(Global_Sales)) %>% arrange(desc(total)) %>% slice(1:10) %>% 
  ggplot(aes(x = reorder(Platform, -total), y = total)) + 
  geom_col(fill = "Dark Green") + 
  labs(title = "Top 10 Platforms by Global Sale", x= "Platforms", y = "Total Sales (Millions)") + 
  theme_classic() + 
  theme(axis.text.x= element_text(size =10, angle = 90))


# 7. Genre Sales per Region
sale = vgs %>%
  subset(select = c(Genre, NA_Sales, EU_Sales, JP_Sales, Other_Sales)) %>%
  group_by(Genre) %>%
  pivot_longer(c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales"), names_to = "Region", values_to = "Sales")

ggplot(sale, aes(x = Genre, y = Sales, fill = Region)) + 
  geom_col(position = "dodge") + 
  labs(title = "Genre Sales per Region", x= "Genre", y = "Sales (Millions)") + 
  theme_bw() + 
  theme(axis.text.x= element_text(size =10, angle = 90)) + 
  scale_fill_brewer(palette = "Set1") + 
  facet_wrap(~ Region)

sales_data <- vgs %>%
  subset(select = c(Year, NA_Sales, EU_Sales, JP_Sales, Other_Sales, Global_Sales)) %>%
  group_by(Year) %>%
  summarize(NAsales = sum(NA_Sales, na.rm = TRUE),
            EUsales = sum(EU_Sales, na.rm = TRUE),
            JPsales = sum(JP_Sales, na.rm = TRUE),
            OCsales = sum(Other_Sales, na.rm = TRUE),
            Global = sum(Global_Sales, na.rm = TRUE)) %>%
  pivot_longer(cols = c(NAsales, EUsales, JPsales, OCsales), names_to = "Region", values_to = "Sales")

# Plot
ggplot(sales_data, aes(x = Year, y = Sales, colour = Region)) +
  geom_line(size = 1) +
  labs(title = "Sales Trend over the Years", y = "Sales (Millions)") +
  scale_colour_manual(name = "Region", values = c("NAsales" = "blue", "EUsales" = "green", "JPsales" = "red", "OCsales" = "purple")) +
  theme_classic() +
  facet_wrap(~ Region, scales = "free_y") +
  theme(
    strip.background = element_rect(fill = "gray90", color = "black"), # Box around facet labels
    panel.border = element_rect(color = "black", fill = NA, size = 1) # Box around each panel
  )
