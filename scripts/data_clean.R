
# get package
package <- show_package("64b54586-6180-4485-83eb-81e8fae3b8fe")
package

# get all resources for this package
resources <- list_package_resources("64b54586-6180-4485-83eb-81e8fae3b8fe")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
raw_data_final <- filter(datastore_resources, row_number()==1) %>% get_resource()
raw_data_final

raw_data <- na.omit(raw_data_final)
raw_data

my_data <- raw_data %>% select(c(`Source of Infection`))
a <- table(my_data)



my_data4 <- raw_data %>% select(c(`Ever Hospitalized`))
d <- table(my_data4)



df_source_table <- "Source of Infection,number of People,Percetage,Total
  Travel,103,13.92%,740
  Community,613,10.29%,5959
  Outbreaks Congregate Settings,108,13.17%,820
  Household Contact,306,4.91%,6228
  No Information,300,3.41%,8788
  Outbreaks Healthcare Institutions,763,13.81%,5526
  Outbreaks Other Settings,33,3.07%,1076
  Pending,5,11.36%,44
  Close Contact,153,4.91%,3119
"

data_source_table <- read_csv(df_source_table) #Creating the csv file of df_source_table. 

gt_tbl_3 <-gt(data_source_table)
gt_tbl_3 <-
  gt_tbl_3 %>%
  tab_header(
    title = "Table1: Summary of Source ofinfection",
    subtitle = "Data source: provincial Case & Contact Management System (CCM)")

#Creating a table for data_source_table. 
gt_tbl_3 <- gt_tbl_3 %>% 
  #Apply new style to all column headers
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      #Give a thick border below
      cell_borders(sides = "bottom", weight = px(3)),
      #Make text bold
      cell_text(weight = "bold")
    )
  ) %>% 
  #Apply different style to the title
  tab_style(
    locations = cells_title(groups = "title"),
    style     = list(
      cell_text(weight = "bold", size = 24)
    )
  )
gt_tbl_3 



my_data2 <- raw_data %>% select(c(`Client Gender`))
b <- table(my_data2)


df_gender_table <- "Client Gender,number of People,Percetage,Total
  FEMALE,760,4.49%,16936
  MALE,901,6.09%,14816
  NON-BINARY,0,0.00%,4
  OTHER,0,0.00%,6
  TRANSGENDER,0,0.00%,5
  UNKNOWN,0,0.00%,233
"

data_gender_table <- read_csv(df_gender_table) #Creating the csv file of df_gender_table. 

gt_tbl_5 <-gt(data_gender_table )
gt_tbl_5 <-
  gt_tbl_5 %>%
  tab_header(
    title = "Table2: Summary of Client Gender",
    subtitle = "Data source: provincial Case & Contact Management System (CCM)")

#Creating a table for data_gender_table . 
gt_tbl_5 <- gt_tbl_5 %>% 
  #Apply new style to all column headers
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      #Give a thick border below
      cell_borders(sides = "bottom", weight = px(3)),
      #Make text bold
      cell_text(weight = "bold")
    )
  ) %>% 
  #Apply different style to the title
  tab_style(
    locations = cells_title(groups = "title"),
    style     = list(
      cell_text(weight = "bold", size = 24)
    )
  )
gt_tbl_5



my_data3 <- raw_data %>% select(c(`Age Group`))
c <- table(my_data3)


df_age_table <- "Age Group,number of People,Percetage,Total
  19 and younger,23,0.74%,3112
  20 to 29 Years,43,0.71%,6047
  30 to 39 Years,64,0.12%,5526
  40 to 49 Years,102,2.21%,4613
  50 to 59 Years,167,3.69%,4529
  60 to 69 Years,295,10.12%,2914
  70 to 79 Years,347,19.41%,1787
  80 to 89 Years,413,20.23%,2042
  90 and older,211,15.49%,1362
"

data_age_table <- read_csv(df_age_table) #Creating the csv file of df_age_table. 

gt_tbl_6 <-gt(data_age_table)
gt_tbl_6 <-
  gt_tbl_6 %>%
  tab_header(
    title = "Table3: Summary of Age Group",
    subtitle = "Data source: provincial Case & Contact Management System (CCM)")

#Creating a table for data_age_tablee . 
gt_tbl_6 <- gt_tbl_6 %>% 
  #Apply new style to all column headers
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      #Give a thick border below
      cell_borders(sides = "bottom", weight = px(3)),
      #Make text bold
      cell_text(weight = "bold")
    )
  ) %>% 
  #Apply different style to the title
  tab_style(
    locations = cells_title(groups = "title"),
    style     = list(
      cell_text(weight = "bold", size = 24)
    )
  )
gt_tbl_6


my_data_merge1 <- raw_data %>% select(c(`Ever Hospitalized`, `Source of Infection`))


clean1 <- as_tibble(my_data_merge1) %>% 
  filter(`Ever Hospitalized` == "Yes")
data_test1 <- clean1 %>% filter(`Source of Infection` == "Travel")  #103 travel
data_test2 <- clean1 %>% filter(`Source of Infection` == "Community") #613 community
data_test3 <- clean1 %>% filter(`Source of Infection` == "Outbreaks, Congregate Settings") #108 congregates settings
data_test4 <- clean1 %>% filter(`Source of Infection` == "Household Contact") #306 household contact
data_test5 <- clean1 %>% filter(`Source of Infection` == "No Information") #300 no information
data_test6 <- clean1 %>% filter(`Source of Infection` == "Outbreaks, Healthcare Institutions") #763 HEALTHCARE INSTITUTIONS
data_test7 <- clean1 %>% filter(`Source of Infection` == "Outbreaks, Other Settings") #33 other settings
data_test8 <- clean1 %>% filter(`Source of Infection` == "Pending") #5 pending
data_test9 <- clean1 %>% filter(`Source of Infection` == "Close Contact")  #153 close contact

# Demo data
Source_of_infection <- data.frame(
  Travel = c(13.92),
  Community = c(10.29),
  Congregate = c(13.17),
  Household_Contact = c(4.91),
  NI = c(3.41),
  Healthcare_Institutions = c(13.81),
  Other = c(3.07),
  Pending = c(11.36),
  Close_Contact = c(4.91)
)

rownames(Source_of_infection) <- c("Source_of_infection")

# Define the variable ranges: maximum and minimum
max_min <- data.frame(
  Travel = c(20, 0), 
  Community = c(20, 0), 
  Congregate = c(20, 0),
  Household_Contact = c(20, 0), 
  NI = c(20, 0), 
  Healthcare_Institutions = c(20, 0),
  Other = c(20, 0), 
  Pending = c(20, 0), 
  Close_Contact = c(20, 0)
)
rownames(max_min) <- c("Max", "Min")

# Bind the variable ranges to the data
df <- rbind(max_min, Source_of_infection)


# Plot the data for Source of infection


Source_of_infection_data <- df[c("Max", "Min", "Source_of_infection"), ]
radarchart(Source_of_infection_data,
           vlcex = 0.8,
           title= "Figure1: Source of infection",
           sub = "Note: NI = No Information", 
           axistype = 1,
           cglcol = "gray", cglty = 2, cglwd = 0.8,
           axislabcol = "grey",
           caxislabels = c(0, 5, 10, 15, 20),
           cex.main = 2,
           pcol =rgb(0.2,0.5,0.5,0.9),
           pfcol=rgb(0.2,0.5,0.5,0.5),
           plwd=1,
           cex.sub = 0.7, adj = 0)



my_data_merge4 <- raw_data %>% select(c(`Ever Hospitalized`, `Age Group`))


clean4 <- as_tibble(my_data_merge4) %>% 
  filter(`Ever Hospitalized` == "Yes")
data_test20 <- clean4 %>% filter(`Age Group` == "19 and younger")  #23 19+
data_test21 <- clean4 %>% filter(`Age Group` == "20 to 29 Years") #43 20~29
data_test22 <- clean4 %>% filter(`Age Group` == "30 to 39 Years") #64 30~39
data_test23 <- clean4 %>% filter(`Age Group` == "40 to 49 Years") #102 40~49
data_test24 <- clean4 %>% filter(`Age Group` == "50 to 59 Years") #167 50~59
data_test25 <- clean4 %>% filter(`Age Group` == "60 to 69 Years") #295 60~69
data_test26 <- clean4 %>% filter(`Age Group` == "70 to 79 Years") #347 70~79
data_test27 <- clean4 %>% filter(`Age Group` == "80 to 89 Years") #413 80~89
data_test28 <- clean4 %>% filter(`Age Group` == "90 and older")  #211 90+


age_data <- tribble(
  ~age_group, ~number_of_people,
  "19 and younger",         23,
  "20 to 29 Years",         43,
  "30 to 39 Years",         64,
  "40 to 49 Years",         102,
  "50 to 59 Years",         167,
  "60 to 69 Years",         295,
  "70 to 79 Years",         347,
  "80 to 89 Years",         413,
  "90 and older",         211
)

age_data_sorted <- age_data %>%
  mutate(age_group = fct_reorder(age_group, number_of_people, .desc = TRUE))

ggplot(age_data_sorted,
       aes(x = number_of_people,
           xend = 0,
           y = age_group,
           yend = age_group,
           colour = age_group)) +
  geom_segment() +
  geom_point() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(breaks = c("19 and younger", "20 to 29 Years", "30 to 39 Years",
                                 "40 to 49 Years", "50 to 59 Years", "60 to 69 Years", "70 to 79 Years", "80 to 89 Years", "90 and older"),
                      values = c("#DE3533", "#0047AB", "#006644",
                                 "#10C25B", "#808080", "#F0E442", "#56B4E9", "#D55E00", "#CC79A7")) +
  labs(x = "Number of People",
       y = "Age Group",
       title = "Figure2: Age distribution of inpatients",
       subtitle = "hospitalization related to covid-19",
       caption = "Data source: provincial Case & Contact Management System (CCM)") +
  theme(legend.position = "off") +
  theme_bw()



# we merge the two columns, "ever in ICU" and "Age group" to see the number of people who ever in ICU among bigger age groups
my_data_merge2 <- raw_data %>% select(c(`Ever in ICU`, `Age Group`))

clean2 <- as_tibble(my_data_merge2) %>% 
  filter(`Ever in ICU` == "Yes")
data_test15 <- clean2 %>% filter(`Age Group` == "50 to 59 Years")  #44 50~59 (23.61%)
data_test16 <- clean2 %>% filter(`Age Group` == "60 to 69 Years") #83 60~69 (29.44%)
data_test17 <- clean2 %>% filter(`Age Group` == "70 to 79 Years") #77 70~79 (22.96%)
data_test18 <- clean2 %>% filter(`Age Group` == "80 to 89 Years") #51 80~89 (11.04%)
data_test19 <- clean2 %>% filter(`Age Group` == "90 and older") #17 90+  (4.06%)

#create the data frame of the relationship between age and "ever in icu"
df_age_icu <- "Age,number of People,Percetage
  50 to 59 Years,44,23.61%
  60 to 69 Years,83,29.44%
  70 to 79 Years,77,22.96%
  80 to 89 Years,51,11.04%
  90 and older,17,4.06%
"

data_age_icu <- read_csv(df_age_icu) #Creating the csv file of df_age_icu. 

gt_tbl_1 <-gt(data_age_icu)
gt_tbl_1 <-
  gt_tbl_1 %>%
  tab_header(
    title = "Table4: Proportion of infected people admitted to ICU",
    subtitle = "Note: elderly people")

#Creating a table for data_age_icu. 
gt_tbl_1 <- gt_tbl_1 %>% 
  #Apply new style to all column headers
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      #Give a thick border below
      cell_borders(sides = "bottom", weight = px(3)),
      #Make text bold
      cell_text(weight = "bold")
    )
  ) %>% 
  #Apply different style to the title
  tab_style(
    locations = cells_title(groups = "title"),
    style     = list(
      cell_text(weight = "bold", size = 24)
    )
  )
gt_tbl_1 <- gt_tbl_1 %>% 
  tab_style(
    style = cell_text(color = "#F2CB05", weight = "bold"),
    locations = cells_body(
      columns = 1, 
      rows = Age %in% c("80 to 89 Years")
    )
  )
gt_tbl_1
# then we will highlight the biggest percetage in step 2. 



my_data_merge3 <- raw_data %>% select(c(`Ever Hospitalized`, `Client Gender`))

clean3 <- as_tibble(my_data_merge3) %>% 
  filter(`Ever Hospitalized` == "Yes")
data_test10 <- clean3 %>% filter(`Client Gender` == "MALE")  #901 6.08%
data_test11 <- clean3 %>% filter(`Client Gender` == "FEMALE") #760 4.49%
data_test12 <- clean3 %>% filter(`Client Gender` == "NON-BINARY") #0 
data_test13 <- clean3 %>% filter(`Client Gender` == "OTHER") #0 
data_test14 <- clean3 %>% filter(`Client Gender` == "TRANSGENDER") #0 

gender_data <- data.frame(
  gender = factor(c("Male","Female"), levels=c("Male","Female")),
  number_of_people = c(901, 760)
)

ggplot(data=gender_data, aes(x=gender, y=number_of_people, fill=gender)) +
  geom_bar(stat="identity")+
  theme_bw() +
  labs(title = "Figure3: Gender Distribution of Inpatients Related to COVID-19",
       subtitle = "Source: provincial Case & Contact Management System (CCM)") + 
  labs(x = "Gender", y = "Number of People")