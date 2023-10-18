#Lee Hua Xuen
#TP063031

#Data Import
#data = read.csv("C:\\Work\\PFDA\\employee_attrition.csv", header = TRUE)
data = read.csv("D:\\Work\\YEAR 2 SEM 1\\PFDA\\employee_attrition.csv", header = TRUE)


View(data)

#Library
library(tidyverse)
library(ggplot2)
library(GGally)
library(magrittr)
library(ggridges)
library(plotly)
library(dplyr)
library(scales)

#Data Cleaning
any(is.na(data))
sum(is.na(data))
data[duplicated(data)]

#Data Pre-Processing


  #this data pre-processing converts the respective data into factor and date datatype.
c_data <- data %>% mutate(
  orighiredate_key = as.Date(orighiredate_key, format = "%m/%d/%Y"),
  terminationdate_key = ifelse(terminationdate_key == "1/1/1900", "Not Applicable", terminationdate_key), #changes every "1/1/1900" to "Not Applicable"
  terminationdate_key = as.Date(terminationdate_key, format = "%m/%d/%Y"),
  city_name = as.factor(city_name),
  department_name = as.factor(department_name),
  job_title = as.factor(job_title),
  store_name = as.factor(store_name),
  gender_full = as.factor(gender_full),
  termreason_desc = ifelse(termreason_desc == "Resignaton", "Resignation", termreason_desc), #changes every "Resignaton" to "Resignation"
  termreason_desc = as.factor(termreason_desc),
  STATUS_YEAR = as.factor(STATUS_YEAR),
  STATUS = as.factor(STATUS),
  BUSINESS_UNIT = as.factor(BUSINESS_UNIT)
)%>%
select(everything(), -c(gender_short, recorddate_key, birthdate_key)) #remove respective columns


summary(c_data)


colnames(c_data) <- c('EmployeeID',
                    'Date_of_Hire',
                    'Date_of_Termination',
                    'Age',
                    'Length_of_Service',
                    'City',
                    'Department',
                    'Job_Title',
                    'Store',
                    'Gender',
                    'Termination_Reason',
                    'Termination_Type',
                    'Status_Year',
                    'Status',
                    'Business_Unit')


#Data Exploration
str(c_data)
names(c_data)
summary(c_data)

head(c_data) 

tail(c_data)

ggplot(c_data, aes(x = Length_of_Service)) +
geom_histogram(binwidth = 1) +
facet_wrap(~ Department, scales = "free") +
labs(x = "Length of Service", y = "Count")

# Question 1: Why do employees leave the company?
#Analysis 1: The number of employees that left over the last 5 years. 
  latest_date <- max(c_data$Date_of_Termination, na.rm = TRUE) #get the latest year from the dataset
  latest_year <- as.integer(format(latest_date, "%Y"))
  
  preceding_years <- seq(latest_year - 1, latest_year - 4) #get the preceding 4 years before the latest year
  
  year_list <- c(latest_year, preceding_years) #concatenate them to a vector
  year_list
  
  filtered_data <- c_data %>% #filter the latest 5 years in which employees resigned
    filter(year(Date_of_Termination) %in% year_list, Termination_Reason == "Resignation")
  
  ggplot(filtered_data, aes(x = year(Date_of_Termination))) +    #creation of histogram
    geom_bar(fill = "blue") +
    labs(x = "Year", y = "Frequency of Resignation") +
    ggtitle("The frequency of resignation in the latest 5 years") +
    theme_minimal()


#Analysis 2: The frequency of resignation in genders in the last 5 years
  ggplot(filtered_data, aes(x = year(Date_of_Termination), fill = Gender)) +
  geom_bar(position = "stack") + #using a bar plot graph
  labs(x = "Year", y = "Count", fill = "Gender") +
  ggtitle("Resignations by Gender in the Last 5 Years") +
  theme_dark()
  
  
#Analysis 3: Resignation rate of the departments. 
  resignations <- c_data %>% filter(Termination_Reason == "Resignation") #only include date where Termination_Reason == "Resignation"
  department_counts <- table(resignations$Department)
  
  plot_ly(labels = names(department_counts), values = department_counts, type = "pie") %>% #creates the pie chart with the department names as the labels (slice of pie)
  layout(title = "Resignation Rate by Department (3D Pie Chart)", scene = list(aspectmode = "data")) #aspectmode = "data" so that the aspect ratio is based on the data.
  
    
#Analysis 4: Relationship between resignation and age.
  ggplot(c_data, aes(x = Termination_Reason, y = Age, fill = Termination_Reason)) +
  geom_boxplot() +
  labs(x = "Termination Reason", y = "Age") +
  ggtitle("Relationship between Termination reason and Age") +
  scale_fill_manual(values = c("Layoff" = "blue",
                               "Not Applicable" = "red",
                               "Resignation" = "yellow",
                               "Retirement" = "green"))
    
#Analysis 5: Relationship between resignation and city.
   
  city_resignation_counts <- c_data %>%
  group_by(City) %>%
  filter(Termination_Reason == "Resignation") %>%
  summarize(resignation_count = n()) %>%
  arrange(desc(resignation_count))
  
  print(as.data.frame(city_resignation_counts))
    
    
#Analysis 6: Relationship between resignation and length of service.
  
  ggplot(c_data %>% filter(Termination_Reason == "Resignation"), aes(x = Length_of_Service, y = ..count..)) +
  geom_line(stat = "bin", binwidth = 1, color = "blue") +
  labs(x = "Length of Service", y = "Frequency of resignation") +
  ggtitle("Relationship between Resignation and Length of Service") +
  theme_minimal()  
    
#Analysis 7: Trend of resignation in stores.
  
  resignation_counts <- c_data %>%
    filter(Termination_Reason == "Resignation") %>%
    group_by(Store) %>%
    summarize(Resignations = n())
  
  ggplot(resignation_counts, aes(x = Store, y = Resignations, fill = Resignations)) +
    geom_bar(stat = "identity", width = 0.7) +
    labs(x = "Store", y = "Resignations") +
    ggtitle("Resignation Count by Store") +
    scale_fill_gradient(low = "darkblue", high = "lightblue") 

#Analysis 8: Relationship between resignation and job titles.
  
  resignations_job <- c_data %>%
    filter(Termination_Reason == "Resignation") %>%
    group_by(Job_Title) %>%
    summarize(Resignation_Count = n())
  
  ggplot(resignations_job, aes(x = Job_Title, y = Resignation_Count, fill = Job_Title)) +
    geom_bar(stat = "identity", width = 0.7) +
    labs(x = "Job Title", y = "Resignation Count", fill = "Job Title") +
    ggtitle("Resignation Count by Job Title") +
    theme_minimal()
  
#Analysis 9: Relationship of the resignation rate of employees of both genders, and their job title.
  
  ggplot(filtered_data, aes(x = Job_Title, fill = Gender)) +
    geom_density(alpha = 0.5) +
    labs(x = "Job Title", y = "Density", fill = "Gender") +
    ggtitle("Density Plot of Resignations by Gender and Job Title") +
    theme_minimal()
  
#Analysis 10 Job titles with the highest resignation count for employees age 20-30: 
  
  filtered_data <- c_data %>%
    filter(Age >= 20, Age <= 30)

  resignation_counts <- filtered_data %>%   #calculate resignation count by job title
    group_by(Job_Title) %>%
    summarize(Resignation_Count = n()) %>%
    arrange(desc(Resignation_Count))
  
  ggplot(resignation_counts, aes(x = reorder(Job_Title, -Resignation_Count), y = Resignation_Count, fill = Resignation_Count)) +
    geom_bar(stat = "identity") +
    labs(x = "Job Title", y = "Resignation Count", fill = "Resignation Count") +
    ggtitle("Employee Resignation Count by Job Title (Age 20-30)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_gradient(low = "orange", high = "red")
 
  
#Question 2: Is there a gender bias in terms of employee layoffs?
#Analysis 1: Number of employees laid off compared to other termination reasons.
  filtered_data <- c_data %>%
    filter(Termination_Reason %in% c("Resignation", "Layoff", "Retirement"))
  

  termination_counts <- filtered_data %>% 
    group_by(Termination_Reason) %>%
    summarize(Employee_Count = n())
  
  
  ggplot(termination_counts, aes(x = Termination_Reason, y = Employee_Count, fill = Termination_Reason)) +  
    geom_bar(stat = "identity") +
    labs(x = "Termination Reason", y = "Number of Employees", fill = "Termination Reason") +
    ggtitle("Number of employees laid off compared to other termination reasons") +
    theme_minimal()
  
  
#Analysis 2: Which job title has the highest layoff.
  layoffs <- c_data %>% filter(Termination_Reason == "Layoff") #filter data for layoffs
  job_counts <- table(layoffs$Job_Title) #count layoffs by job title
  
  plot_ly(labels = names(job_counts), values = job_counts, type = "pie") %>%
    layout(title = "Layoff Distribution by Job Title")
    
  
#Analysis 3: Relationship between layoff and gender.

  layoffs <- c_data %>% filter(Termination_Reason == "Layoff")
  gender_counts <- layoffs %>% count(Gender)
  
  ggplot(gender_counts, aes(x = Gender, y = n, fill = Gender)) +
    geom_bar(stat = "identity", width = 0.6) +
    labs(title = "Layoffs by Gender", x = "Gender", y = "Count") +
    theme_minimal() 

#Analysis 4: Frequency of layoff in the last 5 years. 

  filtered_data <- c_data %>%
    filter(Termination_Reason == "Layoff" & !is.na(Date_of_Termination)) %>%   #filter the data for layoffs in the last five years
    mutate(Termination_Year = year(Date_of_Termination))
  
  latest_year <- max(filtered_data$Termination_Year) #get the latest non-missing termination year
  
  preceding_years <- seq(latest_year - 4, latest_year)  #create a sequence of five years
  

  layoff_counts <- filtered_data %>%   #count the number of employees for each year
    filter(Termination_Year %in% preceding_years) %>%
    count(Termination_Year)
  

  layoff_counts <- layoff_counts %>%   #complete the data to include all five years with zero counts
    complete(Termination_Year = preceding_years, fill = list(n = 0))
  

  layoff_counts$Termination_Year <- factor(layoff_counts$Termination_Year, levels = preceding_years)   #convert Termination_Year to factor with all five years
  
  # Create the horizontal stacked bar plot
  ggplot(layoff_counts, aes(x = n, y = Termination_Year, fill = Termination_Year)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = n), vjust = -0.5, color = "black", size = 4) +
    labs(x = "Number of Employees", y = "Year") +
    ggtitle("Frequency of Employee Layoffs in the Last 5 Years") +
    theme_minimal() 
  
#Analysis 5: Relationship between employee layoffs and length of service. 
  
  filtered_data <- c_data %>%   #filter the data for layoffs and non-null length of service
    filter(Termination_Reason == "Layoff" & !is.na(Length_of_Service))
  
  layoff_counts <- filtered_data %>%   #calculate the count of layoffs at each length of service
    group_by(Length_of_Service) %>%
    summarise(Layoff_Count = n())
  
  ggplot(layoff_counts, aes(x = Length_of_Service, y = Layoff_Count)) +   #create the scatter plot
    geom_point() +
    geom_smooth(method = "loess", se = FALSE) +
    labs(x = "Length of Service", y = "Layoff Count") +
    ggtitle("Relationship between Employee Layoffs and Length of Service") +
    theme_minimal()
  
  
#Analysis 6: Relationship between layoff rates and departments.
  
  layoff_data <- c_data %>% 
    filter(Termination_Reason == "Layoff")
 
  layoff_counts <- layoff_data %>%  #calculate the layoff count by department
    count(Department)
  
  layoff_counts <- layoff_counts %>%  #sort the departments by layoff count in descending order
    arrange(desc(n))

  print(layoff_counts)  
  
  
#Analysis 7: Relationship between stores and layoff counts.
  
  layoff_data <- c_data %>%   #calculate the layoff count and total number of employees by store
    group_by(Store) %>%
    summarise(Layoff_Count = sum(Termination_Reason == "Layoff"),
              Total_Employees = n())
  
  ggplot(layoff_data, aes(x = Store, y = Layoff_Count, size = Total_Employees)) +   #create a bubble chart
    geom_point(color = "darkblue", alpha = 0.7) +
    labs(x = "Store", y = "Layoff Count", size = "Total Employees") +
    ggtitle("Relationship between Stores and Layoff Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
  
#Analysis 8: Relationship between layoff count and age.
  filtered_data <- c_data %>%   #filter the data for layoffs and non-null age
    filter(Termination_Reason == "Layoff" & !is.na(Age))
  
  layoff_counts <- filtered_data %>%   #calculate the layoff count by age
    group_by(Age) %>%
    summarise(Layoff_Count = n())
  
  ggplot(layoff_counts, aes(x = Age, y = Layoff_Count)) +   #create the scatter plot with smoothed line

    geom_point() +
    geom_smooth(method = "loess", color = "red", se = TRUE) +
    labs(x = "Age", y = "Layoff Count") +
    ggtitle("Relationship between Layoff Count and Age") +
    theme_minimal()
  
  
#Analysis 9: Relationship between layoff count and city.
  
  layoff_counts <- c_data %>%   #calculate the layoff count by city
    filter(Termination_Reason == "Layoff") %>%
    count(City) %>%
    arrange(desc(n))
  
  ggplot(layoff_counts, aes(x = n, y = reorder(City, n))) +   #create a horizontal bar plot with geom_text
    geom_bar(stat = "identity", fill = "darkgreen") +
    geom_text(aes(label = n), hjust = -0.2, size = 3.5) +  #add geom_text for displaying count values
    labs(x = "Layoff Count", y = "City") +
    ggtitle("Layoff Count by City") +
    theme_grey()
  
#Analysis 10: Comparison of the department with most layoffs between ‘Fort Nelson’ and ‘Blue River’.

  layoff_data_fn_br <- c_data %>%  #filter the data for layoffs in Fort Nelson and Blue River cities
    filter(Termination_Reason == "Layoff" & (City == "Fort Nelson" | City == "Blue River"))  
  
  layoff_counts_fn_br <- layoff_data_fn_br %>%   #calculate the layoff count by department and city
    group_by(Department, City) %>%
    summarise(Layoff_Count = n())
  

  ggplot(layoff_counts_fn_br, aes(x = Department, y = Layoff_Count, fill = City)) +    #plot the stacked bar chart for layoffs in Fort Nelson and Blue River
    geom_bar(stat = "identity") +
    labs(x = "Department", y = "Layoff Count", fill = "City") +
    ggtitle("Layoffs Comparison: Fort Nelson vs Blue River") +
    theme_dark() 
  
  
  
#Analysis 11: Gender bias of employees’ layoff in stores.
  
  layoff_data <- c_data %>%   #filter the data for layoffs
    filter(Termination_Reason == "Layoff")
  
  layoff_counts <- layoff_data %>%   #calculate the count of layoffs for each gender within each store
    group_by(Store, Gender) %>%
    summarise(Layoff_Count = n())
  
  ggplot(layoff_counts, aes(x = Store, y = Layoff_Count, fill = Gender)) +   #create the bar plot
    geom_col(position = "stack") +
    labs(x = "Store", y = "Layoff Count", fill = "Gender") +
    ggtitle("Gender Bias in Employee Layoffs by Store (Bar Plot)") +
    theme_minimal()
  
#Analysis 12: Gender Bias in employees' layoffs by department.

  layoff_data <- c_data %>%   #calculate the count of layoffs by gender and department
    filter(Termination_Reason == "Layoff") %>%
    group_by(Department, Gender) %>%
    summarise(Layoff_Count = n())
  
  total_layoffs <- layoff_data %>%   #calculate the total count of layoffs in each department
    group_by(Department) %>%
    summarise(Total_Count = sum(Layoff_Count))
  
  layoff_data <- layoff_data %>%   #calculate the percentage of layoffs by gender within each department
    left_join(total_layoffs, by = "Department") %>%
    mutate(Percentage = Layoff_Count / Total_Count * 100)

  ggplot(layoff_data, aes(x = "", y = Percentage, fill = Gender)) +   #create the donut chart
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    facet_wrap(~ Department) +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(title = "Gender Bias in Employees' Layoffs by Department", fill = "Gender") +
    geom_text(aes(label = paste0(round(Percentage), "%")), position = position_stack(vjust = 0.5), color = "black", size = 4)
  
  
  
 #Analysis 13: Gender Bias in employees' layoffs by business unit.
  
  layoffs <- c_data %>% filter(Termination_Reason == "Layoff")   #filter data for layoffs
  
  gender_layoff_counts <- layoffs %>%   #calculate the count of employees laid off by gender within each business unit
    group_by(Business_Unit, Gender) %>%
    summarise(Count = n()) %>%
    ungroup()
  
  total_counts <- gender_layoff_counts %>%   #calculate the total count of employees laid off within each business unit
    group_by(Business_Unit) %>%
    summarise(Total_Count = sum(Count))
  
  gender_layoff_counts <- gender_layoff_counts %>%   #calculate the percentage of employees laid off by gender within each business unit
    left_join(total_counts, by = "Business_Unit") %>%
    mutate(Percentage = round(Count / Total_Count * 100, 2)) 
    
  ggplot(gender_layoff_counts, aes(x = "", y = Count, fill = Gender)) +    #create separate pie charts for each business unit with labeled units
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    facet_wrap(~ Business_Unit, ncol = 2) +
    labs(title = "Gender Bias of Employees' Layoff in Business Unit",
         fill = "Gender", x = NULL, y = NULL) +
    theme_void() +
    theme(legend.position = "bottom") +
    geom_text(aes(label = paste(Percentage, "%")),
              position = position_stack(vjust = 0.5), color = "black", size = 4)
  
#Analysis 14: Total employees in head office categorized by gender. 
  head_office_data <- c_data %>%
    filter(Business_Unit == "HEADOFFICE") %>%
    group_by(Gender) %>%
    summarise(Total_Employees = n())
  
  print(head_office_data)

#Question 3: Do the employees enjoy their jobs
#Analysis 1: Count of employees in each department that retired and resigned.
  
  bar_data <- c_data %>%
    filter(Termination_Reason %in% c("Retirement", "Resignation")) %>%   #filter the data for "Retirement" and "Resignation" termination reasons

    group_by(Department, Termination_Reason) %>%
    summarise(Employee_Count = n())
  
  ggplot(bar_data, aes(x = Department, y = Employee_Count, fill = Termination_Reason)) +   #create a bar chart with dodged bars based on Termination_Reason
    geom_col(position = "dodge") +  # Use geom_col for bar chart
    labs(title = "Count of Retirements and Resignations by Department",
         x = "Department", y = "Count", fill = "Termination Reason") +  #set plot labels
    theme_minimal() +  
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1))     #rotate x-axis titles to prevent overlap
  
  
#Analysis 2: Count of employees of each job titles that retired and resigned.
  retirement_resignation_data <- c_data %>%  #filter the data for retirement and resignation
    filter(Termination_Reason %in% c("Retirement", "Resignation")) %>%
    group_by(Job_Title, Termination_Reason) %>%
    summarise(Count = n())
  
  ggplot(retirement_resignation_data) +   #create the lollipop chart
    geom_segment(aes(x = Job_Title, xend = Job_Title, y = 0, yend = Count),
                 color = "gray", linewidth = 1) +
    geom_point(aes(x = Job_Title, y = Count, fill = Termination_Reason),
               shape = 21, size = 4, color = "black") +
    geom_text(aes(x = Job_Title, y = Count, label = Count),
              vjust = -0.5, color = "black") +
    labs(title = "Count of Retirement and Resignation by Job Title", x = "Job Title", y = "Count", fill = "Termination Reason") +
    scale_fill_manual(values = c("Retirement" = "blue", "Resignation" = "red")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
#Analysis 3: Count of employees of each store that retired and resigned.
  retirement_resignation_data <- c_data %>%   #calculate the count of retirement and resignation by store
    filter(Termination_Reason %in% c("Retirement", "Resignation")) %>%
    group_by(Store, Termination_Reason) %>%
    summarise(Employee_Count = n())
  
  ggplot(retirement_resignation_data, aes(x = Employee_Count, y = Store, fill = Termination_Reason)) +   #create the stacked bar plot
    geom_col(position = "stack") +
    geom_text(aes(label = Employee_Count), position = position_stack(vjust = 0.5), color = "black") +
    labs(title = "Count of Employees Retired and Resigned by Store",
         x = "Count",
         y = "Store",
         fill = "Termination Reason") +
    theme_minimal()
  
#Analysis 4: Count of employees of each city that retired and resigned.
  retirement_resignation_data <- c_data %>%    #calculate the count of retirement and resignation by city
    filter(Termination_Reason %in% c("Retirement", "Resignation")) %>%
    group_by(City, Termination_Reason) %>%
    summarise(Employee_Count = n())
  
  line_data <- retirement_resignation_data %>%    #pivot the data to wide format for stacked line chart
    pivot_wider(names_from = Termination_Reason, values_from = Employee_Count, values_fill = 0)
  
  ggplot(line_data, aes(x = City, group = 1)) +  #create the stacked line chart with rotated axis titles
    geom_area(aes(y = Retirement, fill = "Retirement"), position = "stack", alpha = 0.7, color = "black") +
    geom_area(aes(y = Resignation, fill = "Resignation"), position = "stack", alpha = 0.7, color = "black") +
    scale_fill_manual(values = c(Retirement = "#87CEEB", Resignation = "#FF7F50")) +
    labs(title = "Count of Employees by City (Retirement vs Resignation)",
         x = "City", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
#Analysis 5: Number of stores in Vancouver compared to every other city.
  vancouver_stores <- c_data[c_data$City == "Vancouver", ]   #filter the data for stores in Vancouver
  
  vancouver_count <- nrow(vancouver_stores)   #calculate the number of stores in Vancouver

  other_cities_counts <- table(c_data$City)[-which(names(table(c_data$City)) == "Vancouver")]    #calculate the number of stores in every other city
  
  store_counts <- data.frame(    #create a data frame for the store counts
    City = c("Vancouver", names(other_cities_counts)),
    Count = c(vancouver_count, other_cities_counts)
  )
  
  ggplot(store_counts, aes(x = City, y = Count, fill = City)) +    #create the bar plot
    geom_bar(stat = "identity") +
    geom_text(aes(label = Count), vjust = -0.5, size = 4) +
    labs(title = "Number of Stores in Vancouver vs Other Cities",
         x = "City",
         y = "Store Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  
  
#Analysis 6: Count of male and female employees who retired in each department of Store 35
  retired_employees <- c_data[c_data$Store == "35" & c_data$Termination_Reason == "Retirement", ]
  retired_counts <- table(retired_employees$Department, retired_employees$Gender)
  retired_data <- as.data.frame(retired_counts)
  
  ggplot(retired_data, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = Freq), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
    labs(title = "Count of Male and Female Employees Retired in Each Department (Store 35)",
         x = "Department", y = "Count", fill = "Gender") +
    scale_fill_manual(values = c("Male" = "lightblue", "Female" = "pink")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
#Analysis 7: Count of male and female employees who retired in each department of Store 37.
  retired_employees <- c_data[c_data$Store == "37" & c_data$Termination_Reason == "Retirement", ]
  retired_counts <- table(retired_employees$Department, retired_employees$Gender)
  retired_data <- as.data.frame(retired_counts)
  
  ggplot(retired_data, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = Freq), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
    labs(title = "Count of Male and Female Employees Retired in Each Department (Store 37)",
         x = "Department", y = "Count", fill = "Gender") +
    scale_fill_manual(values = c("Male" = "royalblue", "Female" = "magenta")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
#Question 4: Which store is being underutilized?
#Analysis 1: The number of employees of each department in the organization.

  department_counts <- table(c_data$Department)   #calculate the number of employees in each department
  
  bubble_data <- data.frame(     #create a data frame for the bubble chart
    Department = names(department_counts),
    Count = as.numeric(as.character(department_counts))
  )
  
  
  bubble_data$Size <- sqrt(bubble_data$Count)    #set the size of the bubbles based on the employee count
  
  ggplot(bubble_data, aes(x = Department, y = Count, size = Size)) +     #create the bubble chart
    geom_point(color = "darkblue", alpha = 0.7) +
    labs(title = "Number of Employees by Department") +
    xlab("Department") +
    ylab("Count") +
    scale_size_continuous(name = "Employee Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
#Analysis 2: The number of employees of each store in the organization.
  store_counts <- table(c_data$Store)     #calculate the number of employees in each store
   
  store_data <- data.frame(     #create a data frame for the store employee counts
    Store = names(store_counts),
    Employee_Count = as.numeric(as.character(store_counts))
  )
  
  store_data$Store <- as.numeric(store_data$Store)  #convert Store column to numeric for sorting
  
  store_data <- store_data[order(store_data$Store), ]   #sort the data frame by Store column in ascending order
  
  ggplot(store_data, aes(x = Store, y = Employee_Count)) +     #create a bar plot to visualize the number of employees by store
    geom_bar(stat = "identity", fill = "darkred", alpha = 0.7) +
    scale_x_continuous(breaks = store_data$Store, labels = paste0("Store ", store_data$Store)) +
    labs(title = "Number of Employees by Store", x = "Store", y = "Employee Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
#Analysis 3: Which department is Store 46 from.
  store_46_department <- unique(c_data$Department[c_data$Store == "46"])
  store_46_table <- data.frame(Store = "Store 46", Department = store_46_department)
  View(store_46_table)
  
#Analysis 4: Average length of service for each store.
  store_length_of_service <- c_data %>%   #calculate the length of service for each store
    group_by(Store) %>%
    summarise(Average_Length_of_Service = mean(Length_of_Service, na.rm = TRUE))
  
  ggplot(c_data, aes(x = Store, y = Length_of_Service, fill = Store)) +   #create a box plot with different colors for each box
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = rainbow(length(unique(c_data$Store)))) +
    labs(title = "Length of Service by Store", x = "Store", y = "Length of Service") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
#Analysis 5: Number of active employees for each store in 2015.
  data_2015 <- c_data[c_data$Status_Year == 2015 & c_data$Status == "ACTIVE", ] # Filter the data for the year 2015 and for active employees

  active_employees <- table(data_2015$Store)    #count the number of active employees for each store
  
  store_data <- data.frame(Store = names(active_employees), Active_Employees = as.numeric(active_employees))   #create a data frame for the store and active employee counts
  store_data$Store <- factor(store_data$Store, levels = unique(store_data$Store))
  
  ggplot(store_data, aes(x = Store, y = Active_Employees)) +     #create a bar plot to visualize the relationship between store and active employees in 2015
    geom_bar(stat = "identity", fill = "lemonchiffon4", alpha = 0.7) +
    labs(title = "Number of Active Employees by Store (2015)", x = "Store", y = "Active Employees") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
#Analysis 6: Number of resignations for each store in 2015.
  resignations_2015 <- c_data %>%   #filter the data for resignations in 2015
    filter(Termination_Reason == "Resignation", year(Date_of_Termination) == 2015)
  
  resignations_count <- resignations_2015 %>%    #calculate the number of resignations per store
    group_by(Store) %>%
    summarise(Resignation_Count = n())
  
  resignations_count <- resignations_count[order(resignations_count$Resignation_Count), ]   #sort the stores by ascending order of resignation count
  
  ggplot(resignations_count, aes(x = Resignation_Count, y = Store)) +    #create a horizontal lollipop graph of resignations by store
    geom_segment(aes(xend = 0, yend = Store), color = "black") +
    geom_point(color = "aquamarine4", size = 3, fill = "white") +
    labs(title = "Number of Resignations by Store (2015)", x = "Resignation Count", y = "Store") +
    theme_minimal() +
    theme(axis.text.y = element_text(hjust = 0))
  
#Analysis 7: Average age of employees for each store in 2015.
  active_employees_2015 <- c_data[c_data$Status == "ACTIVE" & c_data$Status_Year == 2015, ]   #filter the data for active employees in the year 2015

  store_age <- active_employees_2015 %>%      #calculate the average age of active employees for each store
    group_by(Store) %>%
    summarise(Average_Age = mean(Age, na.rm = TRUE))
  
  ggplot(store_age, aes(x = Average_Age, y = reorder(Store, -Average_Age))) +     #create a horizontal bar plot of the average age of active employees for each store
    geom_bar(stat = "identity", fill = "turquoise4", alpha = 0.7) +
    labs(title = "Average Age of Active Employees in 2015", x = "Average Age", y = "Store") +
    theme_minimal() +
    theme(axis.text.y = element_text(hjust = 0)) +
    geom_text(aes(label = round(Average_Age, 1)), hjust = -0.1, color = "black")
  
  
  
#Question 5: Is the organization financially stable in 2015?
#Analysis 1: Number of employees over the years 
  employee_counts <- c_data %>%   #calculate the number of employees for each year
    group_by(Status_Year) %>%
    summarise(Employee_Count = n())
  
  ggplot(employee_counts, aes(x = Status_Year, y = Employee_Count)) +   #create a scatter plot of the number of employees every year with text labels
    geom_point() +
    geom_text(aes(label = Employee_Count), vjust = -1.5) +
    labs(title = "Number of Employees Over the Years", x = "Year", y = "Employee Count") +
    theme_minimal()
  
#Analysis 2: Average length of service over the years.
  store_length_of_service <- c_data %>%     #subset the data for average length of service in each store for every year
    group_by(Store, Status_Year) %>%
    summarise(Average_Length_of_Service = mean(Length_of_Service, na.rm = TRUE))
  
  ggplot(store_length_of_service, aes(x = Status_Year, y = Average_Length_of_Service)) +
    geom_violin(fill = "darkslategray4", color = "darkslategray", alpha = 0.7) +
    geom_boxplot(width = 0.1, fill = "white", color = "black") +
    labs(title = "Average Length of Service in Each Store",
         x = "Year", y = "Average Length of Service") +
    theme_minimal()
  
  
#Analysis 3: Number of employees that resigned over the years.
    resignations <- c_data[c_data$Termination_Reason == "Resignation", ]  #filter the data for resignations
    
    resignation_counts <- with(resignations, table(Store, Status_Year))    #calculate the number of resignations in each store and year
  
    resignation_data <- as.data.frame(resignation_counts)   #convert the table to a data frame
    
    ggplot(resignation_data, aes(x = Status_Year, y = Store, fill = Freq)) +   #create a heatmap
      geom_tile() +
      scale_fill_viridis_c(name = "Resignation Count") +
      labs(title = "Amount of Resignation in Every Store Over the Years", x = "Year", y = "Store") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
   
#Analysis 4: Number of active employees over the years.
    active_employees <- c_data[c_data$Status == "ACTIVE", ]  #filter the data for active employees
    
    store_employee_counts <- with(active_employees, table(Status_Year, Store))  #calculate the number of active employees in each store and year
    
    store_employee_data <- as.data.frame(store_employee_counts)  #convert the table to a data frame
    
    ggplot(store_employee_data, aes(x = Status_Year, y = Freq, fill = Store)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Number of Active Employees in Each Store Over the Years", x = "Year", y = "Number of Active Employees", fill = "Store") +
      scale_fill_viridis_d(option = "C") +
      theme_minimal() +
      theme(legend.position = "bottom")
  
    
#Analysis 5: Job titles of the employees that were laid off over the years.
    laid_off_employees <- c_data[c_data$Termination_Reason == "Layoff", ]  #filter the data for laid-off employees
    
    ggplot(laid_off_employees, aes(x = Status_Year, fill = Job_Title)) +
      geom_bar() +
      labs(title = "Jobs of Laid-Off Employees Over the Years", x = "Year", y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_viridis_d() 
  
#Analysis 6: Number of employees laid off in each store over the years.
    layoff_counts <- c_data %>%
      filter(Termination_Reason == "Layoff") %>%
      count(Store, Status_Year) %>%
      arrange(Status_Year, Store)
    
    ggplot(layoff_counts, aes(x = Status_Year, y = n, fill = as.factor(Store))) +
      geom_col(position = "dodge", width = 0.8, color = "black") +
      geom_text(aes(label = n), position = position_dodge(width = 0.8), vjust = -0.5, color = "black", size = 3) +
      labs(title = "Amount of Employees Laid Off in Each Store Over the Years",
           x = "Year", y = "Count", fill = "Store") +
      theme_minimal()
    

#Analysis 7: Number of employees in both 'STORES' and 'HEADOFFICE' over the years.
    employee_counts <- c_data %>%
      filter(Business_Unit %in% c('STORES', 'HEADOFFICE')) %>%
      group_by(Status_Year, Business_Unit) %>%
      summarise(Employee_Count = n()) %>%
      arrange(Status_Year, Business_Unit)
    
    ggplot(employee_counts, aes(x = Employee_Count, y = factor(Status_Year), fill = Business_Unit)) +
      geom_col() +
      labs(title = "Number of Employees in STORES and HEADOFFICE Over the Years",
           x = "Employee Count",
           y = "Year") +
      theme_minimal() +
      theme(axis.text.y = element_text(hjust = 0))
    

    
#Analysis 8: Age of laid off employees over the years.  
    laid_off_employees <- c_data[c_data$Termination_Reason == "Layoff", ]  # Filter the data for laid-off employees
    
    ggplot(laid_off_employees, aes(x = Status_Year, y = Age)) +
      geom_boxplot() +
      labs(title = "Age of Laid-Off Employees Over the Years", x = "Year", y = "Age") +
      theme_minimal()
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  