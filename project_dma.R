install.packages("RMySQL")
install.packages("RSQLServer")

library(DBI)
RMySQL::dbConnect("MySQLConnection")
RMySQL::dbConnect("MySQLDriver")


Database <- DBI::dbConnect(dbDriver("MySQL"), user = 'root', password = '12345',
                      dbname = 'sys' , host = 'localhost')

dbListTables(Database)
dbListFields(Database, 'sys_config' )

#=====================test 2==============================================

Data_Project <- DBI::dbConnect(dbDriver("MySQL"), user = 'root', password = '12345',
                           dbname = 'group_12' , host = 'localhost')

dbListTables(Data_Project)
dbListFields(Data_Project, 'doctors')


doctors_data <- dbSendQuery(Data_Project, "SELECT * 
from doctors")
my_doctor_data <- dbFetch(doctors_data)

nurses_data <- dbSendQuery(Data_Project, "SELECT * 
from nurses")
my_nurse_data <- dbFetch(nurses_data)

admins_data <- dbSendQuery(Data_Project, "SELECT * 
from admins")
my_admins_data <- dbFetch(admins_data)

appoinments_data <- dbSendQuery(Data_Project, "SELECT * 
from appoinments")
my_appoin_data <- dbFetch(appoinments_data)

patients_data <- dbSendQuery(Data_Project, "SELECT * 
from patients")
my_patients_data <- dbFetch(patients_data)

pharmacy_data <- dbSendQuery(Data_Project, "SELECT * 
from pharmacy")
my_pharmacy_data <- dbFetch(pharmacy_data)

prescriptions_data <- dbSendQuery(Data_Project, "SELECT * 
from prescriptions")
my_prescriptions_data <- dbFetch(prescriptions_data)



#===================VIS======================================

library(ggplot2)
library(dplyr)

#1. Create Data
data <- data.frame(
  group=LETTERS[1:5],
  value=c(13,7,9,21,2)
)

#show the distribution per doctor's expertise

doc_expertise <- my_doctor_data %>% 
  group_by(EXPERTISE) %>% 
  summarise(sum = n()) %>% 
  arrange(desc(sum))
#just use tableau for this data


#2. Show the range of salary in doctor using pie chart
library(ggplot2)
doctor_combine <- data.frame(cbind(my_doctor_data,Salary$Salary))

doctor_combine <- doctor_combine %>% 
  mutate(Range = ifelse(Salary.Salary %in% 70000:84999,"70000 - 84999", 
                         ifelse(Salary.Salary %in% 85000:94999,"85000 -  94999",
                                ifelse(Salary.Salary %in% 95000:100000,"95000 - 100000","no"))))

doctor_combine_n <- doctor_combine %>% 
  group_by(Range) %>% 
  summarise(count = n())

ggplot(doctor_combine_n, aes(x="", y=count, fill=Range)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() 

#3. Show the number of patients  visit each year
library(purrr)
library(ggplot2)
library
df_fm <- my_appoin_data %>% 
  mutate(year = map(strsplit(my_appoin_data$DATE, split = "/"),3))

df_fm$year <- as.character(df_fm$year)

df_fm1 <- df_fm %>% group_by(year) %>% 
summarise(count=n()) %>% 
  arrange(desc(year))

ggplot(df_fm1, aes(year, count)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_bw() +
  labs(x = 'Year', y = 'Number of patient') +
  theme(plot.title = element_text(hjust = 0.5)) 


df <- my_appoin_data %>% 
  mutate(year = map(strsplit(my_appoin_data$DATE, split = "/"),3),
         month = map(strsplit(my_appoin_data$DATE, split = "/"),2))

df$year <- as.character(df$year)
df$month <- as.character(df$month)

df1 <- df %>%filter(year=="17") 

a<- df1 %>% group_by(month) %>% 
  summarise(count=n()) %>% 
  arrange(desc(month))

ggplot(data=a, aes(x=month, y=count, group=1)) +
  geom_line()+
  geom_point() + labs(x = 'Month', y = 'Number of patient')

ggplot(df_fm1, aes(year, count)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_bw() +
  labs(x = 'Year', y = 'Number of patient') +
  theme(plot.title = element_text(hjust = 0.5)) 

a <- my_prescriptions_data %>% 
  group_by(drug_id) %>% 
  summarise(count=n())
