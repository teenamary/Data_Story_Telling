crime=read.csv('C://Users//admin//Desktop//crime.csv')

# Handling the Missing datasets
sort(sapply(crime, function(x) sum(is.na(x))), decreasing = TRUE)

# Sometimes we need to be carefull when package ask us to remove the missing data.
plot_missing(crime)

# Lets Rename the distrcit code to district name
district_name = c(
  A1 = 'Downtown',
  A15= 'Charlestown',
  A7= 'East Boston',
  B2= 'Roxbury',
  B3= 'Mattapan',
  C6= 'South Boston',
  C11= 'Dorchester',
  D4= 'South End',
  D14= 'Brighton',
  E5= 'West Roxbury',
  E13= 'Jamaica Plain',
  E18= 'Hyde Park')

district_name

crime%>% 
  count(offense_code, sort = TRUE) %>% 
  ggplot(aes (n))+
  geom_histogram()+
  labs(title="Majority of crimes offense code lies below 5000", x="offense_code", y="n")

# We will change all district code to district name by passing the following code.
crime$district <- as.factor(district_name[(crime$district)] )
#unique(crime$district)
sort(table(crime$district), decreasing = TRUE)


p1 <- data.frame(sort(table(shoot$day), decreasing = TRUE))
p1 <- p1 %>% rename(day = Var1)

# Overall Shooting throughout the year
ggplot(data = shoot, aes(x = month)) +
  geom_bar(label = TRUE)+
  ggtitle("Overall Shooting throughout the year")

# Overall Shooting throughout the month
ggplot(data = shoot, aes(x = day)) +
  geom_bar()+
  ggtitle("Overall Shooting throughout the month ")


library("RColorBrewer") 
pal = brewer.pal(9,"Blues")
street_name <- as.tibble(table(crime$street))
colnames(street_name) <- c("Street_Name", "Count")
wordcloud(street_name$Street_Name, street_name$Count, min.freq = 200, random.order = F, random.color = F, colors =c("black", "cornflowerblue", "darkred"), scale = c(2,.3))