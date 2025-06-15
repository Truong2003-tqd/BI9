

#Duplication Check Script
Competitor_Database %>% 
  mutate(is_duplicated = duplicated(`No#`)) %>%
  summarise(
    "Duplicated count" = sum(is_duplicated),
    "Row count" = n()
  )

#Split column script
Segmentation2017 %>%
  extract(Segmentation, 
          into = c("SegmentCode", "SegmentName", "SpendingRange"), 
          regex = "(Seg\\.\\d{2}) - (.+) \\((.+)\\)")

#Convert all to "Other" script
Brand_Image <- Brand_Image %>%
  mutate(BrandImage = case_when(
    str_detect(BrandImage, regex("other", ignore_case = TRUE)) ~ "Other",
    str_detect(BrandImage, regex("street", ignore_case = TRUE)) ~ "Street",
    TRUE ~ BrandImage
        ),
         Awareness = case_when(
           str_detect(Awareness, regex("other", ignore_case = TRUE)) ~ "Other",
           str_detect(Awareness, regex("street", ignore_case = TRUE)) ~ "Street",
           TRUE ~ BrandImage
         ))

#Standardize variable script script
Brand_Health %>%
  mutate(Comprehension = case_when(
    Comprehension == "Do not know it at all"     ~ "Do not know",
    Comprehension == "Maybe do not know it"      ~ "Unsure",
    Comprehension == "Know a little"             ~ "Know a little",
    Comprehension == "Know it well"              ~ "Know well",
    Comprehension == "Know it very well"         ~ "Know very well",
    TRUE                                         ~ Comprehension
  )) 

#Standardize DayPart
DayPart %>%
  mutate(Daypart = case_when(
    Daypart == "Before 9 AM"               ~ "05:00–08:59",
    Daypart == "9 AM - before 11 AM"       ~ "09:00–10:59",
    Daypart == "11 AM - before 2 PM"       ~ "11:00–13:59",
    Daypart == "2 PM - before 5 PM"        ~ "14:00–16:59",
    Daypart == "5 PM - before 9 PM"        ~ "17:00–20:59",
    Daypart == "9 PM or later"             ~ "21:00–23:59",
    TRUE                                   ~ Daypart
  )) 
  
DayPart %>%
  mutate(DayPartNumericCode = case_when(
    Daypart == "Before 9 AM"               ~ "1",
    Daypart == "9 AM - before 11 AM"       ~ "2",
    Daypart == "11 AM - before 2 PM"       ~ "3",
    Daypart == "2 PM - before 5 PM"        ~ "4",
    Daypart == "5 PM - before 9 PM"        ~ "5",
    Daypart == "9 PM or later"             ~ "6",
    TRUE                                   ~ Daypart
  )) 


#Standardize the MPI#detail
SA_var %>% 
  mutate(`MPI#detail` = case_when(
    str_detect(`MPI#detail`, "Under\\s+3") ~ "< 3M",
    `MPI#detail` == "Refuse"               ~ "Refuse",
    is.na(`MPI#detail`)                    ~ NA,
    TRUE ~ str_extract_all(`MPI#detail`, "\\d+\\.?\\d*") %>%
      lapply(function(x) paste0(x[1], "M – ", x[2], "M")) %>%
      unlist()
  )) %>% 
  distinct(`MPI#detail`)
      
#Standardize the MPI
SA_var %>% 
  mutate(MPI = case_when(
    MPI == "VND 25m+"             ~ "> 25M",
    is.na(MPI)                    ~ NA,
    TRUE                          ~ str_extract_all(MPI, "\\d+\\.?\\d*") %>%
      lapply(function(x) paste0(x[1], "M – ", x[2], "M")) %>%
      unlist()
  )) %>% 
  distinct(MPI)


#Standardize the MPI#2
SA_var %>%
  mutate(`MPI#2` = case_when(
    is.na(`MPI#2`) ~ NA,
    TRUE ~ str_extract(`MPI#2`, "^\\d+")
  )) %>%
  distinct(`MPI#2`)

#Remove text in ()
SA_var <- SA_var %>%
  mutate(Occupation = str_remove(Occupation, "\\s*\\([^\\)]+\\)")) 

#Remove y.o
SA_var %>% 
  mutate(`Age#Group#2` = case_when(
    `Age#Group#2` == "45+ y.o."             ~ "> 45",
    is.na(`Age#Group#2`)                    ~ NA,
    TRUE                          ~ str_extract_all(`Age#Group#2`, "\\d+") %>%
      lapply(function(x) paste0(x[1], " – ", x[2])) %>%
      unlist()
  )) %>% 
  distinct(`Age#Group#2`)

#Export file
write.csv(Segmentation2017, "Segmentation2017.csv", row.names = FALSE)
write.csv(Brand_Image, "Brand_Image.csv", row.names = FALSE)
write.csv(Brand_Health, "Brand_Health.csv", row.names = FALSE)
write.csv(Companion, "Companion.csv", row.names = FALSE)
write.csv(Competitor_Database, "Competitor_Database.csv", row.names = FALSE)
write.csv(DayofWeek, "DayofWeek.csv", row.names = FALSE)
write.csv(DayPart, "DayPart.csv", row.names = FALSE)
write.csv(NeedstateDayDaypart, "Need_State.csv", row.names = FALSE)
write.csv(SA_var, "SA_var.csv", row.names = FALSE)



SA_var %>% 
  filter(is.na(`MPI#detail`)) %>% 
  count(Occupation) %>% 
  gt()

SA_var %>% 
  mutate(is_missing = is.na(`MPI#detail`) | `MPI#detail` == "Refuse") %>% 
  group_by(Occupation) %>% 
  summarise(
    count_missing_MPI = sum(is_missing),
    count_all = n(),
    missing_percentage = (sum(is_missing)/n())*100,
    .groups = "drop"
  ) %>%
  ggplot(aes(x = count_missing_MPI, y = Occupation))+
  geom_bar(stat = "identity", position = "stack")+
  theme_minimal()
  
#Convert "Refuse" in MPI#detail to NA
SA_var <- SA_var %>% 
  kNN(variable = "MPI#detail",
      k = 9) 

SA_var %>% 
  mutate(MPI = case_when(
    `MPI#detail` %in% c("< 3M","3M – 4.49M") ~ "< 4.5M",
    `MPI#detail` %in% c("4.5M – 6.49M","6.5M – 7.49M","7.5M – 8.99M") ~ "4.5M – 8.9M",
    `MPI#detail` %in% c("9M – 11.99M","12M – 14.99M") ~ "9M – 14.9M",
    `MPI#detail` %in% c("20M – 24.99M","15M – 19.99M") ~ "15M – 24.9M",
    `MPI#detail` %in% c("25M – 29.99M","30M – 44.99M","45M – 74.99M","75M – 149.99M") ~ "> 25M"
  ),
    `MPI#2` = case_when(
     MPI == "< 4.5M" ~ "1",
     MPI == "4.5M – 8.9M" ~ "2",
     MPI == "9M – 14.9M" ~ "3",
     MPI == "15M – 24.9M" ~ "4",
     MPI == "> 25M" ~ "5"
  ),
    `MPI#Mean` = case_when(
      
    )) %>% 
    View()


