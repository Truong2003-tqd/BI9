

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


SA_var %>% 
  mutate(`MPI#2` = case_when(
    is.na(`MPI#2`)                    ~ NA,
    TRUE                              ~ str_extract_all(`MPI#2`, "^\\d+")
  )) %>% 
  distinct(`MPI#2`)

SA_var %>%
  mutate(`MPI#2` = case_when(
    is.na(`MPI#2`) ~ NA_character_,
    TRUE ~ str_extract(`MPI#2`, "^\\d+")
  )) %>%
  distinct(`MPI#2`)

    



