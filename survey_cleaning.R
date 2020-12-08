#Loading data from local machine
survey <- readxl::read_excel("/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/Survey.Starting August 31, 2014.Ending August 31, 2020.xlsx")

#Renaming column for convenience
survey <- survey %>%
  rename(
    staff_and_schedule = `Staff or Resource`,
    survey_date = `Survey Date`,
    rating = `I would rate this session (S31)`,
    will_return = `I will return to the center (S32)`,
    will_recommend = `I will recommend the center (S33)`,
    goal_response = `What were your goals for the session and did you accomplish these goals? (S39)`,
    comments = `Any comments or suggestions? (S40)`,
    how_heard = `How did you hear about the Writing Center? (S36)`,
    suggested_appt_times = `Are there any times that you wish the Writing Center offered appointments that we do not currently have on our schedule? Are there other times that you prefer but slots are filled before you can get a (S37)`
  )

#Quantifying the rating scale given by myWCOnline's raw data output
survey <-  
  mutate(survey, rating_num = case_when(
    rating == 'Excellent' ~ 5,
    rating == 'Very Good' ~ 4,
    rating == 'Good' ~ 3,
    rating == 'Fair' ~ 2,
    rating == 'Poor' ~ 1,
    rating == 'Unacceptable' ~ 0
  ))

#Dropping unnecessary columns
survey <- survey %>%
  select(-c("rating"))

survey <- survey %>% relocate("rating_num", .after = "survey_date")
survey$survey_date <- as.Date(survey$survey_date)







