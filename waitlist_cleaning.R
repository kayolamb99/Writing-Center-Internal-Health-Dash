#Loading Data From local machine
waitlist <- readxl::read_excel("/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/Waiting List.Starting August 31, 2014.Ending August 31, 2020.xlsx")

#Renaming columns for convenience
waitlist <- waitlist %>% 
  unite(name, c(`First Name`,`Last Name`), sep = " ")

waitlist <- waitlist %>%
  rename (
    netID = `NetID (R6)`,
    grad_year = `Graduation Year (R3)`,
    standing = `Standing (R2)`,
    school = `School (R7)`,
    first_language = `First Language (R4)`,
    major = `Major? (R8)`,
    reg_date = `Registration Date`,
    last_update = `Last Profile Update`,
    last_login = `Last Login`,
    admin_reinstated = `Administrative Notes`,
    waitlist_date = `Waiting List Date`,
    date_added = `Date & Time Added`,
    contact_method = `Contact Method`,
    schedule = `Schedule Title`,
    tutor_wanted = `Restriction: Staff or Resource`,
    desired_start_by = `Restriction: Starting Time`,
    desired_end_by = `Restriction: Ending Time`
  )

registrations <- registrations %>%
  select(-c(`Email Address`))

#Making time columns conform to the Time and Date data types respectively
waitlist$desired_start_by <-strptime(waitlist$desired_start_by, format = '%I:%M%p')
waitlist$desired_start_by <- substr(waitlist$desired_start_by, 11, str_length(waitlist$desired_start_by))
waitlist$desired_start_by <- substr(waitlist$desired_start_by, 2, str_length(waitlist$desired_start_by))

waitlist$desired_end_by <-strptime(waitlist$desired_end_by, format = '%I:%M%p')
waitlist$desired_end_by <- substr(waitlist$desired_end_by, 12, str_length(waitlist$desired_end_by))

waitlist$desired_start_by <- chron(times = waitlist$desired_start_by)
waitlist$desired_end_by <- chron(times = waitlist$desired_end_by)

waitlist$window <- waitlist$desired_end_by - waitlist$desired_start_by

waitlist <- waitlist %>%
  rename(
    desired_date = waitlist_date
  )

waitlist <- separate(data = waitlist, col = date_added, into = c("date_added", "time_added"), sep = ",")
waitlist <- separate(data = waitlist, col = time_added, into = c("temp", "time"), sep = 5)
waitlist <- waitlist %>% 
  unite(date_created, c(date_added,temp), sep = "")
waitlist <- waitlist %>%
  rename(time_added = time_created)

waitlist$time_added <- substr(waitlist$time_added, 2, str_length(waitlist$time_added))
waitlist$time_added <-strptime(waitlist$time_added, format = '%H:%M:%S%p')
waitlist$time_added <- substr(waitlist$time_added, 11, str_length(waitlist$time_added))
waitlist$time_added <- substr(waitlist$time_added, 2, str_length(waitlist$time_added))
waitlist$time_added <- chron(times = waitlist$time_added)

waitlist$desired_date <- as.Date(waitlist$desired_date, format = "%B %d, %Y")
waitlist$date_created <- as.Date(waitlist$date_created, format = "%B %d %Y")

#Addition of a feature called 'waitlist gap' which quantifies how long it took a client to get off the waitlist
waitlist$waitlist_gap <- waitlist$desired_date - waitlist$date_created
waitlist <- waitlist %>% relocate("waitlist_gap", .after = "date_created")
waitlist <- waitlist %>% rename(date_added = date_created)
waitlist <- waitlist %>% select(-c(`Email Address`))

#Creation of a 'current year' column, iterating on myWCOnline's static class year classification into one that dynamially updates as time goes on
waitlist$grad_year <- as.numeric(waitlist$grad_year)
waitlist$grad_diff <- waitlist$grad_year - as.numeric(substr(waitlist$schedule, str_length(waitlist$schedule) - 4 ,str_length(waitlist$schedule)))
waitlist <- waitlist %>%
  mutate(current_year = 
           case_when (
             ((str_detect(schedule, "Fall") | str_detect(schedule, "Summer")) & (str_detect(standing, regex("U", ignore_case = FALSE))) & grad_diff == 4) ~ 'First Year',
             ((str_detect(schedule, "Fall") | str_detect(schedule, "Summer")) & (str_detect(standing, regex("U", ignore_case = FALSE))) & grad_diff == 3) ~ 'Sophomore',
             ((str_detect(schedule, "Fall") | str_detect(schedule, "Summer")) & (str_detect(standing, regex("U", ignore_case = FALSE))) & grad_diff == 2) ~ 'Junior',
             ((str_detect(schedule, "Fall") | str_detect(schedule, "Summer")) & (str_detect(standing, regex("U", ignore_case = FALSE))) & grad_diff == 1) ~ 'Senior',
             (str_detect(schedule, "Spring") & str_detect(standing, regex("U", ignore_case = FALSE)) & grad_diff == 3) ~ 'First Year',
             (str_detect(schedule, "Spring") & str_detect(standing, regex("U", ignore_case = FALSE)) & grad_diff == 2) ~ 'Sophomore',
             (str_detect(schedule, "Spring") & str_detect(standing, regex("U", ignore_case = FALSE)) & grad_diff == 1) ~ 'Junior',
             (str_detect(schedule, "Spring") & str_detect(standing, regex("U", ignore_case = FALSE)) & grad_diff == 0) ~ 'Senior'
           )
  )

waitlist <- waitlist %>% relocate("current_year", .after = "standing")