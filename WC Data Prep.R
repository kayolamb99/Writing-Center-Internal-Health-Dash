#Data Cleaning


#REPLACE THESE FILEPATHS WITH ONES FROM YOUR LOCAL MACHINE!!!
appointments <- readxl::read_excel("/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/Appointments.Starting August 31, 2014.Ending August 31, 2020.xlsx")
appointments_totals <- readxl::read_excel("/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/Appointment_Condensed.Starting August 31, 2014.Ending August 31, 2020.xlsx")
client_report <- readxl::read_excel("/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/Client Report Forms.Starting August 31, 2014.Ending August 31, 2020.xlsx")
registrations <- readxl::read_excel("/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/Registration.Starting August 31, 2014.Ending August 31, 2020.xlsx")
survey <- readxl::read_excel("/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/Survey.Starting August 31, 2014.Ending August 31, 2020.xlsx")
waitlist <- readxl::read_excel("/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/Waiting List.Starting August 31, 2014.Ending August 31, 2020.xlsx")

#Make sure to install these packages beforehand using the syntax 'install.packages("package_name")'

require(stringr)
require(sqldf)
require(dplyr)
require(tidyverse)
require(chron)
require(naniar)


appointments <- appointments %>%
    rename (
      netID = `NetID (R6)`,
      grad_year = `Graduation Year (R3)`,
      standing = `Standing (R2)`,
      school = `School (R7)`,
      first_language = `First Language (R4)`,
      major = `Major? (R8)`,
      goal_list = `Check all that apply: (A20)`,
      reg_date = `Registration Date`,
      course = `Course or Subject (A11)`,
      instructor = `Instructor (A12)`,
      client_topic_desc = `What would you like to work on today? (A19)`,
      last_login = `Last Login`,
      last_update = `Last Profile Update`,
      tutor = `Staff or Resource Name`,
      appt_date = `Appointment Date`,
      start = `Start Time`,
      end = `End Time`,
      walk_in = `Walk-In/Drop-In`,
      no_show = `Missed/No-Show`,
      date_created = `Created`,
      created_by = `Created By`,
      modify_date = `Modified`,
      is_repeating = `Repeating?`,
      admin_notes = `Administrative Notes`,
      schedule = `Schedule Title`,
      
    )

appointments$reg_date <- as.Date(appointments$reg_date)
appointments$last_update <- as.Date(appointments$last_update)
appointments$last_login <- as.Date(appointments$last_login)
appointments$appt_date <- as.Date(appointments$appt_date)

appointments$start <-strptime(appointments$start, format = '%I:%M%p')
appointments$start <- substr(appointments$start, 12, str_length(appointments$start))
appointments$end <-strptime(appointments$end, format = '%I:%M%p')
appointments$end <- substr(appointments$end, 12, str_length(appointments$end))

appointments <- separate(data = appointments, col = date_created, into = c("date_created", "time_created"), sep = ",")
appointments <- separate(data = appointments, col = time_created, into = c("temp", "time"), sep = 5)
appointments <- appointments %>% 
  unite(date_created, c(date_created,temp), sep = "")
appointments <- appointments %>%
  rename(time_created = time)

appointments <- separate(data = appointments, col = modify_date, into = c("date_modified", "time_modified"), sep = ",")
appointments <- separate(data = appointments, col = time_modified, into = c("temp", "time"), sep = 5)
appointments <- appointments %>% 
  unite(date_modified, c(date_modified,temp), sep = "")
appointments <- appointments %>% 
    replace_with_na(replace = list(date_modified = "NANA"))
appointments <- appointments %>%
  rename(time_modified = time)

appointments <- separate(data = appointments, col = created_by, into = c("first", "last"), sep = ",")
appointments <- appointments %>% 
  unite(modfied_by, c(last,first), sep = " ")
                  

appointments$start <- chron(times = appointments$start)
appointments$end <- chron(times = appointments$end)
appointments$appt_length <- appointments$end - appointments$start

appointments$time_modified <-strptime(appointments$time_modified, format = '%H:%M:%S%p')
appointments$time_modified <- substr(appointments$time_modified, 12, str_length(appointments$time_modified))
appointments$time_modified <- chron(times = appointments$time_modified)

appointments$time_created <- substr(appointments$time_created, 2, str_length(appointments$time_created))
appointments$time_created <-strptime(appointments$time_created, format = '%H:%M:%S%p')
appointments$time_created <- substr(appointments$time_created, 12, str_length(appointments$time_created))
appointments$time_created <- chron(times = appointments$time_created)


appointments <- appointments %>% 
  unite(name, c(`First Name`,`Last Name`), sep = " ")

appointments <- appointments %>%
  select(-c(
            `Email Address`, 
            `Placeholder`, 
            `Online`, 
            `Focus`
            ))

appointments$countGoals <- str_count(appointments$goal_list, "\\|")
appointments$isBrainstorming <- str_count(appointments$goal_list, "Brainstorming/Pre-writing")
appointments$isThesis <- str_count(appointments$goal_list, "Thesis formulation")
appointments$isRevision <- str_count(appointments$goal_list, "Revision")
appointments$isGrammar<- str_count(appointments$goal_list, "Grammar")
appointments$isStructure <- str_count(appointments$goal_list, "Structure/Organization")
appointments$isLabReport <- str_count(appointments$goal_list, "Science writing or lab report")
appointments$isBusiness <- str_count(appointments$goal_list, "Business writing")
appointments$isScholarship <- str_count(appointments$goal_list, "scholarship essay")
appointments$isGrantApp <- str_count(appointments$goal_list, "grant application")
appointments$isCitation <- str_count(appointments$goal_list, "citations")
appointments$isResume <- str_count(appointments$goal_list, "resumes")
appointments$isCoverLetter <- str_count(appointments$goal_list, "cover letters")
appointments$isPersonalStatement <- str_count(appointments$goal_list, "personal statements")

appointments <- appointments %>% relocate("appt_length", .after = "end")

appointments$date_created <- as.Date(appointments$date_created, format = "%B %d %Y")
appointments$date_modified <- as.Date(appointments$date_modified, format = "%B %d %Y")
appointments$appt_gap <- appointments$appt_date - appointments$date_created
appointments <- appointments %>% relocate("appt_gap", .after = "appt_length")


appointments_totals <- appointments_totals %>% 
  unite(name, c(`First Name`,`Last Name`), sep = " ")

appointments_totals <- appointments_totals %>%
  rename(
         total_appt = `Total Appointments`,
         missed_appt = `Missed Appointments (NOT INCLUDED IN TOTAL)`,
         placeholder_appt = `Placeholder Appointments (NOT INCLUDED IN TOTAL)`,
         netID = `Email Address`)

appointments_totals <- separate(data = appointments_totals, col = netID, into = c("netID", "temp"), sep = "@")
appointments_totals <- appointments_totals %>%
  select(-c("temp"))


client_report <- client_report %>%
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
    schedule = `Schedule Title`,
    tutor = `Staff or Resource Name`,
    appt_date = `Appointment Date`,
    start_time = `Start Time`,
    end_time = `End Time`,
    length = `Length (Minutes)`,
    report_added = `Report Added Dated`,
    assignment = `Assignment (C21)`,
    goal_worked = `We worked on:  (C22)`,
    tutor_comments = `Comments (C30)`
  )

client_report <- client_report %>% 
  unite(name, c(`First Name`,`Last Name`), sep = " ")

client_report$countGoalsWorked <- str_count(client_report$goal_worked, "\\|")

client_report$isBrainstorming <- str_count(client_report$goal_worked, "Brainstorming/Pre-writing")
client_report$isThesis <- str_count(client_report$goal_worked, "Thesis formulation")
client_report$isRevision <- str_count(client_report$goal_worked, "Revision")
client_report$isGrammar<- str_count(client_report$goal_worked, "Grammar")
client_report$isStructure <- str_count(client_report$goal_worked, "Structure/Organization")
client_report$isLabReport <- str_count(client_report$goal_worked, "Science writing or lab report")
client_report$isBusiness <- str_count(client_report$goal_worked, "Business writing")
client_report$isScholarship <- str_count(client_report$goal_worked, "scholarship essay")
client_report$isGrantApp <- str_count(client_report$goal_worked, "grant application")
client_report$isCitation <- str_count(client_report$goal_worked, "citations")
client_report$isResume <- str_count(client_report$goal_worked, "resumes")
client_report$isCoverLetter <- str_count(client_report$goal_worked, "cover letters")
client_report$isPersonalStatement <- str_count(client_report$goal_worked, "personal statements")

client_report <- client_report %>%
  select(-c(`Email Address`))

client_report$appt_date <- as.Date(client_report$appt_date)
client_report$last_update <- as.Date(client_report$last_update)
client_report$last_login <- as.Date(client_report$last_login)
client_report$report_added <- as.Date(client_report$report_added)
  
client_report$report_gap <- client_report$report_added - client_report$appt_date
client_report <- client_report %>% relocate("report_gap", .after = "report_added")
client_report <- client_report %>% relocate("tutor_comments", .after = "isPersonalStatement")

registrations <- registrations %>% 
  unite(name, c(`First Name`,`Last Name`), sep = " ")

registrations <- registrations %>%
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
    acct_disableed = `Acct. Disabled?`,
    total_appts = `Total # of Appointments (Not Including Missed or Placeholder)`,
    total_client_report_forms = `Total # of Client Report Forms`,
    missed_appts = `Total # of Missed Appointments`,
    placeholder_appts = `Total # of Placeholder Appointments`,
    canceled_appts = `Total # of Canceled Appointments`
  )

registrations$reg_date <- as.Date(registrations$reg_date)
registrations$last_login <- as.Date(registrations$last_login)
registrations$last_update <- as.Date(registrations$last_update)

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

waitlist$waitlist_gap <- waitlist$desired_date - waitlist$date_created
waitlist <- waitlist %>% relocate("waitlist_gap", .after = "date_created")
waitlist <- waitlist %>% rename(date_added = date_created)
waitlist <- waitlist %>% select(-c(`Email Address`))

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

survey <-  
  mutate(survey, rating_num = case_when(
    rating == 'Excellent' ~ 5,
    rating == 'Very Good' ~ 4,
    rating == 'Good' ~ 3,
    rating == 'Fair' ~ 2,
    rating == 'Poor' ~ 1,
    rating == 'Unacceptable' ~ 0
  ))

survey <- survey %>%
  select(-c("rating"))

survey <- survey %>% relocate("rating_num", .after = "survey_date")
survey$survey_date <- as.Date(survey$survey_date)


#Analysis of Desired Metrics:


appointments$grad_year <- as.numeric(appointments$grad_year)
appointments$grad_diff <- appointments$grad_year - as.numeric(substr(appointments$schedule, str_length(appointments$schedule) - 4 ,str_length(appointments$schedule)))

#Fix this portion!
appointments <- appointments %>%
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

client_report$grad_year <- as.numeric(client_report$grad_year)
client_report$grad_diff <- client_report$grad_year - as.numeric(substr(client_report$schedule, str_length(client_report$schedule) - 4 ,str_length(client_report$schedule)))
client_report <- client_report %>%
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


write.csv(appointments, file = "/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/appts.csv")
write.csv(appointments_totals, file = "/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/appt_totals.csv")
write.csv(registrations, file = "/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/regs.csv")
write.csv(client_report, file = "/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/cr.csv")
write.csv(survey, file = "/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/surv.csv")
write.csv(waitlist, file = "/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/wl.csv")

appointments <- appointments %>% relocate("current_year", .after = "standing")
waitlist <- waitlist %>% relocate("current_year", .after = "standing")
client_report <- client_report %>% relocate("current_year", .after = "standing")



