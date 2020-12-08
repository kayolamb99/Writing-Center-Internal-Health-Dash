

#Reading In Necessary Data From Local Machine
appointments <- readxl::read_excel("/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/Appointments.Starting August 31, 2014.Ending August 31, 2020.xlsx")
appointments_totals <- readxl::read_excel("/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/Appointment_Condensed.Starting August 31, 2014.Ending August 31, 2020.xlsx")

#Packages required for analysis
require(stringr)
require(sqldf)
require(dplyr)
require(tidyverse)
require(chron)
require(naniar)

#Renaming column names to conform to a uniform format

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

#Reshaping Date Columns to conform to the Date data type
appointments$reg_date <- as.Date(appointments$reg_date)
appointments$last_update <- as.Date(appointments$last_update)
appointments$last_login <- as.Date(appointments$last_login)
appointments$appt_date <- as.Date(appointments$appt_date)

#Reshaping Time Columns to conform to the Time data type
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

#Dropping unnecessary columns

appointments <- appointments %>%
  select(-c(
    `Email Address`, 
    `Placeholder`, 
    `Online`, 
    `Focus`
  ))

#Reshaping countGoals variable to include counts for all session goal types recorded

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




#Performing a 'current_year' calclation, as myWCOnline currently siloes users into class years at time of registration. Making this variable dynamic as needed

appointments$grad_year <- as.numeric(appointments$grad_year)
appointments$grad_diff <- appointments$grad_year - as.numeric(substr(appointments$schedule, str_length(appointments$schedule) - 4 ,str_length(appointments$schedule)))

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

appointments <- appointments %>% relocate("current_year", .after = "standing")

#Reshaping Appointment Totlas Dataset, which includes summary appointment information

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