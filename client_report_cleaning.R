#Loading Data from local machine
client_report <- readxl::read_excel("/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/Client Report Forms.Starting August 31, 2014.Ending August 31, 2020.xlsx")

#Renaming columns for workability and ease of use
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

#Reshaping the 'countGoals' data structure to include counts of individual goals worked by the client according to the tutor

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

#Dropping Email Address column, which is a duplicate of the 'netID' column
client_report <- client_report %>%
  select(-c(`Email Address`))

#Making these columns conform to the Date data type
client_report$appt_date <- as.Date(client_report$appt_date)
client_report$last_update <- as.Date(client_report$last_update)
client_report$last_login <- as.Date(client_report$last_login)
client_report$report_added <- as.Date(client_report$report_added)

#Adding a report gap feature that describes time taken between when a session occurred and when the tutor actually filed a report
client_report$report_gap <- client_report$report_added - client_report$appt_date
client_report <- client_report %>% relocate("report_gap", .after = "report_added")
client_report <- client_report %>% relocate("tutor_comments", .after = "isPersonalStatement")

#Adding a 'current year' feature, changing myWCOnline's static metric of class year into a dynamic variable that changes from year to year
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

client_report <- client_report %>% relocate("current_year", .after = "standing")