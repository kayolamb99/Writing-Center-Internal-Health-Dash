#Loading data from local machine
registrations <- readxl::read_excel("/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/Registration.Starting August 31, 2014.Ending August 31, 2020.xlsx")

#Renaming and reshaping column names
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

#Making date columns conform to the Date data type
registrations$reg_date <- as.Date(registrations$reg_date)
registrations$last_login <- as.Date(registrations$last_login)
registrations$last_update <- as.Date(registrations$last_update)