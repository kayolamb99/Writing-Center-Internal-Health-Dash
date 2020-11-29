appts <- read.csv(file = '/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/appts.csv', header = TRUE)
appt_totals <- read.csv(file = '/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/appt_totals.csv', header = TRUE)
cr <- read.csv(file = '/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/cr.csv', header = TRUE)
reg <- read.csv(file = '/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/reg.csv', header = TRUE)
surv <- read.csv(file = '/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/surv.csv', header = TRUE)
wl <- read.csv(file = '/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/wl.csv', header = TRUE)

require(dplyr)
require(sqldf)


demo_breakdown_clients <- sqldf("SELECT current_year, COUNT (DISTINCT netID) AS total_clients
                        FROM appts 
                        GROUP BY current_year ")

demo_breakdown_appts <- sqldf("SELECT current_year, COUNT (netID) AS total_appts
                        FROM appts 
                        GROUP BY current_year ")


write.csv(demo_breakdown_appts, file = "/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/v1.csv")
write.csv(demo_breakdown_clients, file = "/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/v2.csv")

advanced_demo_breakdown_client <- sqldf("SELECT current_year, 
                                        school, first_language, schedule,
                                        COUNT (DISTINCT netID) AS total_clients
                                        FROM appts
                                        GROUP BY 1,2,3")

advanced_demo_breakdown_appts <- sqldf("SELECT current_year, 
                                        school, first_language, schedule,
                                        COUNT (netID) AS total_appts
                                        FROM appts
                                        GROUP BY 1,2,3")

write.csv(advanced_demo_breakdown_appts, file = "/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/v3.csv")
write.csv(advanced_demo_breakdown_client, file = "/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/v4.csv")

cr_ind <- sample(1:nrow(cr), 100)
appt_ind <- sample(1:nrow(appts), 100)

cr_sample <- cr[cr_ind, ]
appt_sample <- appts[appt_ind, ]

appt_sample <- appt_sample %>% select(-c("name", "tutor"))
cr_sample <- cr_sample %>% select(-c("name", "tutor"))

write.csv(appt_sample, file = "/Users/kayode/Desktop/appt_samp.csv")
write.csv(cr_sample, file = "/Users/kayode/Desktop/cr_samp.csv")

goals_combined <- c("Grammar", "Brainstorming", "Thesis", "Structure", "Lab Report", "Business Writing", "Scholarship", "Grant Application", "Citations", "Resume", "Cover Letter", "Personal Statement")
goals_combined <- as.data.frame(goals_combined)

goals_combined <- goals_combined %>%
  rename("Goals" = goals_combined)

colnames(appts)
appt_base1 <- sqldf("SELECT 
                      netID,
                      appt_date, 
                      schedule,
                      school,
                      first_language,
                      tutor,
                      course,
                      current_year,
                      instructor,
                      appt_length,
                      start,
                      end,
                      isBrainstorming AS statedBrainstorming,
                      isThesis AS statedThesis,
                      isRevision AS statedRevision,         
                      isGrammar AS statedGrammar,         
                      isStructure AS statedStructure,
                      isLabReport AS statedLabReport,
                      isBusiness AS statedBusiness,
                      isScholarship AS statedScholarship,
                      isGrantApp AS statedGrantApp,
                      isCitation AS statedCitation,
                      isResume AS statedResume,
                      isCoverLetter AS statedCoverLetter,
                      isPersonalStatement AS statedPersonalStatement, 
                      countGoals AS countGoalsInitial
                    FROM appts")


appt_base2 <-sqldf("SELECT 
                      netID,
                      appt_date, 
                      schedule,
                      school,
                      first_language,
                      tutor,
                      current_year,
                      isBrainstorming AS workedBrainstorming,
                      isThesis AS workedThesis,
                      isRevision AS workedRevision,         
                      isGrammar AS workedGrammar,         
                      isStructure AS workedStructure,
                      isLabReport AS workedLabReport,
                      isBusiness AS workedBusiness,
                      isScholarship AS workedScholarship,
                      isGrantApp AS workedGrantApp,
                      isCitation AS workedCitations,
                      isResume AS workedResume,
                      isCoverLetter AS workedCoverLetter,
                      isPersonalStatement AS workedPersonalStatement, 
                      countGoalsWorked
                    FROM cr")

View(appt_base2)
View(appt_base1)

appt_combined <- sqldf("SELECT *
                       FROM appt_base1
                       JOIN appt_base2 USING(netID, appt_date, schedule)")

appt_combined <- appt_combined %>%
  select(-c(
    school..27,
    first_language..28,
    tutor..29,
    current_year..30
    ))

appt_combined$goal_diff <- appt_combined$countGoalsWorked - appt_combined$countGoalsInitial  

write.csv(appt_combined, file = "/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/combinedappt.csv")

goals_combined <- sqldf("SELECT
                          Goals

                        GROUP BY by Goals")

calc_goal_metrics <- function(goal_name) {
  df = data.frame(goal = character(), numStated = numeric(), 
                  numWorkedGrammar = numeric(),
                  numWorkedBrainstorming = numeric(),
                  numWorkedThesis = numeric(),
                  numWorkedStructure = numeric(),
                  numWorkedLabReport = numeric(),
                  numWorkedBusiness = numeric(),
                  numWorkedScholarship = numeric(),
                  numWorkedGrantApp = numeric(),
                  numWorkedCitations = numeric(),
                  numWorkedResume = numeric(),
                  numWorkedCoverLetter = numeric(),
                  numWorkedPersonalStatement = numeric())
  
  
  goal = goal_name
  numStated = sqldf(sprintf("SELECT SUM(stated%s) AS numStated FROM appt_combined WHERE stated%s == 1",goal_name, goal_name))
  numWorkedGrammar = sqldf(sprintf("SELECT SUM(workedGrammar) AS numWorkedGrammar FROM appt_combined WHERE stated%s == 1",goal_name))
  numWorkedBrainstorming = sqldf(sprintf("SELECT SUM(workedBrainstorming) AS numWorkedBrainstorming FROM appt_combined WHERE stated%s == 1",goal_name))
  numWorkedThesis = sqldf(sprintf("SELECT SUM(workedThesis) AS numWorkedThesis FROM appt_combined WHERE stated%s == 1",goal_name))
  numWorkedStructure = sqldf(sprintf("SELECT SUM(workedStructure) AS numWorkedStructure FROM appt_combined WHERE stated%s == 1",goal_name))
  numWorkedLabReport = sqldf(sprintf("SELECT SUM(workedLabReport) AS numWorkedLabReport FROM appt_combined WHERE stated%s == 1",goal_name))
  numWorkedBusiness = sqldf(sprintf("SELECT SUM(workedBusiness) AS numWorkedBusiness FROM appt_combined WHERE stated%s == 1",goal_name))
  numWorkedScholarship = sqldf(sprintf("SELECT SUM(workedScholarship) AS numWorkedScholarship FROM appt_combined WHERE stated%s == 1",goal_name))
  numWorkedGrantApp = sqldf(sprintf("SELECT SUM(workedGrantApp) AS numWorkedGrantApp FROM appt_combined WHERE stated%s == 1",goal_name))
  numWorkedCitations = sqldf(sprintf("SELECT SUM(workedCitations) AS numWorkedCitations FROM appt_combined WHERE stated%s == 1",goal_name))
  numWorkedResume = sqldf(sprintf("SELECT SUM(workedResume) AS numWorkedResume FROM appt_combined WHERE stated%s == 1",goal_name))
  numWorkedCoverLetter = sqldf(sprintf("SELECT SUM(workedCoverLetter) AS numWorkedCoverLetter FROM appt_combined WHERE stated%s == 1",goal_name))
  numWorkedPersonalStatement = sqldf(sprintf("SELECT SUM(workedPersonalStatement) AS numWorkedPersonalStatement FROM appt_combined WHERE stated%s == 1",goal_name))
  
  df <- rbind(df, c(goal,numStated, numWorkedGrammar, numWorkedBrainstorming, numWorkedThesis, numWorkedStructure, numWorkedLabReport, numWorkedBusiness,
                    numWorkedScholarship, numWorkedGrantApp, numWorkedCitations, numWorkedResume, numWorkedCoverLetter, numWorkedPersonalStatement))
  return (df)
}


appt_combined <- appt_combined %>%
  rename (
    statedCitations = statedCitation
  )

  
  df1 <- data.frame(calc_goal_metrics("Grammar"))
  df2 <- data.frame(calc_goal_metrics("Brainstorming"))
  df3 <- data.frame(calc_goal_metrics("Thesis"))
  df4 <- data.frame(calc_goal_metrics("Structure"))
  df5 <- data.frame(calc_goal_metrics("LabReport"))
  df6 <- data.frame(calc_goal_metrics("Business"))
  df7 <- data.frame(calc_goal_metrics("Scholarship"))
  df8 <- data.frame(calc_goal_metrics("GrantApp"))
  df9 <- data.frame(calc_goal_metrics("Citations"))
  df10 <- data.frame(calc_goal_metrics("Resume"))
  df11 <- data.frame(calc_goal_metrics("CoverLetter"))
  df12 <- data.frame(calc_goal_metrics("PersonalStatement"))
  
  df1 <- df1 %>%
    rename (
      goal = X.Grammar.
    )
  
  df2 <- df2 %>%
    rename (
      goal = X.Brainstorming.
    )
  
  df3 <- df3 %>%
    rename (
      goal = X.Thesis.
    )
  
  df4 <- df4 %>%
    rename (
      goal = X.Structure.
    )
  
  df5 <- df5 %>%
    rename (
      goal = X.LabReport.
    )
  
  df6 <- df6 %>%
    rename (
      goal = X.Business.
    )
  
  df7 <- df7 %>%
    rename (
      goal = X.Scholarship.
    )
  
  df8 <- df8 %>%
    rename (
      goal = X.GrantApp.
    )
  
  df9 <- df9 %>%
    rename (
      goal = X.Citations.
    )
  
  df10 <- df10 %>%
    rename (
      goal = X.Resume.
    )
  
  df11 <- df11 %>%
    rename (
      goal = X.CoverLetter.
    )
  
  df12 <- df12 %>%
    rename (
      goal = X.PersonalStatement.
    )
  
  goals_combined <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
  write.csv(goals_combined, file = "/Users/kayode/Desktop/Writing Center Data Projects/New Dashboard/Data/goals_combined.csv")


