library(readxl)
library(tidyverse)

#Elementary
RPIPOO_Elm_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/RPIPOO.xlsx", sheet = "ElmRPIPOO")
colnames(RPIPOO_Elm_data) [1] = "grade_span_n"
colnames(RPIPOO_Elm_data) [2] = "position_n"

#conversions
RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, total_class_contacts_c = RPIPOO_Elm_data$`Enter the number of students on your caseload with 1-5 accommodations.`+RPIPOO_Elm_data$`Enter the number of students on your caseload with 6 or more accommodations.`)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of students on your caseload with 1-5 accommodations.`,`Enter the number of students on your caseload with 6 or more accommodations.`))

RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, students_data_c = RPIPOO_Elm_data$`Enter the number of students you have collected data on to determine if the student requires para support.`*120)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of students you have collected data on to determine if the student requires para support.`))

RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, rating_scale_c = RPIPOO_Elm_data$`Enter the number of Rating Scales  (FBA,  Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`*45)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of Rating Scales  (FBA,  Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`))

RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, progress_goals_c = RPIPOO_Elm_data$`Enter the number of students you wrote progress on goals for this year.`*360)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of students you wrote progress on goals for this year.`))

RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, present_levels_c = RPIPOO_Elm_data$`Enter the number of hours this year you have spent writing present levels this school year.`*60)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of hours this year you have spent writing present levels this school year.`))

RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, district_assessment_c = RPIPOO_Elm_data$`Enter the number of students you administered one to one District Mandated Assessments with.`*60)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of students you administered one to one District Mandated Assessments with.`))

RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, initials__c = RPIPOO_Elm_data$`Enter the number of initials you held this year.`*180)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of initials you held this year.`))

RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, district_IEP_c = RPIPOO_Elm_data$`Enter the number of District Staff Involved IEP meetings that you held this school year.`*480)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you held this school year.`))

RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, annual_IEP_c = RPIPOO_Elm_data$`Enter the number of annual IEP meetings that you held this school year.`*180)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of annual IEP meetings that you held this school year.`))

RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, triennial_IEP_c = RPIPOO_Elm_data$`Enter the number of Triennial IEP meetings that you held this school year.`*360)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of Triennial IEP meetings that you held this school year.`))

RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, staff_meeting_c = RPIPOO_Elm_data$`Enter the number of staffing meetings that you attended this school year.`*60)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, amendment_meeting_c = RPIPOO_Elm_data$`Enter the number of amendment meetings that you held this school year.`*90)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of amendment meetings that you held this school year.`))

RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, manifestation_meeting_c = RPIPOO_Elm_data$`Enter the number of manifestation determination meetings that you held this school year.`*240)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of manifestation determination meetings that you held this school year.`))

RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, transition_meeting_c = RPIPOO_Elm_data$`Enter the number of Transition Meetings that you anticipate holding this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`*120)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate holding this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, interim_IEP_c = RPIPOO_Elm_data$`Enter the number of Interim IEPs that you held this school year.`*90)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of Interim IEPs that you held this school year.`))

RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, MTSS_meeting_c = RPIPOO_Elm_data$`Enter the number of MTSS T2/T3 meetings you have attended this year.`*90)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of MTSS T2/T3 meetings you have attended this year.`))

RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, BER_report_c = RPIPOO_Elm_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`*15)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, paras_7_c = RPIPOO_Elm_data$`Enter the number of 7-hour paras requiring direction that you facilitate.`*5400)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of 7-hour paras requiring direction that you facilitate.`))

RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, paras_unfilled_c = RPIPOO_Elm_data$`Enter the number of unfilled para positions or para positions filled with contracted paras.`*10800)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of unfilled para positions or para positions filled with contracted paras.`))

RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, discipline_referrals_c = RPIPOO_Elm_data$`Enter the number of Discipline Referrals you have written this year.`*5)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of Discipline Referrals you have written this year.`))

RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, days_pulled_c = RPIPOO_Elm_data$`Enter the number of days you have been pulled from your duties this year.`*420)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of days you have been pulled from your duties this year.`))

RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, sites_traveled_c = RPIPOO_Elm_data$`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`*5400)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`))

RPIPOO_Elm_data = mutate(RPIPOO_Elm_data, miles_traveled_c = (RPIPOO_Elm_data$`Enter the number of miles driven between sites in a typical month.`*2)+15)
RPIPOO_Elm_data = RPIPOO_Elm_data %>% select(-c(`Enter the number of miles driven between sites in a typical month.`))



#creating the rankings

RPIPOO_Elm_ranking = transmute(RPIPOO_Elm_data, scale_1_5 = (RPIPOO_Elm_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
RPIPOO_Elm_ranking = mutate(RPIPOO_Elm_ranking, min_sum = rowSums(RPIPOO_Elm_data[ ,c(3:4,9:33)]))

write_xlsx(RPIPOO_Elm_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/RPIPOO_Elm_ranking.xlsx")

jpeg(file="RPIPOO_Elm_ranking_plot.jpeg")
plot(RPIPOO_Elm_ranking$scale_1_5,RPIPOO_Elm_ranking$min_sum)
dev.off()