library(readxl)
library(tidyverse)

#Intermediate
RPIOSCC_Int_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/RPIOSCC.xlsx", sheet = "IntRPIOSCC")
colnames(RPIOSCC_Int_data) [1] = "grade_span_n"
colnames(RPIOSCC_Int_data) [2] = "position_n"

#conversions
RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, total_class_contacts_c = RPIOSCC_Int_data$`Enter the number of students on your caseload with 1-5 accommodations.`+RPIOSCC_Int_data$`Enter the number of students on your caseload with 6 or more accommodations.`)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of students on your caseload with 1-5 accommodations.`,`Enter the number of students on your caseload with 6 or more accommodations.`))

RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, students_data_c = RPIOSCC_Int_data$`Enter the number of students you have collected data on to determine if the student requires para support.`*120)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of students you have collected data on to determine if the student requires para support.`))

RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, rating_scale_c = RPIOSCC_Int_data$`Enter the number of Rating Scales  (FBA,  Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`*45)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of Rating Scales  (FBA,  Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`))

RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, progress_goals_c = RPIOSCC_Int_data$`Enter the number of students you wrote progress on goals for this year.`*270)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of students you wrote progress on goals for this year.`))

RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, present_levels_c = RPIOSCC_Int_data$`Enter the number of hours this year you have spent writing present levels this school year.`*60)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of hours this year you have spent writing present levels this school year.`))

RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, district_assessment_c = RPIOSCC_Int_data$`Enter the number of students you administered one to one District Mandated Assessments with.`*60)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of students you administered one to one District Mandated Assessments with.`))

RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, initials__c = RPIOSCC_Int_data$`Enter the number of initials you held this year.`*180)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of initials you held this year.`))

RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, district_IEP_c = RPIOSCC_Int_data$`Enter the number of District Staff Involved IEP meetings that you held this school year.`*480)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you held this school year.`))

RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, annual_IEP_c = RPIOSCC_Int_data$`Enter the number of annual IEP meetings that you held this school year.`*180)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of annual IEP meetings that you held this school year.`))

RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, triennial_IEP_c = RPIOSCC_Int_data$`Enter the number of Triennial IEP meetings that you held this school year.`*360)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of Triennial IEP meetings that you held this school year.`))

RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, staff_meeting_c = RPIOSCC_Int_data$`Enter the number of staffing meetings that you attended this school year.`*60)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, amendment_meeting_c = RPIOSCC_Int_data$`Enter the number of amendment meetings that you held this school year.`*90)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of amendment meetings that you held this school year.`))

RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, manifestation_meeting_c = RPIOSCC_Int_data$`Enter the number of manifestation determination meetings that you held this school year.`*240)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of manifestation determination meetings that you held this school year.`))

RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, transition_meeting_c = RPIOSCC_Int_data$`Enter the number of Transition Meetings that you anticipate holding this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`*120)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate holding this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, interim_IEP_c = RPIOSCC_Int_data$`Enter the number of Interim IEPs that you held this school year.`*90)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of Interim IEPs that you held this school year.`))

RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, MTSS_meeting_c = RPIOSCC_Int_data$`Enter the number of MTSS T2/T3 meetings you have attended this year.`*90)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of MTSS T2/T3 meetings you have attended this year.`))

RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, BER_report_c = RPIOSCC_Int_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`*15)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, paras_7_c = RPIOSCC_Int_data$`Enter the number of 7-hour paras requiring direction that you facilitate.`*5400)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of 7-hour paras requiring direction that you facilitate.`))

RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, paras_unfilled_c = RPIOSCC_Int_data$`Enter the number of unfilled para positions or para positions filled with contracted paras.`*10800)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of unfilled para positions or para positions filled with contracted paras.`))

RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, discipline_referrals_c = RPIOSCC_Int_data$`Enter the number of Discipline Referrals you have written this year.`*5)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of Discipline Referrals you have written this year.`))

RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, days_pulled_c = RPIOSCC_Int_data$`Enter the number of days you have been pulled from your duties this year.`*420)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of days you have been pulled from your duties this year.`))

RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, sites_traveled_c = RPIOSCC_Int_data$`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`*5400)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`))

RPIOSCC_Int_data = mutate(RPIOSCC_Int_data, miles_traveled_c = (RPIOSCC_Int_data$`Enter the number of miles driven between sites in a typical month.`*2)+15)
RPIOSCC_Int_data = RPIOSCC_Int_data %>% select(-c(`Enter the number of miles driven between sites in a typical month.`))



#creating the rankings

RPIOSCC_Int_ranking = transmute(RPIOSCC_Int_data, scale_1_5 = (RPIOSCC_Int_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
RPIOSCC_Int_ranking = mutate(RPIOSCC_Int_ranking, min_sum = rowSums(RPIOSCC_Int_data[ ,c(3:4,9:33)]))

write_xlsx(RPIOSCC_Int_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/RPIOSCC_Int_ranking.xlsx")

#jpeg(file="RPIOSCC_Int_ranking_plot.jpeg")
#plot(RPIOSCC_Int_ranking$scale_1_5,RPIOSCC_Int_ranking$min_sum)
#dev.off()






