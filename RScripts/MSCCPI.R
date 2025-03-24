library(writexl)
library(readxl)
library(tidyverse)

#Intermediate
MSCCPI_Int_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/MSCCPI.xlsx", sheet = "IntMSCCPI")
colnames(MSCCPI_Int_data) [1] = "grade_span_n"
colnames(MSCCPI_Int_data) [2] = "position_n"

#conversions
MSCCPI_Int_data = mutate(MSCCPI_Int_data, total_class_contacts_c = MSCCPI_Int_data$`Enter the number of students on your caseload with 1-5 accommodations.` +MSCCPI_Int_data$`Enter the number of students on your caseload with 6 or more accommodations.`)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of students on your caseload with 1-5 accommodations.`,`Enter the number of students on your caseload with 6 or more accommodations.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, student_data_c = MSCCPI_Int_data$`Enter the number of students you have collected data on to determine if the student requires para support.`*120)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of students you have collected data on to determine if the student requires para support.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, present_levels_c = MSCCPI_Int_data$`Enter the number of hours this year you have spent writing present levels this school year.`*60)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of hours this year you have spent writing present levels this school year.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, progress_goals_c = MSCCPI_Int_data$`Enter the number of students you wrote progress on goals for this year.`*270)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of students you wrote progress on goals for this year.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, different_subjects_c = MSCCPI_Int_data$`Enter the number of different subjects taught (Preps) in a single day or for blocked sites, over the two days.  (For example: US History, World History, AP Gov/Econ would equal 3)`*9000)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of different subjects taught (Preps) in a single day or for blocked sites, over the two days.  (For example: US History, World History, AP Gov/Econ would equal 3)`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, different_grades_c = MSCCPI_Int_data$`Enter the number of grade level curriculums/subjects you are required to teach in a SINGLE class.`*9000)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of grade level curriculums/subjects you are required to teach in a SINGLE class.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, DRDP_c = MSCCPI_Int_data$`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`* 45)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, rating_scale_c = MSCCPI_Int_data$`Enter the number of Rating Scales  (FBA,  Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`* 45)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of Rating Scales  (FBA,  Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, district_asessments_c = MSCCPI_Int_data$`Enter the number of students you administered one to one District Mandated Assessments with.`* 60)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of students you administered one to one District Mandated Assessments with.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, ELPAC_c = MSCCPI_Int_data$`Enter the number of students you administered the one to one ELPAC/Alt-ELPAC Testing with this year.`* 20)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of students you administered the one to one ELPAC/Alt-ELPAC Testing with this year.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, meetings_504_c = MSCCPI_Int_data$`Enter the number of 504 meetings you have attended this year.`*60)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of 504 meetings you have attended this year.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, initial_c = MSCCPI_Int_data$`Enter the number of initials that you held this year.`*180)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of initials that you held this year.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, district_IEP_c = MSCCPI_Int_data$`Enter the number of District Staff Involved IEP meetings that you held this school year.`*480)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you held this school year.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, annual_IEP_c = MSCCPI_Int_data$`Enter the number of annual IEP meetings that you held this school year.`* 180)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of annual IEP meetings that you held this school year.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, triennial_IEP_c = MSCCPI_Int_data$`Enter the number of Triennial IEP meetings that you held this school year.`* 360)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of Triennial IEP meetings that you held this school year.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, staff_meeting_c = MSCCPI_Int_data$`Enter the number of staffing meetings that you attended this school year.`* 60)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, amendment_meeting_c = MSCCPI_Int_data$`Enter the number of amendment meetings that you held this school year.`* 90)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of amendment meetings that you held this school year.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, mainfestation_meeting_c = MSCCPI_Int_data$`Enter the number of manifestation determination meetings that you held this school year.`* 240)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of manifestation determination meetings that you held this school year.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, transition_meeting_c = MSCCPI_Int_data$`Enter the number of Transition Meetings that you anticipate holding this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`* 120)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate holding this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, interim_IEP_c = MSCCPI_Int_data$`Enter the number of Interim IEPs that you held this school year.`* 90)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of Interim IEPs that you held this school year.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, discipline_referrals_c = MSCCPI_Int_data$`Enter the number of Discipline Referrals you have written this year.`* 5)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of Discipline Referrals you have written this year.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, independent_study_c = MSCCPI_Int_data$`Enter the number of Independent Study Agreements you have written and approved this year.`* 60)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of Independent Study Agreements you have written and approved this year.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, paras_7_c = MSCCPI_Int_data$`Enter the number of 7-hour paras requiring direction that you facilitate.`* 5400)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of 7-hour paras requiring direction that you facilitate.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, paras_unfilled_c = MSCCPI_Int_data$`Enter the number of unfilled para positions or para positions filled with contracted paras.`* 10800)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of unfilled para positions or para positions filled with contracted paras.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, BER_report_c = MSCCPI_Int_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`* 15)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

MSCCPI_Int_data = mutate(MSCCPI_Int_data, pulled_days_c = MSCCPI_Int_data$`Enter the number of days you have been pulled out of the classroom this year.`* 60)
MSCCPI_Int_data = MSCCPI_Int_data %>% select(-c(`Enter the number of days you have been pulled out of the classroom this year.`))



#creating the rankings

MSCCPI_Int_ranking = transmute(MSCCPI_Int_data, scale_1_5 = (MSCCPI_Int_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
MSCCPI_Int_ranking = mutate(MSCCPI_Int_ranking, min_sum = rowSums(MSCCPI_Int_data[ ,c(15,18:38)]))

write_xlsx(MSCCPI_Int_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/MSCCPI_Int_ranking.xlsx")

jpeg(file="MSCCPI_Int_ranking_plot.jpeg")
plot(MSCCPI_Int_ranking$scale_1_5,MSCCPI_Int_ranking$min_sum)
dev.off()