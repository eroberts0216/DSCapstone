library(writexl)
library(readxl)
library(tidyverse)

#Elementary
MSSC_Elm_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/MSSC.xlsx", sheet = "ElmMSSC")
colnames(MSSC_Elm_data) [1] = "grade_span_n"
colnames(MSSC_Elm_data) [2] = "position_n"

#conversions
MSSC_Elm_data = mutate(MSSC_Elm_data, total_class_contacts_c = MSSC_Elm_data$`Enter the number of students on your caseload with 1-5 accommodations.` +MSSC_Elm_data$`Enter the number of students on your caseload with 6 or more accommodations.`)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of students on your caseload with 1-5 accommodations.`,`Enter the number of students on your caseload with 6 or more accommodations.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, student_data_c = MSSC_Elm_data$`Enter the number of students you have collected data on to determine if the student requires para support.`*120)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of students you have collected data on to determine if the student requires para support.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, present_levels_c = MSSC_Elm_data$`Enter the number of hours this year you have spent writing present levels this school year.`*60)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of hours this year you have spent writing present levels this school year.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, progress_goals_c = MSSC_Elm_data$`Enter the number of students you wrote progress on goals for this year.`*360)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of students you wrote progress on goals for this year.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, different_subjects_c = MSSC_Elm_data$`Enter the number of different subjects taught (Preps) in a single day or for blocked sites, over the two days.  (For example: US History, World History, AP Gov/Econ would equal 3)`*9000)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of different subjects taught (Preps) in a single day or for blocked sites, over the two days.  (For example: US History, World History, AP Gov/Econ would equal 3)`))

MSSC_Elm_data = mutate(MSSC_Elm_data, different_grades_c = MSSC_Elm_data$`Enter the number of grade level curriculums/subjects you are required to teach in a SINGLE class.`*9000)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of grade level curriculums/subjects you are required to teach in a SINGLE class.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, DRDP_c = MSSC_Elm_data$`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`* 45)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, rating_scale_c = MSSC_Elm_data$`Enter the number of Rating Scales  (FBA,  Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`* 45)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of Rating Scales  (FBA,  Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, district_asessments_c = MSSC_Elm_data$`Enter the number of students you administered one to one District Mandated Assessments with.`* 60)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of students you administered one to one District Mandated Assessments with.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, ELPAC_c = MSSC_Elm_data$`Enter the number of students you administered the one to one ELPAC/Alt-ELPAC Testing with this year.`* 20)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of students you administered the one to one ELPAC/Alt-ELPAC Testing with this year.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, meetings_504_c = MSSC_Elm_data$`Enter the number of 504 meetings you have attended this year.`*60)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of 504 meetings you have attended this year.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, initial_c = MSSC_Elm_data$`Enter the number of initials that you held this year.`*180)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of initials that you held this year.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, district_IEP_c = MSSC_Elm_data$`Enter the number of District Staff Involved IEP meetings that you held this school year.`*480)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you held this school year.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, annual_IEP_c = MSSC_Elm_data$`Enter the number of annual IEP meetings that you held this school year.`* 180)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of annual IEP meetings that you held this school year.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, triennial_IEP_c = MSSC_Elm_data$`Enter the number of Triennial IEP meetings that you held this school year.`* 360)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of Triennial IEP meetings that you held this school year.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, staff_meeting_c = MSSC_Elm_data$`Enter the number of staffing meetings that you attended this school year.`* 60)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, amendment_meeting_c = MSSC_Elm_data$`Enter the number of amendment meetings that you held this school year.`* 90)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of amendment meetings that you held this school year.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, mainfestation_meeting_c = MSSC_Elm_data$`Enter the number of manifestation determination meetings that you held this school year.`* 240)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of manifestation determination meetings that you held this school year.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, transition_meeting_c = MSSC_Elm_data$`Enter the number of Transition Meetings that you anticipate holding this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`* 120)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate holding this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

MSSC_Elm_data = mutate(MSSC_Elm_data, interim_IEP_c = MSSC_Elm_data$`Enter the number of Interim IEPs that you held this school year.`* 90)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of Interim IEPs that you held this school year.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, discipline_referrals_c = MSSC_Elm_data$`Enter the number of Discipline Referrals you have written this year.`* 5)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of Discipline Referrals you have written this year.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, independent_study_c = MSSC_Elm_data$`Enter the number of Independent Study Agreements you have written and approved this year.`* 60)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of Independent Study Agreements you have written and approved this year.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, paras_7_c = MSSC_Elm_data$`Enter the number of 7-hour paras requiring direction that you facilitate.`* 5400)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of 7-hour paras requiring direction that you facilitate.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, paras_unfilled_c = MSSC_Elm_data$`Enter the number of unfilled para positions or para positions filled with contracted paras.`* 10800)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of unfilled para positions or para positions filled with contracted paras.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, BER_report_c = MSSC_Elm_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`* 15)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

MSSC_Elm_data = mutate(MSSC_Elm_data, pulled_days_c = MSSC_Elm_data$`Enter the number of days you have been pulled out of the classroom this year.`* 60)
MSSC_Elm_data = MSSC_Elm_data %>% select(-c(`Enter the number of days you have been pulled out of the classroom this year.`))



#creating the rankings

MSSC_Elm_ranking = transmute(MSSC_Elm_data, scale_1_5 = (MSSC_Elm_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
MSSC_Elm_ranking = mutate(MSSC_Elm_ranking, min_sum = rowSums(MSSC_Elm_data[ ,c(15,18:38)]))

write_xlsx(MSSC_Elm_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/MSSC_Elm_ranking.xlsx")

jpeg(file="MSSC_Elm_ranking_plot.jpeg")
plot(MSSC_Elm_ranking$scale_1_5,MSSC_Elm_ranking$min_sum)
dev.off()





#High School
MSSC_HS_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/MSSC.xlsx", sheet = "HSMSSC")
colnames(MSSC_HS_data) [1] = "grade_span_n"
colnames(MSSC_HS_data) [2] = "position_n"

#conversions
MSSC_HS_data = mutate(MSSC_HS_data, total_class_contacts_c = MSSC_HS_data$`Enter the number of students on your caseload with 1-5 accommodations.` +MSSC_HS_data$`Enter the number of students on your caseload with 6 or more accommodations.`)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of students on your caseload with 1-5 accommodations.`,`Enter the number of students on your caseload with 6 or more accommodations.`))

MSSC_HS_data = mutate(MSSC_HS_data, student_data_c = MSSC_HS_data$`Enter the number of students you have collected data on to determine if the student requires para support.`*120)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of students you have collected data on to determine if the student requires para support.`))

MSSC_HS_data = mutate(MSSC_HS_data, present_levels_c = MSSC_HS_data$`Enter the number of hours this year you have spent writing present levels this school year.`*60)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of hours this year you have spent writing present levels this school year.`))

MSSC_HS_data = mutate(MSSC_HS_data, progress_goals_c = MSSC_HS_data$`Enter the number of students you wrote progress on goals for this year.`*360)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of students you wrote progress on goals for this year.`))

MSSC_HS_data = mutate(MSSC_HS_data, different_subjects_c = MSSC_HS_data$`Enter the number of different subjects taught (Preps) in a single day or for blocked sites, over the two days.  (For example: US History, World History, AP Gov/Econ would equal 3)`*9000)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of different subjects taught (Preps) in a single day or for blocked sites, over the two days.  (For example: US History, World History, AP Gov/Econ would equal 3)`))

MSSC_HS_data = mutate(MSSC_HS_data, different_grades_c = MSSC_HS_data$`Enter the number of grade level curriculums/subjects you are required to teach in a SINGLE class.`*9000)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of grade level curriculums/subjects you are required to teach in a SINGLE class.`))

MSSC_HS_data = mutate(MSSC_HS_data, DRDP_c = MSSC_HS_data$`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`* 45)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`))

MSSC_HS_data = mutate(MSSC_HS_data, rating_scale_c = MSSC_HS_data$`Enter the number of Rating Scales  (FBA,  Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`* 45)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of Rating Scales  (FBA,  Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`))

MSSC_HS_data = mutate(MSSC_HS_data, district_asessments_c = MSSC_HS_data$`Enter the number of students you administered one to one District Mandated Assessments with.`* 60)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of students you administered one to one District Mandated Assessments with.`))

MSSC_HS_data = mutate(MSSC_HS_data, ELPAC_c = MSSC_HS_data$`Enter the number of students you administered the one to one ELPAC/Alt-ELPAC Testing with this year.`* 20)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of students you administered the one to one ELPAC/Alt-ELPAC Testing with this year.`))

MSSC_HS_data = mutate(MSSC_HS_data, meetings_504_c = MSSC_HS_data$`Enter the number of 504 meetings you have attended this year.`*60)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of 504 meetings you have attended this year.`))

MSSC_HS_data = mutate(MSSC_HS_data, initial_c = MSSC_HS_data$`Enter the number of initials that you held this year.`*180)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of initials that you held this year.`))

MSSC_HS_data = mutate(MSSC_HS_data, district_IEP_c = MSSC_HS_data$`Enter the number of District Staff Involved IEP meetings that you held this school year.`*480)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you held this school year.`))

MSSC_HS_data = mutate(MSSC_HS_data, annual_IEP_c = MSSC_HS_data$`Enter the number of annual IEP meetings that you held this school year.`* 180)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of annual IEP meetings that you held this school year.`))

MSSC_HS_data = mutate(MSSC_HS_data, triennial_IEP_c = MSSC_HS_data$`Enter the number of Triennial IEP meetings that you held this school year.`* 360)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of Triennial IEP meetings that you held this school year.`))

MSSC_HS_data = mutate(MSSC_HS_data, staff_meeting_c = MSSC_HS_data$`Enter the number of staffing meetings that you attended this school year.`* 60)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

MSSC_HS_data = mutate(MSSC_HS_data, amendment_meeting_c = MSSC_HS_data$`Enter the number of amendment meetings that you held this school year.`* 90)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of amendment meetings that you held this school year.`))

MSSC_HS_data = mutate(MSSC_HS_data, mainfestation_meeting_c = MSSC_HS_data$`Enter the number of manifestation determination meetings that you held this school year.`* 240)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of manifestation determination meetings that you held this school year.`))

MSSC_HS_data = mutate(MSSC_HS_data, transition_meeting_c = MSSC_HS_data$`Enter the number of Transition Meetings that you anticipate holding this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`* 120)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate holding this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

MSSC_HS_data = mutate(MSSC_HS_data, interim_IEP_c = MSSC_HS_data$`Enter the number of Interim IEPs that you held this school year.`* 90)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of Interim IEPs that you held this school year.`))

MSSC_HS_data = mutate(MSSC_HS_data, discipline_referrals_c = MSSC_HS_data$`Enter the number of Discipline Referrals you have written this year.`* 5)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of Discipline Referrals you have written this year.`))

MSSC_HS_data = mutate(MSSC_HS_data, independent_study_c = MSSC_HS_data$`Enter the number of Independent Study Agreements you have written and approved this year.`* 60)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of Independent Study Agreements you have written and approved this year.`))

MSSC_HS_data = mutate(MSSC_HS_data, paras_7_c = MSSC_HS_data$`Enter the number of 7-hour paras requiring direction that you facilitate.`* 5400)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of 7-hour paras requiring direction that you facilitate.`))

MSSC_HS_data = mutate(MSSC_HS_data, paras_unfilled_c = MSSC_HS_data$`Enter the number of unfilled para positions or para positions filled with contracted paras.`* 10800)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of unfilled para positions or para positions filled with contracted paras.`))

MSSC_HS_data = mutate(MSSC_HS_data, BER_report_c = MSSC_HS_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`* 15)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

MSSC_HS_data = mutate(MSSC_HS_data, pulled_days_c = MSSC_HS_data$`Enter the number of days you have been pulled out of the classroom this year.`* 60)
MSSC_HS_data = MSSC_HS_data %>% select(-c(`Enter the number of days you have been pulled out of the classroom this year.`))



#creating the rankings

MSSC_HS_ranking = transmute(MSSC_HS_data, scale_1_5 = (MSSC_HS_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
MSSC_HS_ranking = mutate(MSSC_HS_ranking, min_sum = rowSums(MSSC_HS_data[ ,c(15,18:38)]))

write_xlsx(MSSC_HS_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/MSSC_HS_ranking.xlsx")

jpeg(file="MSSC_HS_ranking_plot.jpeg")
plot(MSSC_HS_ranking$scale_1_5,MSSC_HS_ranking$min_sum)
dev.off()





#MSSC Group Ranking#

MSSC_ranking = bind_rows(MSSC_Elm_ranking, MSSC_HS_ranking)

write_xlsx(MSSC_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/MSSC_ranking.xlsx")

jpeg(file="MSSC_ranking_plot.jpeg")
plot(MSSC_ranking$scale_1_5,MSSC_ranking$min_sum)
dev.off()