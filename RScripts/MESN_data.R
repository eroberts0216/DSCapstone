library(writexl)
library(readxl)
library(tidyverse)

#Elementary
MESN_Elm_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/MESN.xlsx", sheet = "ElmMESN")
colnames(MESN_Elm_data) [1] = "grade_span_n"
colnames(MESN_Elm_data) [2] = "position_n"

#conversions
MESN_Elm_data = mutate(MESN_Elm_data, total_class_contacts_c = MESN_Elm_data$`Enter the number of students on your caseload with 1-5 accommodations.` +MESN_Elm_data$`Enter the number of students on your caseload with 6 or more accommodations.`)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of students on your caseload with 1-5 accommodations.`,`Enter the number of students on your caseload with 6 or more accommodations.`))

MESN_Elm_data = mutate(MESN_Elm_data, student_data_c = MESN_Elm_data$`Enter the number of students you have collected data on to determine if the student requires para support.`*120)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of students you have collected data on to determine if the student requires para support.`))

MESN_Elm_data = mutate(MESN_Elm_data, present_levels_c = MESN_Elm_data$`Enter the number of hours this year you have spent writing present levels this school year.`*60)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of hours this year you have spent writing present levels this school year.`))

MESN_Elm_data = mutate(MESN_Elm_data, progress_goals_c = MESN_Elm_data$`Enter the number of students you wrote progress on goals for this year.`*360)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of students you wrote progress on goals for this year.`))

MESN_Elm_data = mutate(MESN_Elm_data, different_subjects_c = MESN_Elm_data$`Enter the number of different subjects taught (Preps) in a single day or for blocked sites, over the two days.  (For example: US History, World History, AP Gov/Econ would equal 3)`*9000)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of different subjects taught (Preps) in a single day or for blocked sites, over the two days.  (For example: US History, World History, AP Gov/Econ would equal 3)`))

MESN_Elm_data = mutate(MESN_Elm_data, different_grades_c = MESN_Elm_data$`Enter the number of grade level curriculums/subjects you are required to teach in a SINGLE class.`*9000)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of grade level curriculums/subjects you are required to teach in a SINGLE class.`))

MESN_Elm_data = mutate(MESN_Elm_data, DRDP_c = MESN_Elm_data$`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`* 45)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`))

MESN_Elm_data = mutate(MESN_Elm_data, rating_scale_c = MESN_Elm_data$`Enter the number of Rating Scales  (FBA,  Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`* 45)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of Rating Scales  (FBA,  Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`))

MESN_Elm_data = mutate(MESN_Elm_data, district_asessments_c = MESN_Elm_data$`Enter the number of students you administered one to one District Mandated Assessments with.`* 60)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of students you administered one to one District Mandated Assessments with.`))

MESN_Elm_data = mutate(MESN_Elm_data, ELPAC_c = MESN_Elm_data$`Enter the number of students you administered the one to one ELPAC/Alt-ELPAC Testing with this year.`* 20)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of students you administered the one to one ELPAC/Alt-ELPAC Testing with this year.`))

MESN_Elm_data = mutate(MESN_Elm_data, meetings_504_c = MESN_Elm_data$`Enter the number of 504 meetings you have attended this year.`*60)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of 504 meetings you have attended this year.`))

MESN_Elm_data = mutate(MESN_Elm_data, initial_c = MESN_Elm_data$`Enter the number of initials that you held this year.`*180)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of initials that you held this year.`))

MESN_Elm_data = mutate(MESN_Elm_data, district_IEP_c = MESN_Elm_data$`Enter the number of District Staff Involved IEP meetings that you held this school year.`*480)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you held this school year.`))

MESN_Elm_data = mutate(MESN_Elm_data, annual_IEP_c = MESN_Elm_data$`Enter the number of annual IEP meetings that you held this school year.`* 180)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of annual IEP meetings that you held this school year.`))

MESN_Elm_data = mutate(MESN_Elm_data, triennial_IEP_c = MESN_Elm_data$`Enter the number of Triennial IEP meetings that you held this school year.`* 360)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of Triennial IEP meetings that you held this school year.`))

MESN_Elm_data = mutate(MESN_Elm_data, staff_meeting_c = MESN_Elm_data$`Enter the number of staffing meetings that you attended this school year.`* 60)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

MESN_Elm_data = mutate(MESN_Elm_data, amendment_meeting_c = MESN_Elm_data$`Enter the number of amendment meetings that you held this school year.`* 90)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of amendment meetings that you held this school year.`))

MESN_Elm_data = mutate(MESN_Elm_data, mainfestation_meeting_c = MESN_Elm_data$`Enter the number of manifestation determination meetings that you held this school year.`* 240)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of manifestation determination meetings that you held this school year.`))

MESN_Elm_data = mutate(MESN_Elm_data, transition_meeting_c = MESN_Elm_data$`Enter the number of Transition Meetings that you anticipate holding this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`* 120)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate holding this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

MESN_Elm_data = mutate(MESN_Elm_data, interim_IEP_c = MESN_Elm_data$`Enter the number of Interim IEPs that you held this school year.`* 90)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of Interim IEPs that you held this school year.`))

MESN_Elm_data = mutate(MESN_Elm_data, discipline_referrals_c = MESN_Elm_data$`Enter the number of Discipline Referrals you have written this year.`* 5)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of Discipline Referrals you have written this year.`))

MESN_Elm_data = mutate(MESN_Elm_data, independent_study_c = MESN_Elm_data$`Enter the number of Independent Study Agreements you have written and approved this year.`* 60)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of Independent Study Agreements you have written and approved this year.`))

MESN_Elm_data = mutate(MESN_Elm_data, paras_7_c = MESN_Elm_data$`Enter the number of 7-hour paras requiring direction that you facilitate.`* 5400)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of 7-hour paras requiring direction that you facilitate.`))

MESN_Elm_data = mutate(MESN_Elm_data, paras_unfilled_c = MESN_Elm_data$`Enter the number of unfilled para positions or para positions filled with contracted paras.`* 10800)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of unfilled para positions or para positions filled with contracted paras.`))

MESN_Elm_data = mutate(MESN_Elm_data, BER_report_c = MESN_Elm_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`* 15)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

MESN_Elm_data = mutate(MESN_Elm_data, pulled_days_c = MESN_Elm_data$`Enter the number of days you have been pulled out of the classroom this year.`* 60)
MESN_Elm_data = MESN_Elm_data %>% select(-c(`Enter the number of days you have been pulled out of the classroom this year.`))



#creating the rankings

MESN_Elm_ranking = transmute(MESN_Elm_data, scale_1_5 = (MESN_Elm_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
MESN_Elm_ranking = mutate(MESN_Elm_ranking, min_sum = rowSums(MESN_Elm_data[ ,c(15,18:38)]))

write_xlsx(MESN_Elm_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/MESN_Elm_ranking.xlsx")

jpeg(file="MESN_Elm_ranking_plot.jpeg")
plot(MESN_Elm_ranking$scale_1_5,MESN_Elm_ranking$min_sum)
dev.off()





#Intermediate
MESN_Int_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/MESN.xlsx", sheet = "IntMESN")
colnames(MESN_Int_data) [1] = "grade_span_n"
colnames(MESN_Int_data) [2] = "position_n"

#conversions
MESN_Int_data = mutate(MESN_Int_data, total_class_contacts_c = MESN_Int_data$`Enter the number of students on your caseload with 1-5 accommodations.` +MESN_Int_data$`Enter the number of students on your caseload with 6 or more accommodations.`)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of students on your caseload with 1-5 accommodations.`,`Enter the number of students on your caseload with 6 or more accommodations.`))

MESN_Int_data = mutate(MESN_Int_data, student_data_c = MESN_Int_data$`Enter the number of students you have collected data on to determine if the student requires para support.`*120)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of students you have collected data on to determine if the student requires para support.`))

MESN_Int_data = mutate(MESN_Int_data, present_levels_c = MESN_Int_data$`Enter the number of hours this year you have spent writing present levels this school year.`*60)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of hours this year you have spent writing present levels this school year.`))

MESN_Int_data = mutate(MESN_Int_data, progress_goals_c = MESN_Int_data$`Enter the number of students you wrote progress on goals for this year.`*270)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of students you wrote progress on goals for this year.`))

MESN_Int_data = mutate(MESN_Int_data, different_subjects_c = MESN_Int_data$`Enter the number of different subjects taught (Preps) in a single day or for blocked sites, over the two days.  (For example: US History, World History, AP Gov/Econ would equal 3)`*9000)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of different subjects taught (Preps) in a single day or for blocked sites, over the two days.  (For example: US History, World History, AP Gov/Econ would equal 3)`))

MESN_Int_data = mutate(MESN_Int_data, different_grades_c = MESN_Int_data$`Enter the number of grade level curriculums/subjects you are required to teach in a SINGLE class.`*9000)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of grade level curriculums/subjects you are required to teach in a SINGLE class.`))

MESN_Int_data = mutate(MESN_Int_data, DRDP_c = MESN_Int_data$`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`* 45)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`))

MESN_Int_data = mutate(MESN_Int_data, rating_scale_c = MESN_Int_data$`Enter the number of Rating Scales  (FBA,  Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`* 45)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of Rating Scales  (FBA,  Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`))

MESN_Int_data = mutate(MESN_Int_data, district_asessments_c = MESN_Int_data$`Enter the number of students you administered one to one District Mandated Assessments with.`* 60)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of students you administered one to one District Mandated Assessments with.`))

MESN_Int_data = mutate(MESN_Int_data, ELPAC_c = MESN_Int_data$`Enter the number of students you administered the one to one ELPAC/Alt-ELPAC Testing with this year.`* 20)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of students you administered the one to one ELPAC/Alt-ELPAC Testing with this year.`))

MESN_Int_data = mutate(MESN_Int_data, meetings_504_c = MESN_Int_data$`Enter the number of 504 meetings you have attended this year.`*60)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of 504 meetings you have attended this year.`))

MESN_Int_data = mutate(MESN_Int_data, initial_c = MESN_Int_data$`Enter the number of initials that you held this year.`*180)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of initials that you held this year.`))

MESN_Int_data = mutate(MESN_Int_data, district_IEP_c = MESN_Int_data$`Enter the number of District Staff Involved IEP meetings that you held this school year.`*480)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you held this school year.`))

MESN_Int_data = mutate(MESN_Int_data, annual_IEP_c = MESN_Int_data$`Enter the number of annual IEP meetings that you held this school year.`* 180)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of annual IEP meetings that you held this school year.`))

MESN_Int_data = mutate(MESN_Int_data, triennial_IEP_c = MESN_Int_data$`Enter the number of Triennial IEP meetings that you held this school year.`* 360)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of Triennial IEP meetings that you held this school year.`))

MESN_Int_data = mutate(MESN_Int_data, staff_meeting_c = MESN_Int_data$`Enter the number of staffing meetings that you attended this school year.`* 60)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

MESN_Int_data = mutate(MESN_Int_data, amendment_meeting_c = MESN_Int_data$`Enter the number of amendment meetings that you held this school year.`* 90)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of amendment meetings that you held this school year.`))

MESN_Int_data = mutate(MESN_Int_data, mainfestation_meeting_c = MESN_Int_data$`Enter the number of manifestation determination meetings that you held this school year.`* 240)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of manifestation determination meetings that you held this school year.`))

MESN_Int_data = mutate(MESN_Int_data, transition_meeting_c = MESN_Int_data$`Enter the number of Transition Meetings that you anticipate holding this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`* 120)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate holding this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

MESN_Int_data = mutate(MESN_Int_data, interim_IEP_c = MESN_Int_data$`Enter the number of Interim IEPs that you held this school year.`* 90)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of Interim IEPs that you held this school year.`))

MESN_Int_data = mutate(MESN_Int_data, discipline_referrals_c = MESN_Int_data$`Enter the number of Discipline Referrals you have written this year.`* 5)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of Discipline Referrals you have written this year.`))

MESN_Int_data = mutate(MESN_Int_data, independent_study_c = MESN_Int_data$`Enter the number of Independent Study Agreements you have written and approved this year.`* 60)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of Independent Study Agreements you have written and approved this year.`))

MESN_Int_data = mutate(MESN_Int_data, paras_7_c = MESN_Int_data$`Enter the number of 7-hour paras requiring direction that you facilitate.`* 5400)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of 7-hour paras requiring direction that you facilitate.`))

MESN_Int_data = mutate(MESN_Int_data, paras_unfilled_c = MESN_Int_data$`Enter the number of unfilled para positions or para positions filled with contracted paras.`* 10800)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of unfilled para positions or para positions filled with contracted paras.`))

MESN_Int_data = mutate(MESN_Int_data, BER_report_c = MESN_Int_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`* 15)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

MESN_Int_data = mutate(MESN_Int_data, pulled_days_c = MESN_Int_data$`Enter the number of days you have been pulled out of the classroom this year.`* 60)
MESN_Int_data = MESN_Int_data %>% select(-c(`Enter the number of days you have been pulled out of the classroom this year.`))



#creating the rankings

MESN_Int_ranking = transmute(MESN_Int_data, scale_1_5 = (MESN_Int_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
MESN_Int_ranking = mutate(MESN_Int_ranking, min_sum = rowSums(MESN_Int_data[ ,c(15,18:38)]))

write_xlsx(MESN_Int_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/MESN_Int_ranking.xlsx")

jpeg(file="MESN_Int_ranking_plot.jpeg")
plot(MESN_Int_ranking$scale_1_5,MESN_Int_ranking$min_sum)
dev.off()





#High School
MESN_HS_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/MESN.xlsx", sheet = "HSMESN")
colnames(MESN_HS_data) [1] = "grade_span_n"
colnames(MESN_HS_data) [2] = "position_n"

#conversions
MESN_HS_data = mutate(MESN_HS_data, total_class_contacts_c = MESN_HS_data$`Enter the number of students on your caseload with 1-5 accommodations.` +MESN_HS_data$`Enter the number of students on your caseload with 6 or more accommodations.`)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of students on your caseload with 1-5 accommodations.`,`Enter the number of students on your caseload with 6 or more accommodations.`))

MESN_HS_data = mutate(MESN_HS_data, student_data_c = MESN_HS_data$`Enter the number of students you have collected data on to determine if the student requires para support.`*120)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of students you have collected data on to determine if the student requires para support.`))

MESN_HS_data = mutate(MESN_HS_data, present_levels_c = MESN_HS_data$`Enter the number of hours this year you have spent writing present levels this school year.`*60)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of hours this year you have spent writing present levels this school year.`))

MESN_HS_data = mutate(MESN_HS_data, progress_goals_c = MESN_HS_data$`Enter the number of students you wrote progress on goals for this year.`*270)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of students you wrote progress on goals for this year.`))

MESN_HS_data = mutate(MESN_HS_data, different_subjects_c = MESN_HS_data$`Enter the number of different subjects taught (Preps) in a single day or for blocked sites, over the two days.  (For example: US History, World History, AP Gov/Econ would equal 3)`*9000)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of different subjects taught (Preps) in a single day or for blocked sites, over the two days.  (For example: US History, World History, AP Gov/Econ would equal 3)`))

MESN_HS_data = mutate(MESN_HS_data, different_grades_c = MESN_HS_data$`Enter the number of grade level curriculums/subjects you are required to teach in a SINGLE class.`*9000)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of grade level curriculums/subjects you are required to teach in a SINGLE class.`))

MESN_HS_data = mutate(MESN_HS_data, DRDP_c = MESN_HS_data$`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`* 45)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`))

MESN_HS_data = mutate(MESN_HS_data, rating_scale_c = MESN_HS_data$`Enter the number of Rating Scales  (FBA,  Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`* 45)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of Rating Scales  (FBA,  Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`))

MESN_HS_data = mutate(MESN_HS_data, district_asessments_c = MESN_HS_data$`Enter the number of students you administered one to one District Mandated Assessments with.`* 60)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of students you administered one to one District Mandated Assessments with.`))

MESN_HS_data = mutate(MESN_HS_data, ELPAC_c = MESN_HS_data$`Enter the number of students you administered the one to one ELPAC/Alt-ELPAC Testing with this year.`* 20)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of students you administered the one to one ELPAC/Alt-ELPAC Testing with this year.`))

MESN_HS_data = mutate(MESN_HS_data, meetings_504_c = MESN_HS_data$`Enter the number of 504 meetings you have attended this year.`*60)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of 504 meetings you have attended this year.`))

MESN_HS_data = mutate(MESN_HS_data, initial_c = MESN_HS_data$`Enter the number of initials that you held this year.`*180)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of initials that you held this year.`))

MESN_HS_data = mutate(MESN_HS_data, district_IEP_c = MESN_HS_data$`Enter the number of District Staff Involved IEP meetings that you held this school year.`*480)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you held this school year.`))

MESN_HS_data = mutate(MESN_HS_data, annual_IEP_c = MESN_HS_data$`Enter the number of annual IEP meetings that you held this school year.`* 180)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of annual IEP meetings that you held this school year.`))

MESN_HS_data = mutate(MESN_HS_data, triennial_IEP_c = MESN_HS_data$`Enter the number of Triennial IEP meetings that you held this school year.`* 360)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of Triennial IEP meetings that you held this school year.`))

MESN_HS_data = mutate(MESN_HS_data, staff_meeting_c = MESN_HS_data$`Enter the number of staffing meetings that you attended this school year.`* 60)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

MESN_HS_data = mutate(MESN_HS_data, amendment_meeting_c = MESN_HS_data$`Enter the number of amendment meetings that you held this school year.`* 90)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of amendment meetings that you held this school year.`))

MESN_HS_data = mutate(MESN_HS_data, mainfestation_meeting_c = MESN_HS_data$`Enter the number of manifestation determination meetings that you held this school year.`* 240)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of manifestation determination meetings that you held this school year.`))

MESN_HS_data = mutate(MESN_HS_data, transition_meeting_c = MESN_HS_data$`Enter the number of Transition Meetings that you anticipate holding this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`* 120)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate holding this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

MESN_HS_data = mutate(MESN_HS_data, interim_IEP_c = MESN_HS_data$`Enter the number of Interim IEPs that you held this school year.`* 90)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of Interim IEPs that you held this school year.`))

MESN_HS_data = mutate(MESN_HS_data, discipline_referrals_c = MESN_HS_data$`Enter the number of Discipline Referrals you have written this year.`* 5)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of Discipline Referrals you have written this year.`))

MESN_HS_data = mutate(MESN_HS_data, independent_study_c = MESN_HS_data$`Enter the number of Independent Study Agreements you have written and approved this year.`* 60)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of Independent Study Agreements you have written and approved this year.`))

MESN_HS_data = mutate(MESN_HS_data, paras_7_c = MESN_HS_data$`Enter the number of 7-hour paras requiring direction that you facilitate.`* 5400)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of 7-hour paras requiring direction that you facilitate.`))

MESN_HS_data = mutate(MESN_HS_data, paras_unfilled_c = MESN_HS_data$`Enter the number of unfilled para positions or para positions filled with contracted paras.`* 10800)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of unfilled para positions or para positions filled with contracted paras.`))

MESN_HS_data = mutate(MESN_HS_data, BER_report_c = MESN_HS_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`* 15)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

MESN_HS_data = mutate(MESN_HS_data, pulled_days_c = MESN_HS_data$`Enter the number of days you have been pulled out of the classroom this year.`* 60)
MESN_HS_data = MESN_HS_data %>% select(-c(`Enter the number of days you have been pulled out of the classroom this year.`))



#creating the rankings

MESN_HS_ranking = transmute(MESN_HS_data, scale_1_5 = (MESN_HS_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
MESN_HS_ranking = mutate(MESN_HS_ranking, min_sum = rowSums(MESN_HS_data[ ,c(15,18:38)]))

write_xlsx(MESN_HS_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/MESN_HS_ranking.xlsx")

jpeg(file="MESN_HS_ranking_plot.jpeg")
plot(MESN_HS_ranking$scale_1_5,MESN_HS_ranking$min_sum)
dev.off()





#MESN Group Ranking#

MESN_ranking = bind_rows(MESN_Elm_ranking, MESN_Int_ranking, MESN_HS_ranking)

write_xlsx(MESN_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/MESN_ranking.xlsx")

jpeg(file="MESN_ranking_plot.jpeg")
plot(MESN_ranking$scale_1_5,MESN_ranking$min_sum)
dev.off()