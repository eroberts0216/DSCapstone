library(writexl)
library(readxl)
library(tidyverse)

#Elementary
GenEd_Elm_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/GenEd.xlsx", sheet = "ElmGenEd")
colnames(GenEd_Elm_data) [1] = "grade_span_n"
colnames(GenEd_Elm_data) [2] = "position_n"

#conversions
GenEd_Elm_data = mutate(GenEd_Elm_data, meetings_504_c = GenEd_Elm_data$`Enter the approximate number of 504 meetings you have attended this year.`*60)
GenEd_Elm_data = GenEd_Elm_data %>% select(-c(`Enter the approximate number of 504 meetings you have attended this year.`))

GenEd_Elm_data = mutate(GenEd_Elm_data, district_IEP_c = GenEd_Elm_data$`Enter the number of District Staff Involved IEP meetings that you attended this school year.`*120)
GenEd_Elm_data = GenEd_Elm_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you attended this school year.`))

GenEd_Elm_data = mutate(GenEd_Elm_data, IEP_meeting_c = GenEd_Elm_data$`Enter the number of IEP meetings that you attended this school year.`*60)
GenEd_Elm_data = GenEd_Elm_data %>% select(-c(`Enter the number of IEP meetings that you attended this school year.`))

GenEd_Elm_data = mutate(GenEd_Elm_data, different_levels_c = GenEd_Elm_data$`Enter the number of grade level curriculums/subjects you are required to teach in a SINGLE class.`*9000)
GenEd_Elm_data = GenEd_Elm_data %>% select(-c(`Enter the number of grade level curriculums/subjects you are required to teach in a SINGLE class.`))

GenEd_Elm_data = mutate(GenEd_Elm_data, different_subjects_c = GenEd_Elm_data$`Enter the number of different subjects taught (Preps) in a single day or for blocked sites, over the two days. (For example: US History, World History, AP Gov/Econ would equal 3)`*9000)
GenEd_Elm_data = GenEd_Elm_data %>% select(-c(`Enter the number of different subjects taught (Preps) in a single day or for blocked sites, over the two days. (For example: US History, World History, AP Gov/Econ would equal 3)`))

GenEd_Elm_data = mutate(GenEd_Elm_data, pulled_days_c = GenEd_Elm_data$`Enter the number of days you have been pulled out of the classroom this year.`* 60)
GenEd_Elm_data = GenEd_Elm_data %>% select(-c(`Enter the number of days you have been pulled out of the classroom this year.`))

GenEd_Elm_data = mutate(GenEd_Elm_data, discipline_referrals_c = GenEd_Elm_data$`Enter the approximate number of Discipline Referrals you have written this year.`* 5)
GenEd_Elm_data = GenEd_Elm_data %>% select(-c(`Enter the approximate number of Discipline Referrals you have written this year.`))

GenEd_Elm_data = mutate(GenEd_Elm_data, independent_study_c = GenEd_Elm_data$`Enter the approximate number of Independent Study Agreements you have written and approved this year.`* 60)
GenEd_Elm_data = GenEd_Elm_data %>% select(-c(`Enter the approximate number of Independent Study Agreements you have written and approved this year.`))

GenEd_Elm_data = mutate(GenEd_Elm_data, RFA_c = GenEd_Elm_data$`Enter the number of Request for Assistance (RFAs) you have submitted this year.`* 60)
GenEd_Elm_data = GenEd_Elm_data %>% select(-c(`Enter the number of Request for Assistance (RFAs) you have submitted this year.`))

GenEd_Elm_data = mutate(GenEd_Elm_data, ELPAC_c = GenEd_Elm_data$`Enter the number of students you administered the one to one ELPAC/Alt-ELPAC Testing with this year.`* 20)
GenEd_Elm_data = GenEd_Elm_data %>% select(-c(`Enter the number of students you administered the one to one ELPAC/Alt-ELPAC Testing with this year.`))

GenEd_Elm_data = mutate(GenEd_Elm_data, DRDP_c = GenEd_Elm_data$`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`* 45)
GenEd_Elm_data = GenEd_Elm_data %>% select(-c(`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`))

GenEd_Elm_data = mutate(GenEd_Elm_data, district_asessments_c = GenEd_Elm_data$`Enter the number of students you administered one to one District Mandated Assessments with.`* 60)
GenEd_Elm_data = GenEd_Elm_data %>% select(-c(`Enter the number of students you administered one to one District Mandated Assessments with.`))

GenEd_Elm_data = mutate(GenEd_Elm_data, rating_scale_c = GenEd_Elm_data$`Enter the approximate number of Rating Scales  (FBA, Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`* 45)
GenEd_Elm_data = GenEd_Elm_data %>% select(-c(`Enter the approximate number of Rating Scales  (FBA, Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`))

GenEd_Elm_data = mutate(GenEd_Elm_data, students_data_c = GenEd_Elm_data$`Enter the number of students you have collected data on to determine if the student requires para support.`* 120)
GenEd_Elm_data = GenEd_Elm_data %>% select(-c(`Enter the number of students you have collected data on to determine if the student requires para support.`))

GenEd_Elm_data = mutate(GenEd_Elm_data, paras_3.5_c = GenEd_Elm_data$`Enter the number of 3.5-hour paras requiring direction that you facilitate.`* -2700)
GenEd_Elm_data = GenEd_Elm_data %>% select(-c(`Enter the number of 3.5-hour paras requiring direction that you facilitate.`))

GenEd_Elm_data = mutate(GenEd_Elm_data, staff_meeting_c = GenEd_Elm_data$`Enter the number of staffing meetings that you attended this school year.`* 60)
GenEd_Elm_data = GenEd_Elm_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

GenEd_Elm_data = mutate(GenEd_Elm_data, mainfestation_meeting_c = GenEd_Elm_data$`Enter the number of manifestation determination meetings that you attended this school year.`* 60)
GenEd_Elm_data = GenEd_Elm_data %>% select(-c(`Enter the number of manifestation determination meetings that you attended this school year.`))

GenEd_Elm_data = mutate(GenEd_Elm_data, transition_meeting_c = GenEd_Elm_data$`Enter the number of Transition Meetings that you anticipate attending this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`* 60)
GenEd_Elm_data = GenEd_Elm_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate attending this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

GenEd_Elm_data = mutate(GenEd_Elm_data, paras_7_c = GenEd_Elm_data$`Enter the number of 7-hour paras requiring direction that you facilitate.`* 5400)
GenEd_Elm_data = GenEd_Elm_data %>% select(-c(`Enter the number of 7-hour paras requiring direction that you facilitate.`))

GenEd_Elm_data = mutate(GenEd_Elm_data, paras_unfilled_c = GenEd_Elm_data$`Enter the number of unfilled para positions or para positions filled with contracted paras.`* 10800)
GenEd_Elm_data = GenEd_Elm_data %>% select(-c(`Enter the number of unfilled para positions or para positions filled with contracted paras.`))

GenEd_Elm_data = mutate(GenEd_Elm_data, BER_report_c = GenEd_Elm_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`* 15)
GenEd_Elm_data = GenEd_Elm_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

#creating the rankings

GenEd_Elm_ranking = transmute(GenEd_Elm_data, scale_1_5 = (GenEd_Elm_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
GenEd_Elm_ranking = mutate(GenEd_Elm_ranking, min_sum = rowSums(GenEd_Elm_data[ ,c(15,18:38)]))

write_xlsx(GenEd_Elm_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/GenEd_Elm_ranking.xlsx")

jpeg(file="GenEd_Elm_ranking_plot.jpeg")
plot(GenEd_Elm_ranking$scale_1_5,GenEd_Elm_ranking$min_sum)
dev.off()





#Intermediate
GenEd_Int_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/GenEd.xlsx", sheet = "IntGenEd")
colnames(GenEd_Int_data) [1] = "grade_span_n"
colnames(GenEd_Int_data) [2] = "position_n"

#conversions
GenEd_Int_data = mutate(GenEd_Int_data, meetings_504_c = GenEd_Int_data$`Enter the approximate number of 504 meetings you have attended this year.`*60)
GenEd_Int_data = GenEd_Int_data %>% select(-c(`Enter the approximate number of 504 meetings you have attended this year.`))

GenEd_Int_data = mutate(GenEd_Int_data, district_IEP_c = GenEd_Int_data$`Enter the number of District Staff Involved IEP meetings that you attended this school year.`*120)
GenEd_Int_data = GenEd_Int_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you attended this school year.`))

GenEd_Int_data = mutate(GenEd_Int_data, IEP_meeting_c = GenEd_Int_data$`Enter the number of IEP meetings that you attended this school year.`*60)
GenEd_Int_data = GenEd_Int_data %>% select(-c(`Enter the number of IEP meetings that you attended this school year.`))

GenEd_Int_data = mutate(GenEd_Int_data, different_levels_c = GenEd_Int_data$`Enter the number of grade level curriculums/subjects you are required to teach in a SINGLE class.`*9000)
GenEd_Int_data = GenEd_Int_data %>% select(-c(`Enter the number of grade level curriculums/subjects you are required to teach in a SINGLE class.`))

GenEd_Int_data = mutate(GenEd_Int_data, different_subjects_c = GenEd_Int_data$`Enter the number of different subjects taught (Preps) in a single day or for blocked sites, over the two days. (For example: US History, World History, AP Gov/Econ would equal 3)`*9000)
GenEd_Int_data = GenEd_Int_data %>% select(-c(`Enter the number of different subjects taught (Preps) in a single day or for blocked sites, over the two days. (For example: US History, World History, AP Gov/Econ would equal 3)`))

GenEd_Int_data = mutate(GenEd_Int_data, pulled_days_c = GenEd_Int_data$`Enter the number of days you have been pulled out of the classroom this year.`* 60)
GenEd_Int_data = GenEd_Int_data %>% select(-c(`Enter the number of days you have been pulled out of the classroom this year.`))

GenEd_Int_data = mutate(GenEd_Int_data, discipline_referrals_c = GenEd_Int_data$`Enter the approximate number of Discipline Referrals you have written this year.`* 5)
GenEd_Int_data = GenEd_Int_data %>% select(-c(`Enter the approximate number of Discipline Referrals you have written this year.`))

GenEd_Int_data = mutate(GenEd_Int_data, independent_study_c = GenEd_Int_data$`Enter the approximate number of Independent Study Agreements you have written and approved this year.`* 60)
GenEd_Int_data = GenEd_Int_data %>% select(-c(`Enter the approximate number of Independent Study Agreements you have written and approved this year.`))

GenEd_Int_data = mutate(GenEd_Int_data, RFA_c = GenEd_Int_data$`Enter the number of Request for Assistance (RFAs) you have submitted this year.`* 60)
GenEd_Int_data = GenEd_Int_data %>% select(-c(`Enter the number of Request for Assistance (RFAs) you have submitted this year.`))

GenEd_Int_data = mutate(GenEd_Int_data, ELPAC_c = GenEd_Int_data$`Enter the number of students you administered the one to one ELPAC/Alt-ELPAC Testing with this year.`* 20)
GenEd_Int_data = GenEd_Int_data %>% select(-c(`Enter the number of students you administered the one to one ELPAC/Alt-ELPAC Testing with this year.`))

GenEd_Int_data = mutate(GenEd_Int_data, DRDP_c = GenEd_Int_data$`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`* 45)
GenEd_Int_data = GenEd_Int_data %>% select(-c(`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`))

GenEd_Int_data = mutate(GenEd_Int_data, district_asessments_c = GenEd_Int_data$`Enter the number of students you administered one to one District Mandated Assessments with.`* 60)
GenEd_Int_data = GenEd_Int_data %>% select(-c(`Enter the number of students you administered one to one District Mandated Assessments with.`))

GenEd_Int_data = mutate(GenEd_Int_data, rating_scale_c = GenEd_Int_data$`Enter the approximate number of Rating Scales  (FBA, Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`* 45)
GenEd_Int_data = GenEd_Int_data %>% select(-c(`Enter the approximate number of Rating Scales  (FBA, Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`))

GenEd_Int_data = mutate(GenEd_Int_data, students_data_c = GenEd_Int_data$`Enter the number of students you have collected data on to determine if the student requires para support.`* 120)
GenEd_Int_data = GenEd_Int_data %>% select(-c(`Enter the number of students you have collected data on to determine if the student requires para support.`))

GenEd_Int_data = mutate(GenEd_Int_data, paras_3.5_c = GenEd_Int_data$`Enter the number of 3.5-hour paras requiring direction that you facilitate.`* -2700)
GenEd_Int_data = GenEd_Int_data %>% select(-c(`Enter the number of 3.5-hour paras requiring direction that you facilitate.`))

GenEd_Int_data = mutate(GenEd_Int_data, staff_meeting_c = GenEd_Int_data$`Enter the number of staffing meetings that you attended this school year.`* 60)
GenEd_Int_data = GenEd_Int_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

GenEd_Int_data = mutate(GenEd_Int_data, mainfestation_meeting_c = GenEd_Int_data$`Enter the number of manifestation determination meetings that you attended this school year.`* 60)
GenEd_Int_data = GenEd_Int_data %>% select(-c(`Enter the number of manifestation determination meetings that you attended this school year.`))

GenEd_Int_data = mutate(GenEd_Int_data, transition_meeting_c = GenEd_Int_data$`Enter the number of Transition Meetings that you anticipate attending this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`* 60)
GenEd_Int_data = GenEd_Int_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate attending this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

GenEd_Int_data = mutate(GenEd_Int_data, paras_7_c = GenEd_Int_data$`Enter the number of 7-hour paras requiring direction that you facilitate.`* 5400)
GenEd_Int_data = GenEd_Int_data %>% select(-c(`Enter the number of 7-hour paras requiring direction that you facilitate.`))

GenEd_Int_data = mutate(GenEd_Int_data, paras_unfilled_c = GenEd_Int_data$`Enter the number of unfilled para positions or para positions filled with contracted paras.`* 10800)
GenEd_Int_data = GenEd_Int_data %>% select(-c(`Enter the number of unfilled para positions or para positions filled with contracted paras.`))

GenEd_Int_data = mutate(GenEd_Int_data, BER_report_c = GenEd_Int_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`* 15)
GenEd_Int_data = GenEd_Int_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

#creating the rankings

GenEd_Int_ranking = transmute(GenEd_Int_data, scale_1_5 = (GenEd_Int_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
GenEd_Int_ranking = mutate(GenEd_Int_ranking, min_sum = rowSums(GenEd_Int_data[ ,c(15,18:38)]))

write_xlsx(GenEd_Int_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/GenEd_Int_ranking.xlsx")

jpeg(file="GenEd_Int_ranking_plot.jpeg")
plot(GenEd_Int_ranking$scale_1_5,GenEd_Int_ranking$min_sum)
dev.off()





#High School
GenEd_HS_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/GenEd.xlsx", sheet = "HSGenEd")
colnames(GenEd_HS_data) [1] = "grade_span_n"
colnames(GenEd_HS_data) [2] = "position_n"

#conversions
GenEd_HS_data = mutate(GenEd_HS_data, meetings_504_c = GenEd_HS_data$`Enter the approximate number of 504 meetings you have attended this year.`*60)
GenEd_HS_data = GenEd_HS_data %>% select(-c(`Enter the approximate number of 504 meetings you have attended this year.`))

GenEd_HS_data = mutate(GenEd_HS_data, district_IEP_c = GenEd_HS_data$`Enter the number of District Staff Involved IEP meetings that you attended this school year.`*120)
GenEd_HS_data = GenEd_HS_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you attended this school year.`))

GenEd_HS_data = mutate(GenEd_HS_data, IEP_meeting_c = GenEd_HS_data$`Enter the number of IEP meetings that you attended this school year.`*60)
GenEd_HS_data = GenEd_HS_data %>% select(-c(`Enter the number of IEP meetings that you attended this school year.`))

GenEd_HS_data = mutate(GenEd_HS_data, different_levels_c = GenEd_HS_data$`Enter the number of grade level curriculums/subjects you are required to teach in a SINGLE class.`*9000)
GenEd_HS_data = GenEd_HS_data %>% select(-c(`Enter the number of grade level curriculums/subjects you are required to teach in a SINGLE class.`))

GenEd_HS_data = mutate(GenEd_HS_data, different_subjects_c = GenEd_HS_data$`Enter the number of different subjects taught (Preps) in a single day or for blocked sites, over the two days. (For example: US History, World History, AP Gov/Econ would equal 3)`*9000)
GenEd_HS_data = GenEd_HS_data %>% select(-c(`Enter the number of different subjects taught (Preps) in a single day or for blocked sites, over the two days. (For example: US History, World History, AP Gov/Econ would equal 3)`))

GenEd_HS_data = mutate(GenEd_HS_data, pulled_days_c = GenEd_HS_data$`Enter the number of days you have been pulled out of the classroom this year.`* 60)
GenEd_HS_data = GenEd_HS_data %>% select(-c(`Enter the number of days you have been pulled out of the classroom this year.`))

GenEd_HS_data = mutate(GenEd_HS_data, discipline_referrals_c = GenEd_HS_data$`Enter the approximate number of Discipline Referrals you have written this year.`* 5)
GenEd_HS_data = GenEd_HS_data %>% select(-c(`Enter the approximate number of Discipline Referrals you have written this year.`))

GenEd_HS_data = mutate(GenEd_HS_data, independent_study_c = GenEd_HS_data$`Enter the approximate number of Independent Study Agreements you have written and approved this year.`* 60)
GenEd_HS_data = GenEd_HS_data %>% select(-c(`Enter the approximate number of Independent Study Agreements you have written and approved this year.`))

GenEd_HS_data = mutate(GenEd_HS_data, RFA_c = GenEd_HS_data$`Enter the number of Request for Assistance (RFAs) you have submitted this year.`* 60)
GenEd_HS_data = GenEd_HS_data %>% select(-c(`Enter the number of Request for Assistance (RFAs) you have submitted this year.`))

GenEd_HS_data = mutate(GenEd_HS_data, ELPAC_c = GenEd_HS_data$`Enter the number of students you administered the one to one ELPAC/Alt-ELPAC Testing with this year.`* 20)
GenEd_HS_data = GenEd_HS_data %>% select(-c(`Enter the number of students you administered the one to one ELPAC/Alt-ELPAC Testing with this year.`))

GenEd_HS_data = mutate(GenEd_HS_data, DRDP_c = GenEd_HS_data$`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`* 45)
GenEd_HS_data = GenEd_HS_data %>% select(-c(`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`))

GenEd_HS_data = mutate(GenEd_HS_data, district_asessments_c = GenEd_HS_data$`Enter the number of students you administered one to one District Mandated Assessments with.`* 60)
GenEd_HS_data = GenEd_HS_data %>% select(-c(`Enter the number of students you administered one to one District Mandated Assessments with.`))

GenEd_HS_data = mutate(GenEd_HS_data, rating_scale_c = GenEd_HS_data$`Enter the approximate number of Rating Scales  (FBA, Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`* 45)
GenEd_HS_data = GenEd_HS_data %>% select(-c(`Enter the approximate number of Rating Scales  (FBA, Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`))

GenEd_HS_data = mutate(GenEd_HS_data, students_data_c = GenEd_HS_data$`Enter the number of students you have collected data on to determine if the student requires para support.`* 120)
GenEd_HS_data = GenEd_HS_data %>% select(-c(`Enter the number of students you have collected data on to determine if the student requires para support.`))

GenEd_HS_data = mutate(GenEd_HS_data, paras_3.5_c = GenEd_HS_data$`Enter the number of 3.5-hour paras requiring direction that you facilitate.`* -2700)
GenEd_HS_data = GenEd_HS_data %>% select(-c(`Enter the number of 3.5-hour paras requiring direction that you facilitate.`))

GenEd_HS_data = mutate(GenEd_HS_data, staff_meeting_c = GenEd_HS_data$`Enter the number of staffing meetings that you attended this school year.`* 60)
GenEd_HS_data = GenEd_HS_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

GenEd_HS_data = mutate(GenEd_HS_data, mainfestation_meeting_c = GenEd_HS_data$`Enter the number of manifestation determination meetings that you attended this school year.`* 60)
GenEd_HS_data = GenEd_HS_data %>% select(-c(`Enter the number of manifestation determination meetings that you attended this school year.`))

GenEd_HS_data = mutate(GenEd_HS_data, transition_meeting_c = GenEd_HS_data$`Enter the number of Transition Meetings that you anticipate attending this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`* 60)
GenEd_HS_data = GenEd_HS_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate attending this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

GenEd_HS_data = mutate(GenEd_HS_data, paras_7_c = GenEd_HS_data$`Enter the number of 7-hour paras requiring direction that you facilitate.`* 5400)
GenEd_HS_data = GenEd_HS_data %>% select(-c(`Enter the number of 7-hour paras requiring direction that you facilitate.`))

GenEd_HS_data = mutate(GenEd_HS_data, paras_unfilled_c = GenEd_HS_data$`Enter the number of unfilled para positions or para positions filled with contracted paras.`* 10800)
GenEd_HS_data = GenEd_HS_data %>% select(-c(`Enter the number of unfilled para positions or para positions filled with contracted paras.`))

GenEd_HS_data = mutate(GenEd_HS_data, BER_report_c = GenEd_HS_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`* 15)
GenEd_HS_data = GenEd_HS_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

#creating the rankings

GenEd_HS_ranking = transmute(GenEd_HS_data, scale_1_5 = (GenEd_HS_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
GenEd_HS_ranking = mutate(GenEd_HS_ranking, min_sum = rowSums(GenEd_HS_data[ ,c(15,18:38)]))

write_xlsx(GenEd_HS_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/GenEd_HS_ranking.xlsx")

jpeg(file="GenEd_HS_ranking_plot.jpeg")
plot(GenEd_HS_ranking$scale_1_5,GenEd_HS_ranking$min_sum)
dev.off()





#GenEd Group Ranking#

GenEd_ranking = bind_rows(GenEd_Elm_ranking, GenEd_Int_ranking, GenEd_HS_ranking)

write_xlsx(GenEd_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/GenEd_ranking.xlsx")

jpeg(file="GenEd_ranking_plot.jpeg")
plot(GenEd_ranking$scale_1_5,GenEd_ranking$min_sum)
dev.off()
