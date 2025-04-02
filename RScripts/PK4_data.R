library(writexl)
library(readxl)
library(tidyverse)


#Pre-School#
PK4_PS_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/PK4.xlsx", sheet = "PK4")
colnames(PK4_PS_data) [1] = "grade_span_n"
colnames(PK4_PS_data) [2] = "position_n"

#conversions
PK4_PS_data = mutate(PK4_PS_data, total_class_contacts_c = PK4_PS_data$`Enter the number of students on your caseload with 1-5 accommodations.`+ PK4_PS_data$`Enter the number of students on your caseload with 6 or more accommodations.`)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of students on your caseload with 1-5 accommodations.`,`Enter the number of students on your caseload with 6 or more accommodations.`))

PK4_PS_data = mutate(PK4_PS_data, student_data_c = PK4_PS_data$`Enter the number of students you have collected data on to determine if the student requires para support.`*120)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of students you have collected data on to determine if the student requires para support.`))

PK4_PS_data = mutate(PK4_PS_data, progress_goals_c = PK4_PS_data$`Enter the number of students you wrote progress on goals for this year.`*360)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of students you wrote progress on goals for this year.`))

PK4_PS_data = mutate(PK4_PS_data, different_grade_c = PK4_PS_data$`Enter the number of grade level curriculums/subjects you are required to teach in a SINGLE class.`*9000)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of grade level curriculums/subjects you are required to teach in a SINGLE class.`))

PK4_PS_data = mutate(PK4_PS_data, present_levels_c = PK4_PS_data$`Enter the number of hours this year you have spent writing present levels this school year.`*60)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of hours this year you have spent writing present levels this school year.`))

PK4_PS_data = mutate(PK4_PS_data, ELPAC_c = PK4_PS_data$`Enter the number of students you administered the one to one ELPAC/Alt-ELPAC Testing with this year.`*20)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of students you administered the one to one ELPAC/Alt-ELPAC Testing with this year.`))

PK4_PS_data = mutate(PK4_PS_data, DRDP_c = PK4_PS_data$`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`*45)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`))

PK4_PS_data = mutate(PK4_PS_data, district_assessments_c = PK4_PS_data$`Enter the number of students you administered one to one District Mandated Assessments with.`*60)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of students you administered one to one District Mandated Assessments with.`))

PK4_PS_data = mutate(PK4_PS_data, rating_scale_c = PK4_PS_data$`Enter the number of Rating Scales  (FBA,  Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`*45)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of Rating Scales  (FBA,  Behavior Intervention Plan (BIP), teacher questionnaires, etc.) you have completed this year.`))

PK4_PS_data = mutate(PK4_PS_data, meeting_504_c = PK4_PS_data$`Enter the number of 504 meetings you have attended this year.`*60)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of 504 meetings you have attended this year.`))

PK4_PS_data = mutate(PK4_PS_data, initial_c = PK4_PS_data$`Enter the number of initials that you attended this year as a support provider.`*180)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of initials that you attended this year as a support provider.`))

PK4_PS_data = mutate(PK4_PS_data, district_IEP_c = PK4_PS_data$`Enter the number of District Staff Involved IEP meetings that you held this school year.`*480)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you held this school year.`))

PK4_PS_data = mutate(PK4_PS_data, annual_IEP_c = PK4_PS_data$`Enter the number of annual IEP meetings that you held this school year.`*180)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of annual IEP meetings that you held this school year.`))

PK4_PS_data = mutate(PK4_PS_data, triennial_IEP_c = PK4_PS_data$`Enter the number of Triennial IEP meetings that you held this school year.`*360)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of Triennial IEP meetings that you held this school year.`))

PK4_PS_data = mutate(PK4_PS_data, staff_meeting_c = PK4_PS_data$`Enter the number of staffing meetings that you attended this school year.`*60)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

PK4_PS_data = mutate(PK4_PS_data, amendment_meeting_c = PK4_PS_data$`Enter the number of amendment meetings that you held this school year.`*90)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of amendment meetings that you held this school year.`))

PK4_PS_data = mutate(PK4_PS_data, manifestation_meeting_c = PK4_PS_data$`Enter the number of manifestation determination meetings that you held this school year.`*240)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of manifestation determination meetings that you held this school year.`))

PK4_PS_data = mutate(PK4_PS_data, transition_meeting_c = PK4_PS_data$`Enter the number of Transition Meetings that you anticipate holding this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`*120)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate holding this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

PK4_PS_data = mutate(PK4_PS_data, interim_IEP_c = PK4_PS_data$`Enter the number of Interim IEPs that you held this school year.`*90)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of Interim IEPs that you held this school year.`))

PK4_PS_data = mutate(PK4_PS_data, discipline_referral_c = PK4_PS_data$`Enter the number of Discipline Referrals you have written this year.`*5)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of Discipline Referrals you have written this year.`))

PK4_PS_data = mutate(PK4_PS_data, amendment_meeting_c = PK4_PS_data$`Enter the number of Independent Study Agreements you have written and approved this year.`*60)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of Independent Study Agreements you have written and approved this year.`))

PK4_PS_data = mutate(PK4_PS_data, para_7_c = PK4_PS_data$`Enter the number of 7-hour paras requiring direction that you facilitate.`*5400)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of 7-hour paras requiring direction that you facilitate.`))

PK4_PS_data = mutate(PK4_PS_data, para_unfilled_c = PK4_PS_data$`Enter the number of unfilled para positions or para positions filled with contracted paras.`*10800)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of unfilled para positions or para positions filled with contracted paras.`))

PK4_PS_data = mutate(PK4_PS_data, BER_report_c = PK4_PS_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`*15)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

PK4_PS_data = mutate(PK4_PS_data, days_pulled_c = PK4_PS_data$`Enter the number of days you have been pulled out of the classroom this year.`*60)
PK4_PS_data = PK4_PS_data %>% select(-c(`Enter the number of days you have been pulled out of the classroom this year.`))



#creating the rankings

PK4_PS_ranking = transmute(PK4_PS_data, scale_1_5 = (PK4_PS_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
PK4_PS_ranking = mutate(PK4_PS_ranking, min_sum = rowSums(PK4_PS_data[ ,c(3,5:21)]))

write_xlsx(PK4_PS_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/PK4_PS_ranking.xlsx")

#jpeg(file="PK4_PS_ranking_plot.jpeg")
#plot(PK4_PS_ranking$scale_1_5,PK4_PS_ranking$min_sum)
#dev.off()