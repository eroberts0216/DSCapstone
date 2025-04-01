library(readxl)
library(tidyverse)

#Elementary
SLP_Elm_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/SLP.xlsx", sheet = "ElmSLP")
colnames(SLP_Elm_data) [1] = "grade_span_n"
colnames(SLP_Elm_data) [2] = "position_n"

#conversions
SLP_Elm_data = mutate(SLP_Elm_data, total_class_contacts_c = SLP_Elm_data$`Enter the number of students on your caseload with 1-5 accommodations.`+SLP_Elm_data$`Enter the number of students on your caseload with 6 or more accommodations.`)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of students on your caseload with 1-5 accommodations.`,`Enter the number of students on your caseload with 6 or more accommodations.`))

SLP_Elm_data = mutate(SLP_Elm_data, assessment_reports_c = SLP_Elm_data$`Enter the number of assessment reports you have completed this year.`*120)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of assessment reports you have completed this year.`))

SLP_Elm_data = mutate(SLP_Elm_data, meetings_504_c = SLP_Elm_data$`Enter the number of 504 meetings you have attended this year.`*60)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of 504 meetings you have attended this year.`))

SLP_Elm_data = mutate(SLP_Elm_data, inital_CC_c = SLP_Elm_data$`Enter the number of initials you held as a case carrier.`*180)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of initials you held as a case carrier.`))

SLP_Elm_data = mutate(SLP_Elm_data, inital_SP_c = SLP_Elm_data$`Enter the number of initials you attended as a service provider.`*60)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of initials you attended as a service provider.`))

SLP_Elm_data = mutate(SLP_Elm_data, district_IEP_CC_c = SLP_Elm_data$`Enter the number of District Staff Involved IEP meetings that you held this school year as a case carrier.`*480)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you held this school year as a case carrier.`))

SLP_Elm_data = mutate(SLP_Elm_data, district_IEP_SP_c = SLP_Elm_data$`Enter the number of District Staff Involved IEP meetings that you attended this school year as a service provider.`*240)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you attended this school year as a service provider.`))

SLP_Elm_data = mutate(SLP_Elm_data, annual_IEP_CC_c = SLP_Elm_data$`Enter the number of annual IEP meetings that you held this school year as a case carrier.`*180)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of annual IEP meetings that you held this school year as a case carrier.`))

SLP_Elm_data = mutate(SLP_Elm_data, annual_IEP_SP_c = SLP_Elm_data$`Enter the number of annual IEP meetings that you attended this school year as a service provider.`*90)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of annual IEP meetings that you attended this school year as a service provider.`))

SLP_Elm_data = mutate(SLP_Elm_data, triennial_IEP_CC_c = SLP_Elm_data$`Enter the number of Triennial IEP meetings that you held this school year as a case carrier.`*360)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of Triennial IEP meetings that you held this school year as a case carrier.`))

SLP_Elm_data = mutate(SLP_Elm_data, triennial_IEP_SP_c = SLP_Elm_data$`Enter the number of Triennial IEP meetings that you attended this school year as a service provider.`*120)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of Triennial IEP meetings that you attended this school year as a service provider.`))

SLP_Elm_data = mutate(SLP_Elm_data, staff_meeting_c = SLP_Elm_data$`Enter the number of staffing meetings that you attended this school year.`*60)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

SLP_Elm_data = mutate(SLP_Elm_data, amendment_meeting_CC_c = SLP_Elm_data$`Enter the number of amendment meetings that you held this school year as a case carrier.`*90)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of amendment meetings that you held this school year as a case carrier.`))

SLP_Elm_data = mutate(SLP_Elm_data, amendment_meeting_SP_c = SLP_Elm_data$`Enter the number of amendment meetings that you attended this school year as a service provider.`*60)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of amendment meetings that you attended this school year as a service provider.`))

SLP_Elm_data = mutate(SLP_Elm_data, manifestation_meeting_CC_c = SLP_Elm_data$`Enter the number of manifestation determination meetings that you held this school year as a case carrier.`*240)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of manifestation determination meetings that you held this school year as a case carrier.`))

SLP_Elm_data = mutate(SLP_Elm_data, manifestation_meeting_SP_c = SLP_Elm_data$`Enter the number of manifestation determination meetings that you attended this school year as a service provider.`*60)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of manifestation determination meetings that you attended this school year as a service provider.`))

SLP_Elm_data = mutate(SLP_Elm_data, transition_meeting_CC_c = SLP_Elm_data$`Enter the number of Transition Meetings that you anticipate holding this school year as a case carrier. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`*120)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate holding this school year as a case carrier. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

SLP_Elm_data = mutate(SLP_Elm_data, transition_meeting_SP_c = SLP_Elm_data$`Enter the number of Transition Meetings that you anticipate attending this school year as a service provider.  (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`*60)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate attending this school year as a service provider.  (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

SLP_Elm_data = mutate(SLP_Elm_data, interim_IEP_CC_c = SLP_Elm_data$`Enter the number of Interim IEPs that you held this school year as a case carrier.`*90)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of Interim IEPs that you held this school year as a case carrier.`))

SLP_Elm_data = mutate(SLP_Elm_data, interim_IEP_SP_c = SLP_Elm_data$`Enter the number of Interim IEPs that you attended this school year as a service provider.`*60)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of Interim IEPs that you attended this school year as a service provider.`))

SLP_Elm_data = mutate(SLP_Elm_data, BER_reports_c = SLP_Elm_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`*15)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

SLP_Elm_data = mutate(SLP_Elm_data, DRDP_assessment_c = SLP_Elm_data$`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`*45)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`))

SLP_Elm_data = mutate(SLP_Elm_data, days_pulled_c = SLP_Elm_data$`Enter the number of days you have been pulled from your duties this year.`*420)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of days you have been pulled from your duties this year.`))

SLP_Elm_data = mutate(SLP_Elm_data, sites_travel_c = SLP_Elm_data$`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`*5400)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`))

SLP_Elm_data = mutate(SLP_Elm_data, miles_travel_c = (SLP_Elm_data$`Enter the number of miles driven between sites in a typical month.`*2)+15)
SLP_Elm_data = SLP_Elm_data %>% select(-c(`Enter the number of miles driven between sites in a typical month.`))



#creating the rankings

SLP_Elm_ranking = transmute(SLP_Elm_data, scale_1_5 = (SLP_Elm_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
SLP_Elm_ranking = mutate(SLP_Elm_ranking, min_sum = rowSums(SLP_Elm_data[ ,c(3:4,9:33)]))

write_xlsx(SLP_Elm_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/SLP_Elm_ranking.xlsx")

#jpeg(file="SLP_Elm_ranking_plot.jpeg")
#plot(SLP_Elm_ranking$scale_1_5,SLP_Elm_ranking$min_sum)
#dev.off()





#Intermediate
SLP_Int_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/SLP.xlsx", sheet = "IntSLP")
colnames(SLP_Int_data) [1] = "grade_span_n"
colnames(SLP_Int_data) [2] = "position_n"

#conversions
SLP_Int_data = mutate(SLP_Int_data, total_class_contacts_c = SLP_Int_data$`Enter the number of students on your caseload with 1-5 accommodations.`+SLP_Int_data$`Enter the number of students on your caseload with 6 or more accommodations.`)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of students on your caseload with 1-5 accommodations.`,`Enter the number of students on your caseload with 6 or more accommodations.`))

SLP_Int_data = mutate(SLP_Int_data, assessment_reports_c = SLP_Int_data$`Enter the number of assessment reports you have completed this year.`*120)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of assessment reports you have completed this year.`))

SLP_Int_data = mutate(SLP_Int_data, meetings_504_c = SLP_Int_data$`Enter the number of 504 meetings you have attended this year.`*60)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of 504 meetings you have attended this year.`))

SLP_Int_data = mutate(SLP_Int_data, inital_CC_c = SLP_Int_data$`Enter the number of initials you held as a case carrier.`*180)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of initials you held as a case carrier.`))

SLP_Int_data = mutate(SLP_Int_data, inital_SP_c = SLP_Int_data$`Enter the number of initials you attended as a service provider.`*60)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of initials you attended as a service provider.`))

SLP_Int_data = mutate(SLP_Int_data, district_IEP_CC_c = SLP_Int_data$`Enter the number of District Staff Involved IEP meetings that you held this school year as a case carrier.`*480)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you held this school year as a case carrier.`))

SLP_Int_data = mutate(SLP_Int_data, district_IEP_SP_c = SLP_Int_data$`Enter the number of District Staff Involved IEP meetings that you attended this school year as a service provider.`*240)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you attended this school year as a service provider.`))

SLP_Int_data = mutate(SLP_Int_data, annual_IEP_CC_c = SLP_Int_data$`Enter the number of annual IEP meetings that you held this school year as a case carrier.`*180)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of annual IEP meetings that you held this school year as a case carrier.`))

SLP_Int_data = mutate(SLP_Int_data, annual_IEP_SP_c = SLP_Int_data$`Enter the number of annual IEP meetings that you attended this school year as a service provider.`*90)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of annual IEP meetings that you attended this school year as a service provider.`))

SLP_Int_data = mutate(SLP_Int_data, triennial_IEP_CC_c = SLP_Int_data$`Enter the number of Triennial IEP meetings that you held this school year as a case carrier.`*360)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of Triennial IEP meetings that you held this school year as a case carrier.`))

SLP_Int_data = mutate(SLP_Int_data, triennial_IEP_SP_c = SLP_Int_data$`Enter the number of Triennial IEP meetings that you attended this school year as a service provider.`*120)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of Triennial IEP meetings that you attended this school year as a service provider.`))

SLP_Int_data = mutate(SLP_Int_data, staff_meeting_c = SLP_Int_data$`Enter the number of staffing meetings that you attended this school year.`*60)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

SLP_Int_data = mutate(SLP_Int_data, amendment_meeting_CC_c = SLP_Int_data$`Enter the number of amendment meetings that you held this school year as a case carrier.`*90)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of amendment meetings that you held this school year as a case carrier.`))

SLP_Int_data = mutate(SLP_Int_data, amendment_meeting_SP_c = SLP_Int_data$`Enter the number of amendment meetings that you attended this school year as a service provider.`*60)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of amendment meetings that you attended this school year as a service provider.`))

SLP_Int_data = mutate(SLP_Int_data, manifestation_meeting_CC_c = SLP_Int_data$`Enter the number of manifestation determination meetings that you held this school year as a case carrier.`*240)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of manifestation determination meetings that you held this school year as a case carrier.`))

SLP_Int_data = mutate(SLP_Int_data, manifestation_meeting_SP_c = SLP_Int_data$`Enter the number of manifestation determination meetings that you attended this school year as a service provider.`*60)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of manifestation determination meetings that you attended this school year as a service provider.`))

SLP_Int_data = mutate(SLP_Int_data, transition_meeting_CC_c = SLP_Int_data$`Enter the number of Transition Meetings that you anticipate holding this school year as a case carrier. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`*120)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate holding this school year as a case carrier. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

SLP_Int_data = mutate(SLP_Int_data, transition_meeting_SP_c = SLP_Int_data$`Enter the number of Transition Meetings that you anticipate attending this school year as a service provider.  (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`*60)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate attending this school year as a service provider.  (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

SLP_Int_data = mutate(SLP_Int_data, interim_IEP_CC_c = SLP_Int_data$`Enter the number of Interim IEPs that you held this school year as a case carrier.`*90)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of Interim IEPs that you held this school year as a case carrier.`))

SLP_Int_data = mutate(SLP_Int_data, interim_IEP_SP_c = SLP_Int_data$`Enter the number of Interim IEPs that you attended this school year as a service provider.`*60)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of Interim IEPs that you attended this school year as a service provider.`))

SLP_Int_data = mutate(SLP_Int_data, BER_reports_c = SLP_Int_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`*15)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

SLP_Int_data = mutate(SLP_Int_data, DRDP_assessment_c = SLP_Int_data$`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`*45)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`))

SLP_Int_data = mutate(SLP_Int_data, days_pulled_c = SLP_Int_data$`Enter the number of days you have been pulled from your duties this year.`*420)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of days you have been pulled from your duties this year.`))

SLP_Int_data = mutate(SLP_Int_data, sites_travel_c = SLP_Int_data$`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`*5400)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`))

SLP_Int_data = mutate(SLP_Int_data, miles_travel_c = (SLP_Int_data$`Enter the number of miles driven between sites in a typical month.`*2)+15)
SLP_Int_data = SLP_Int_data %>% select(-c(`Enter the number of miles driven between sites in a typical month.`))



#creating the rankings

SLP_Int_ranking = transmute(SLP_Int_data, scale_1_5 = (SLP_Int_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
SLP_Int_ranking = mutate(SLP_Int_ranking, min_sum = rowSums(SLP_Int_data[ ,c(3:4,9:33)]))

write_xlsx(SLP_Int_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/SLP_Int_ranking.xlsx")

#jpeg(file="SLP_Int_ranking_plot.jpeg")
#plot(SLP_Int_ranking$scale_1_5,SLP_Int_ranking$min_sum)
#dev.off()





#High School
SLP_HS_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/SLP.xlsx", sheet = "HSSLP")
colnames(SLP_HS_data) [1] = "grade_span_n"
colnames(SLP_HS_data) [2] = "position_n"

#conversions
SLP_HS_data = mutate(SLP_HS_data, total_class_contacts_c = SLP_HS_data$`Enter the number of students on your caseload with 1-5 accommodations.`+SLP_HS_data$`Enter the number of students on your caseload with 6 or more accommodations.`)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of students on your caseload with 1-5 accommodations.`,`Enter the number of students on your caseload with 6 or more accommodations.`))

SLP_HS_data = mutate(SLP_HS_data, assessment_reports_c = SLP_HS_data$`Enter the number of assessment reports you have completed this year.`*120)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of assessment reports you have completed this year.`))

SLP_HS_data = mutate(SLP_HS_data, meetings_504_c = SLP_HS_data$`Enter the number of 504 meetings you have attended this year.`*60)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of 504 meetings you have attended this year.`))

SLP_HS_data = mutate(SLP_HS_data, inital_CC_c = SLP_HS_data$`Enter the number of initials you held as a case carrier.`*180)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of initials you held as a case carrier.`))

SLP_HS_data = mutate(SLP_HS_data, inital_SP_c = SLP_HS_data$`Enter the number of initials you attended as a service provider.`*60)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of initials you attended as a service provider.`))

SLP_HS_data = mutate(SLP_HS_data, district_IEP_CC_c = SLP_HS_data$`Enter the number of District Staff Involved IEP meetings that you held this school year as a case carrier.`*480)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you held this school year as a case carrier.`))

SLP_HS_data = mutate(SLP_HS_data, district_IEP_SP_c = SLP_HS_data$`Enter the number of District Staff Involved IEP meetings that you attended this school year as a service provider.`*240)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you attended this school year as a service provider.`))

SLP_HS_data = mutate(SLP_HS_data, annual_IEP_CC_c = SLP_HS_data$`Enter the number of annual IEP meetings that you held this school year as a case carrier.`*180)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of annual IEP meetings that you held this school year as a case carrier.`))

SLP_HS_data = mutate(SLP_HS_data, annual_IEP_SP_c = SLP_HS_data$`Enter the number of annual IEP meetings that you attended this school year as a service provider.`*90)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of annual IEP meetings that you attended this school year as a service provider.`))

SLP_HS_data = mutate(SLP_HS_data, triennial_IEP_CC_c = SLP_HS_data$`Enter the number of Triennial IEP meetings that you held this school year as a case carrier.`*360)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of Triennial IEP meetings that you held this school year as a case carrier.`))

SLP_HS_data = mutate(SLP_HS_data, triennial_IEP_SP_c = SLP_HS_data$`Enter the number of Triennial IEP meetings that you attended this school year as a service provider.`*120)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of Triennial IEP meetings that you attended this school year as a service provider.`))

SLP_HS_data = mutate(SLP_HS_data, staff_meeting_c = SLP_HS_data$`Enter the number of staffing meetings that you attended this school year.`*60)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

SLP_HS_data = mutate(SLP_HS_data, amendment_meeting_CC_c = SLP_HS_data$`Enter the number of amendment meetings that you held this school year as a case carrier.`*90)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of amendment meetings that you held this school year as a case carrier.`))

SLP_HS_data = mutate(SLP_HS_data, amendment_meeting_SP_c = SLP_HS_data$`Enter the number of amendment meetings that you attended this school year as a service provider.`*60)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of amendment meetings that you attended this school year as a service provider.`))

SLP_HS_data = mutate(SLP_HS_data, manifestation_meeting_CC_c = SLP_HS_data$`Enter the number of manifestation determination meetings that you held this school year as a case carrier.`*240)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of manifestation determination meetings that you held this school year as a case carrier.`))

SLP_HS_data = mutate(SLP_HS_data, manifestation_meeting_SP_c = SLP_HS_data$`Enter the number of manifestation determination meetings that you attended this school year as a service provider.`*60)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of manifestation determination meetings that you attended this school year as a service provider.`))

SLP_HS_data = mutate(SLP_HS_data, transition_meeting_CC_c = SLP_HS_data$`Enter the number of Transition Meetings that you anticipate holding this school year as a case carrier. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`*120)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate holding this school year as a case carrier. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

SLP_HS_data = mutate(SLP_HS_data, transition_meeting_SP_c = SLP_HS_data$`Enter the number of Transition Meetings that you anticipate attending this school year as a service provider.  (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`*60)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate attending this school year as a service provider.  (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

SLP_HS_data = mutate(SLP_HS_data, interim_IEP_CC_c = SLP_HS_data$`Enter the number of Interim IEPs that you held this school year as a case carrier.`*90)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of Interim IEPs that you held this school year as a case carrier.`))

SLP_HS_data = mutate(SLP_HS_data, interim_IEP_SP_c = SLP_HS_data$`Enter the number of Interim IEPs that you attended this school year as a service provider.`*60)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of Interim IEPs that you attended this school year as a service provider.`))

SLP_HS_data = mutate(SLP_HS_data, BER_reports_c = SLP_HS_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`*15)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

SLP_HS_data = mutate(SLP_HS_data, DRDP_assessment_c = SLP_HS_data$`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`*45)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`))

SLP_HS_data = mutate(SLP_HS_data, days_pulled_c = SLP_HS_data$`Enter the number of days you have been pulled from your duties this year.`*420)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of days you have been pulled from your duties this year.`))

SLP_HS_data = mutate(SLP_HS_data, sites_travel_c = SLP_HS_data$`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`*5400)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`))

SLP_HS_data = mutate(SLP_HS_data, miles_travel_c = (SLP_HS_data$`Enter the number of miles driven between sites in a typical month.`*2)+15)
SLP_HS_data = SLP_HS_data %>% select(-c(`Enter the number of miles driven between sites in a typical month.`))



#creating the rankings

SLP_HS_ranking = transmute(SLP_HS_data, scale_1_5 = (SLP_HS_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
SLP_HS_ranking = mutate(SLP_HS_ranking, min_sum = rowSums(SLP_HS_data[ ,c(3:4,9:33)]))

write_xlsx(SLP_HS_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/SLP_HS_ranking.xlsx")

jpeg(file="SLP_HS_ranking_plot.jpeg")
plot(SLP_HS_ranking$scale_1_5,SLP_HS_ranking$min_sum)
dev.off()





#Mixed
SLP_M_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/SLP.xlsx", sheet = "MSLP")
colnames(SLP_M_data) [1] = "grade_span_n"
colnames(SLP_M_data) [2] = "position_n"

#conversions
SLP_M_data = mutate(SLP_M_data, total_class_contacts_c = SLP_M_data$`Enter the number of students on your caseload with 1-5 accommodations.`+SLP_M_data$`Enter the number of students on your caseload with 6 or more accommodations.`)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of students on your caseload with 1-5 accommodations.`,`Enter the number of students on your caseload with 6 or more accommodations.`))

SLP_M_data = mutate(SLP_M_data, assessment_reports_c = SLP_M_data$`Enter the number of assessment reports you have completed this year.`*120)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of assessment reports you have completed this year.`))

SLP_M_data = mutate(SLP_M_data, meetings_504_c = SLP_M_data$`Enter the number of 504 meetings you have attended this year.`*60)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of 504 meetings you have attended this year.`))

SLP_M_data = mutate(SLP_M_data, inital_CC_c = SLP_M_data$`Enter the number of initials you held as a case carrier.`*180)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of initials you held as a case carrier.`))

SLP_M_data = mutate(SLP_M_data, inital_SP_c = SLP_M_data$`Enter the number of initials you attended as a service provider.`*60)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of initials you attended as a service provider.`))

SLP_M_data = mutate(SLP_M_data, district_IEP_CC_c = SLP_M_data$`Enter the number of District Staff Involved IEP meetings that you held this school year as a case carrier.`*480)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you held this school year as a case carrier.`))

SLP_M_data = mutate(SLP_M_data, district_IEP_SP_c = SLP_M_data$`Enter the number of District Staff Involved IEP meetings that you attended this school year as a service provider.`*240)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you attended this school year as a service provider.`))

SLP_M_data = mutate(SLP_M_data, annual_IEP_CC_c = SLP_M_data$`Enter the number of annual IEP meetings that you held this school year as a case carrier.`*180)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of annual IEP meetings that you held this school year as a case carrier.`))

SLP_M_data = mutate(SLP_M_data, annual_IEP_SP_c = SLP_M_data$`Enter the number of annual IEP meetings that you attended this school year as a service provider.`*90)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of annual IEP meetings that you attended this school year as a service provider.`))

SLP_M_data = mutate(SLP_M_data, triennial_IEP_CC_c = SLP_M_data$`Enter the number of Triennial IEP meetings that you held this school year as a case carrier.`*360)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of Triennial IEP meetings that you held this school year as a case carrier.`))

SLP_M_data = mutate(SLP_M_data, triennial_IEP_SP_c = SLP_M_data$`Enter the number of Triennial IEP meetings that you attended this school year as a service provider.`*120)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of Triennial IEP meetings that you attended this school year as a service provider.`))

SLP_M_data = mutate(SLP_M_data, staff_meeting_c = SLP_M_data$`Enter the number of staffing meetings that you attended this school year.`*60)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

SLP_M_data = mutate(SLP_M_data, amendment_meeting_CC_c = SLP_M_data$`Enter the number of amendment meetings that you held this school year as a case carrier.`*90)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of amendment meetings that you held this school year as a case carrier.`))

SLP_M_data = mutate(SLP_M_data, amendment_meeting_SP_c = SLP_M_data$`Enter the number of amendment meetings that you attended this school year as a service provider.`*60)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of amendment meetings that you attended this school year as a service provider.`))

SLP_M_data = mutate(SLP_M_data, manifestation_meeting_CC_c = SLP_M_data$`Enter the number of manifestation determination meetings that you held this school year as a case carrier.`*240)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of manifestation determination meetings that you held this school year as a case carrier.`))

SLP_M_data = mutate(SLP_M_data, manifestation_meeting_SP_c = SLP_M_data$`Enter the number of manifestation determination meetings that you attended this school year as a service provider.`*60)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of manifestation determination meetings that you attended this school year as a service provider.`))

SLP_M_data = mutate(SLP_M_data, transition_meeting_CC_c = SLP_M_data$`Enter the number of Transition Meetings that you anticipate holding this school year as a case carrier. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`*120)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate holding this school year as a case carrier. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

SLP_M_data = mutate(SLP_M_data, transition_meeting_SP_c = SLP_M_data$`Enter the number of Transition Meetings that you anticipate attending this school year as a service provider.  (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`*60)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate attending this school year as a service provider.  (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

SLP_M_data = mutate(SLP_M_data, interim_IEP_CC_c = SLP_M_data$`Enter the number of Interim IEPs that you held this school year as a case carrier.`*90)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of Interim IEPs that you held this school year as a case carrier.`))

SLP_M_data = mutate(SLP_M_data, interim_IEP_SP_c = SLP_M_data$`Enter the number of Interim IEPs that you attended this school year as a service provider.`*60)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of Interim IEPs that you attended this school year as a service provider.`))

SLP_M_data = mutate(SLP_M_data, BER_reports_c = SLP_M_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`*15)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

SLP_M_data = mutate(SLP_M_data, DRDP_assessment_c = SLP_M_data$`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`*45)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`))

SLP_M_data = mutate(SLP_M_data, days_pulled_c = SLP_M_data$`Enter the number of days you have been pulled from your duties this year.`*420)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of days you have been pulled from your duties this year.`))

SLP_M_data = mutate(SLP_M_data, sites_travel_c = SLP_M_data$`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`*5400)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`))

SLP_M_data = mutate(SLP_M_data, miles_travel_c = (SLP_M_data$`Enter the number of miles driven between sites in a typical month.`*2)+15)
SLP_M_data = SLP_M_data %>% select(-c(`Enter the number of miles driven between sites in a typical month.`))



#creating the rankings

SLP_M_ranking = transmute(SLP_M_data, scale_1_5 = (SLP_M_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
SLP_M_ranking = mutate(SLP_M_ranking, min_sum = rowSums(SLP_M_data[ ,c(3:4,9:33)]))

write_xlsx(SLP_M_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/SLP_M_ranking.xlsx")

jpeg(file="SLP_M_ranking_plot.jpeg")
plot(SLP_M_ranking$scale_1_5,SLP_M_ranking$min_sum)
dev.off()





#Pre-School
SLP_PS_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/SLP.xlsx", sheet = "PSSLP")
colnames(SLP_PS_data) [1] = "grade_span_n"
colnames(SLP_PS_data) [2] = "position_n"

#conversions
SLP_PS_data = mutate(SLP_PS_data, total_class_contacts_c = SLP_PS_data$`Enter the number of students on your caseload with 1-5 accommodations.`+SLP_PS_data$`Enter the number of students on your caseload with 6 or more accommodations.`)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of students on your caseload with 1-5 accommodations.`,`Enter the number of students on your caseload with 6 or more accommodations.`))

SLP_PS_data = mutate(SLP_PS_data, assessment_reports_c = SLP_PS_data$`Enter the number of assessment reports you have completed this year.`*120)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of assessment reports you have completed this year.`))

SLP_PS_data = mutate(SLP_PS_data, meetings_504_c = SLP_PS_data$`Enter the number of 504 meetings you have attended this year.`*60)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of 504 meetings you have attended this year.`))

SLP_PS_data = mutate(SLP_PS_data, inital_CC_c = SLP_PS_data$`Enter the number of initials you held as a case carrier.`*180)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of initials you held as a case carrier.`))

SLP_PS_data = mutate(SLP_PS_data, inital_SP_c = SLP_PS_data$`Enter the number of initials you attended as a service provider.`*60)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of initials you attended as a service provider.`))

SLP_PS_data = mutate(SLP_PS_data, district_IEP_CC_c = SLP_PS_data$`Enter the number of District Staff Involved IEP meetings that you held this school year as a case carrier.`*480)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you held this school year as a case carrier.`))

SLP_PS_data = mutate(SLP_PS_data, district_IEP_SP_c = SLP_PS_data$`Enter the number of District Staff Involved IEP meetings that you attended this school year as a service provider.`*240)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you attended this school year as a service provider.`))

SLP_PS_data = mutate(SLP_PS_data, annual_IEP_CC_c = SLP_PS_data$`Enter the number of annual IEP meetings that you held this school year as a case carrier.`*180)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of annual IEP meetings that you held this school year as a case carrier.`))

SLP_PS_data = mutate(SLP_PS_data, annual_IEP_SP_c = SLP_PS_data$`Enter the number of annual IEP meetings that you attended this school year as a service provider.`*90)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of annual IEP meetings that you attended this school year as a service provider.`))

SLP_PS_data = mutate(SLP_PS_data, triennial_IEP_CC_c = SLP_PS_data$`Enter the number of Triennial IEP meetings that you held this school year as a case carrier.`*360)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of Triennial IEP meetings that you held this school year as a case carrier.`))

SLP_PS_data = mutate(SLP_PS_data, triennial_IEP_SP_c = SLP_PS_data$`Enter the number of Triennial IEP meetings that you attended this school year as a service provider.`*120)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of Triennial IEP meetings that you attended this school year as a service provider.`))

SLP_PS_data = mutate(SLP_PS_data, staff_meeting_c = SLP_PS_data$`Enter the number of staffing meetings that you attended this school year.`*60)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

SLP_PS_data = mutate(SLP_PS_data, amendment_meeting_CC_c = SLP_PS_data$`Enter the number of amendment meetings that you held this school year as a case carrier.`*90)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of amendment meetings that you held this school year as a case carrier.`))

SLP_PS_data = mutate(SLP_PS_data, amendment_meeting_SP_c = SLP_PS_data$`Enter the number of amendment meetings that you attended this school year as a service provider.`*60)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of amendment meetings that you attended this school year as a service provider.`))

SLP_PS_data = mutate(SLP_PS_data, manifestation_meeting_CC_c = SLP_PS_data$`Enter the number of manifestation determination meetings that you held this school year as a case carrier.`*240)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of manifestation determination meetings that you held this school year as a case carrier.`))

SLP_PS_data = mutate(SLP_PS_data, manifestation_meeting_SP_c = SLP_PS_data$`Enter the number of manifestation determination meetings that you attended this school year as a service provider.`*60)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of manifestation determination meetings that you attended this school year as a service provider.`))

SLP_PS_data = mutate(SLP_PS_data, transition_meeting_CC_c = SLP_PS_data$`Enter the number of Transition Meetings that you anticipate holding this school year as a case carrier. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`*120)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate holding this school year as a case carrier. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

SLP_PS_data = mutate(SLP_PS_data, transition_meeting_SP_c = SLP_PS_data$`Enter the number of Transition Meetings that you anticipate attending this school year as a service provider.  (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`*60)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate attending this school year as a service provider.  (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

SLP_PS_data = mutate(SLP_PS_data, interim_IEP_CC_c = SLP_PS_data$`Enter the number of Interim IEPs that you held this school year as a case carrier.`*90)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of Interim IEPs that you held this school year as a case carrier.`))

SLP_PS_data = mutate(SLP_PS_data, interim_IEP_SP_c = SLP_PS_data$`Enter the number of Interim IEPs that you attended this school year as a service provider.`*60)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of Interim IEPs that you attended this school year as a service provider.`))

SLP_PS_data = mutate(SLP_PS_data, BER_reports_c = SLP_PS_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`*15)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

SLP_PS_data = mutate(SLP_PS_data, DRDP_assessment_c = SLP_PS_data$`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`*45)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`))

SLP_PS_data = mutate(SLP_PS_data, days_pulled_c = SLP_PS_data$`Enter the number of days you have been pulled from your duties this year.`*420)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of days you have been pulled from your duties this year.`))

SLP_PS_data = mutate(SLP_PS_data, sites_travel_c = SLP_PS_data$`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`*5400)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`))

SLP_PS_data = mutate(SLP_PS_data, miles_travel_c = (SLP_PS_data$`Enter the number of miles driven between sites in a typical month.`*2)+15)
SLP_PS_data = SLP_PS_data %>% select(-c(`Enter the number of miles driven between sites in a typical month.`))



#creating the rankings

SLP_PS_ranking = transmute(SLP_PS_data, scale_1_5 = (SLP_PS_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
SLP_PS_ranking = mutate(SLP_PS_ranking, min_sum = rowSums(SLP_PS_data[ ,c(3:4,9:33)]))

write_xlsx(SLP_PS_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/SLP_PS_ranking.xlsx")

jpeg(file="SLP_PS_ranking_plot.jpeg")
plot(SLP_PS_ranking$scale_1_5,SLP_PS_ranking$min_sum)
dev.off()


#SLP Group Ranking#

SLP_ranking = bind_rows(SLP_Elm_ranking, SLP_Int_ranking, SLP_HS_ranking, SLP_M_ranking, SLP_PS_ranking)

write_xlsx(SLP_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/SLP_ranking.xlsx")

jpeg(file="SLP_ranking_plot.jpeg")
plot(SLP_ranking$scale_1_5,SLP_ranking$min_sum)
dev.off()