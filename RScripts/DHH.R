library(readxl)
library(tidyverse)

DHH_M_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/DHH.xlsx", sheet = "MDHH")
colnames(DHH_M_data) [1] = "grade_span_n"
colnames(DHH_M_data) [2] = "position_n"


#conversions
DHH_M_data = mutate(DHH_M_data, total_class_contacts_c = DHH_M_data$`Enter the number of students on your caseload with 1-5 accommodations.`+DHH_M_data$`Enter the number of students on your caseload with 6 or more accommodations.`)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of students on your caseload with 1-5 accommodations.`,`Enter the number of students on your caseload with 6 or more accommodations.`))

DHH_M_data = mutate(DHH_M_data, assessment_reports_c = DHH_M_data$`Enter the number of assessment reports you have completed this year.`*120)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of assessment reports you have completed this year.`))


DHH_M_data = mutate(DHH_M_data, DRDP_assessment_c = DHH_M_data$`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`*45)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of Desired Results Developmental Profile (DRDP) Assessments given this year.`))

DHH_M_data = mutate(DHH_M_data, present_levels_mins_c = DHH_M_data$`Enter the number of hours this year you have spent writing present levels this school year.`*60)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of hours this year you have spent writing present levels this school year.`))

DHH_M_data = mutate(DHH_M_data, meetings_504_c = DHH_M_data$`Enter the number of 504 meetings you have attended this year.`*60)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of 504 meetings you have attended this year.`))

DHH_M_data = mutate(DHH_M_data, inital_CC_c = DHH_M_data$`Enter the number of initials you held as a case carrier.`*180)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of initials you held as a case carrier.`))

DHH_M_data = mutate(DHH_M_data, inital_SP_c = DHH_M_data$`Enter the number of initials you attended as a service provider.`*60)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of initials you attended as a service provider.`))

DHH_M_data = mutate(DHH_M_data, district_IEP_CC_c = DHH_M_data$`Enter the number of District Staff Involved IEP meetings that you held this school year as a case carrier.`*480)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you held this school year as a case carrier.`))

DHH_M_data = mutate(DHH_M_data, district_IEP_SP_c = DHH_M_data$`Enter the number of District Staff Involved IEP meetings that you attended this school year as a service provider.`*240)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you attended this school year as a service provider.`))

DHH_M_data = mutate(DHH_M_data, annual_IEP_CC_c = DHH_M_data$`Enter the number of annual IEP meetings that you held this school year as a case carrier.`*180)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of annual IEP meetings that you held this school year as a case carrier.`))

DHH_M_data = mutate(DHH_M_data, annual_IEP_SP_c = DHH_M_data$`Enter the number of annual IEP meetings that you attended this school year as a service provider.`*90)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of annual IEP meetings that you attended this school year as a service provider.`))

DHH_M_data = mutate(DHH_M_data, triennial_IEP_CC_c = DHH_M_data$`Enter the number of Triennial IEP meetings that you held this school year as a case carrier.`*360)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of Triennial IEP meetings that you held this school year as a case carrier.`))

DHH_M_data = mutate(DHH_M_data, triennial_IEP_SP_c = DHH_M_data$`Enter the number of Triennial IEP meetings that you attended this school year as a service provider.`*120)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of Triennial IEP meetings that you attended this school year as a service provider.`))

DHH_M_data = mutate(DHH_M_data, staff_meeting_c = DHH_M_data$`Enter the number of staffing meetings that you attended this school year.`*60)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

DHH_M_data = mutate(DHH_M_data, amendment_meeting_CC_c = DHH_M_data$`Enter the number of amendment meetings that you held this school year as a case carrier.`*90)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of amendment meetings that you held this school year as a case carrier.`))

DHH_M_data = mutate(DHH_M_data, amendment_meeting_SP_c = DHH_M_data$`Enter the number of amendment meetings that you attended this school year as a service provider.`*60)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of amendment meetings that you attended this school year as a service provider.`))

DHH_M_data = mutate(DHH_M_data, manifestation_meeting_CC_c = DHH_M_data$`Enter the number of manifestation determination meetings that you held this school year as a case carrier.`*240)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of manifestation determination meetings that you held this school year as a case carrier.`))

DHH_M_data = mutate(DHH_M_data, manifestation_meeting_SP_c = DHH_M_data$`Enter the number of manifestation determination meetings that you attended this school year as a service provider`*60)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of manifestation determination meetings that you attended this school year as a service provider`))

DHH_M_data = mutate(DHH_M_data, transition_meeting_CC_c = DHH_M_data$`Enter the number of Transition Meetings that you anticipate holding this school year as a case carrier.  (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`*120)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate holding this school year as a case carrier.  (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

DHH_M_data = mutate(DHH_M_data, transition_meeting_SP_c = DHH_M_data$`Enter the number of Transition Meetings that you anticipate attending this school year as a service provider. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`*60)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate attending this school year as a service provider. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

DHH_M_data = mutate(DHH_M_data, interim_IEP_CC_c = DHH_M_data$`Enter the number of Interim IEPs that you held this school year as a case carrier.`*90)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of Interim IEPs that you held this school year as a case carrier.`))

DHH_M_data = mutate(DHH_M_data, interim_IEP_SP_c = DHH_M_data$`Enter the number of Interim IEPs that you attended this school year as a service provider`*60)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of Interim IEPs that you attended this school year as a service provider`))

DHH_M_data = mutate(DHH_M_data, BER_reports_c = DHH_M_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`*15)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

DHH_M_data = mutate(DHH_M_data, days_pulled_c = DHH_M_data$`Enter the number of days you have been pulled from your duties this year.`*420)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of days you have been pulled from your duties this year.`))

DHH_M_data = mutate(DHH_M_data, sites_travel_c = DHH_M_data$`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`*5400)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`))

DHH_M_data = mutate(DHH_M_data, miles_travel_c = (DHH_M_data$`Enter the number of miles driven between sites in a typical month.`*2)+15)
DHH_M_data = DHH_M_data %>% select(-c(`Enter the number of miles driven between sites in a typical month.`))

#creating the rankings

DHH_M_ranking = transmute(DHH_M_data, scale_1_5 = (DHH_M_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
DHH_M_ranking = mutate(DHH_M_ranking, min_sum = rowSums(DHH_M_data[ ,c(6:8,10:35)]))

write_xlsx(DHH_M_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/DHH_ranking.xlsx")

#jpeg(file="DHH_M_ranking_plot.jpeg")
#plot(DHH_M_ranking$scale_1_5,DHH_M_ranking$min_sum)
#dev.off()


