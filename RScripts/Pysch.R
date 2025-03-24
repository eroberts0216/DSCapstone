library(readxl)
library(tidyverse)

#Elementary#
Pysch_Elm_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/Psych.xlsx", sheet = "ElmPsych")
colnames(Pysch_Elm_data) [1] = "grade_span_n"
colnames(Pysch_Elm_data) [2] = "position_n"

#conversions
Pysch_Elm_data = mutate(Pysch_Elm_data, psychoeducational_assessment_c = Pysch_Elm_data$`Enter the number of psychoeducational assessments you have written this year.`* 840)
Pysch_Elm_data = Pysch_Elm_data %>% select(-c(`Enter the number of psychoeducational assessments you have written this year.`))

Pysch_Elm_data = mutate(Pysch_Elm_data, IIS_assessment_c = Pysch_Elm_data$`Enter the number of Intensive Individual Service (IIS) assessments you have written this year.`* 360)
Pysch_Elm_data = Pysch_Elm_data %>% select(-c(`Enter the number of Intensive Individual Service (IIS) assessments you have written this year.`))

Pysch_Elm_data = mutate(Pysch_Elm_data, BIP_assessment_c = Pysch_Elm_data$`Enter the number of Behavior Intervention Plan (BIP) assessments you have written this year.  Please include any you have revised as well.`* 120)
Pysch_Elm_data = Pysch_Elm_data %>% select(-c(`Enter the number of Behavior Intervention Plan (BIP) assessments you have written this year.  Please include any you have revised as well.`))

Pysch_Elm_data = mutate(Pysch_Elm_data, FBA_assessment_c = Pysch_Elm_data$`Enter the number of Functional Behavior Assessments (FBA) you have written this year.`* 480)
Pysch_Elm_data = Pysch_Elm_data %>% select(-c(`Enter the number of Functional Behavior Assessments (FBA) you have written this year.`))

Pysch_Elm_data = mutate(Pysch_Elm_data, MTSS_meeting_c = Pysch_Elm_data$`Enter the number of MTSS T2/T3 meetings you have attended this year.`* 90)
Pysch_Elm_data = Pysch_Elm_data %>% select(-c(`Enter the number of MTSS T2/T3 meetings you have attended this year.`))

Pysch_Elm_data = mutate(Pysch_Elm_data, district_IEP_c = Pysch_Elm_data$`Enter the number of District Staff Involved IEP meetings that you attended this school year.`* 480)
Pysch_Elm_data = Pysch_Elm_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you attended this school year.`))

Pysch_Elm_data = mutate(Pysch_Elm_data, annual_IEP_c = Pysch_Elm_data$`Enter the number of annual IEP meetings that you attended this school year.`* 180)
Pysch_Elm_data = Pysch_Elm_data %>% select(-c(`Enter the number of annual IEP meetings that you attended this school year.`))

Pysch_Elm_data = mutate(Pysch_Elm_data, triennial_IEP_c = Pysch_Elm_data$`Enter the number of Triennial IEP meetings that you attended this school year.`* 360)
Pysch_Elm_data = Pysch_Elm_data %>% select(-c(`Enter the number of Triennial IEP meetings that you attended this school year.`))

Pysch_Elm_data = mutate(Pysch_Elm_data, staff_meeting_c = Pysch_Elm_data$`Enter the number of staffing meetings that you attended this school year.`* 60)
Pysch_Elm_data = Pysch_Elm_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

Pysch_Elm_data = mutate(Pysch_Elm_data, amendment_meeting_c = Pysch_Elm_data$`Enter the number of amendment meetings that you attended this school year.`* 90)
Pysch_Elm_data = Pysch_Elm_data %>% select(-c(`Enter the number of amendment meetings that you attended this school year.`))

Pysch_Elm_data = mutate(Pysch_Elm_data, manifestation_meeting_c = Pysch_Elm_data$`Enter the number of manifestation determination meetings that you attended this school year.`* 240)
Pysch_Elm_data = Pysch_Elm_data %>% select(-c(`Enter the number of manifestation determination meetings that you attended this school year.`))

Pysch_Elm_data = mutate(Pysch_Elm_data, transition_meeting_c = Pysch_Elm_data$`Enter the number of Transition Meetings that you anticipate attending this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`* 120)
Pysch_Elm_data = Pysch_Elm_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate attending this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

Pysch_Elm_data = mutate(Pysch_Elm_data, interim_IEP_c = Pysch_Elm_data$`Enter the number of Interim IEPs that you attended this school year.`* 90)
Pysch_Elm_data = Pysch_Elm_data %>% select(-c(`Enter the number of Interim IEPs that you attended this school year.`))

Pysch_Elm_data = mutate(Pysch_Elm_data, BER_report_c = Pysch_Elm_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`* 15)
Pysch_Elm_data = Pysch_Elm_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

Pysch_Elm_data = mutate(Pysch_Elm_data, pulled_days_c = Pysch_Elm_data$`Enter the number of days you have been pulled from your duties this year.`* 420)
Pysch_Elm_data = Pysch_Elm_data %>% select(-c(`Enter the number of days you have been pulled from your duties this year.`))

Pysch_Elm_data = mutate(Pysch_Elm_data, sites_travel_c = Pysch_Elm_data$`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`* 5400)
Pysch_Elm_data = Pysch_Elm_data %>% select(-c(`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`))

Pysch_Elm_data = mutate(Pysch_Elm_data, miles_travel_c = ((Pysch_Elm_data$`Enter the number of miles driven between sites in a typical month.`*2)+15))
Pysch_Elm_data = Pysch_Elm_data %>% select(-c(`Enter the number of miles driven between sites in a typical month.`))


#creating the rankings

Pysch_Elm_ranking = transmute(Pysch_Elm_data, scale_1_5 = (Pysch_Elm_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
Pysch_Elm_ranking = mutate(Pysch_Elm_ranking, min_sum = rowSums(Pysch_Elm_data[ ,c(3,5:21)]))

write_xlsx(Pysch_Elm_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/Psych_Elm_ranking.xlsx")

#jpeg(file="Pysch_Elm_ranking_plot.jpeg")
#plot(Pysch_Elm_ranking$scale_1_5,Pysch_Elm_ranking$min_sum)
#dev.off()



#Intermediate#
Pysch_Int_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/Psych.xlsx", sheet = "IntPsych")
colnames(Pysch_Int_data) [1] = "grade_span_n"
colnames(Pysch_Int_data) [2] = "position_n"

#conversions
Pysch_Int_data = mutate(Pysch_Int_data, psychoeducational_assessment_c = Pysch_Int_data$`Enter the number of psychoeducational assessments you have written this year.`* 840)
Pysch_Int_data = Pysch_Int_data %>% select(-c(`Enter the number of psychoeducational assessments you have written this year.`))

Pysch_Int_data = mutate(Pysch_Int_data, IIS_assessment_c = Pysch_Int_data$`Enter the number of Intensive Individual Service (IIS) assessments you have written this year.`* 360)
Pysch_Int_data = Pysch_Int_data %>% select(-c(`Enter the number of Intensive Individual Service (IIS) assessments you have written this year.`))

Pysch_Int_data = mutate(Pysch_Int_data, BIP_assessment_c = Pysch_Int_data$`Enter the number of Behavior Intervention Plan (BIP) assessments you have written this year.  Please include any you have revised as well.`* 120)
Pysch_Int_data = Pysch_Int_data %>% select(-c(`Enter the number of Behavior Intervention Plan (BIP) assessments you have written this year.  Please include any you have revised as well.`))

Pysch_Int_data = mutate(Pysch_Int_data, FBA_assessment_c = Pysch_Int_data$`Enter the number of Functional Behavior Assessments (FBA) you have written this year.`* 480)
Pysch_Int_data = Pysch_Int_data %>% select(-c(`Enter the number of Functional Behavior Assessments (FBA) you have written this year.`))

Pysch_Int_data = mutate(Pysch_Int_data, MTSS_meeting_c = Pysch_Int_data$`Enter the number of MTSS T2/T3 meetings you have attended this year.`* 90)
Pysch_Int_data = Pysch_Int_data %>% select(-c(`Enter the number of MTSS T2/T3 meetings you have attended this year.`))

Pysch_Int_data = mutate(Pysch_Int_data, district_IEP_c = Pysch_Int_data$`Enter the number of District Staff Involved IEP meetings that you attended this school year.`* 480)
Pysch_Int_data = Pysch_Int_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you attended this school year.`))

Pysch_Int_data = mutate(Pysch_Int_data, annual_IEP_c = Pysch_Int_data$`Enter the number of annual IEP meetings that you attended this school year.`* 180)
Pysch_Int_data = Pysch_Int_data %>% select(-c(`Enter the number of annual IEP meetings that you attended this school year.`))

Pysch_Int_data = mutate(Pysch_Int_data, triennial_IEP_c = Pysch_Int_data$`Enter the number of Triennial IEP meetings that you attended this school year.`* 360)
Pysch_Int_data = Pysch_Int_data %>% select(-c(`Enter the number of Triennial IEP meetings that you attended this school year.`))

Pysch_Int_data = mutate(Pysch_Int_data, staff_meeting_c = Pysch_Int_data$`Enter the number of staffing meetings that you attended this school year.`* 60)
Pysch_Int_data = Pysch_Int_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

Pysch_Int_data = mutate(Pysch_Int_data, amendment_meeting_c = Pysch_Int_data$`Enter the number of amendment meetings that you attended this school year.`* 90)
Pysch_Int_data = Pysch_Int_data %>% select(-c(`Enter the number of amendment meetings that you attended this school year.`))

Pysch_Int_data = mutate(Pysch_Int_data, manifestation_meeting_c = Pysch_Int_data$`Enter the number of manifestation determination meetings that you attended this school year.`* 240)
Pysch_Int_data = Pysch_Int_data %>% select(-c(`Enter the number of manifestation determination meetings that you attended this school year.`))

Pysch_Int_data = mutate(Pysch_Int_data, transition_meeting_c = Pysch_Int_data$`Enter the number of Transition Meetings that you anticipate attending this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`* 120)
Pysch_Int_data = Pysch_Int_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate attending this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

Pysch_Int_data = mutate(Pysch_Int_data, interim_IEP_c = Pysch_Int_data$`Enter the number of Interim IEPs that you attended this school year.`* 90)
Pysch_Int_data = Pysch_Int_data %>% select(-c(`Enter the number of Interim IEPs that you attended this school year.`))

Pysch_Int_data = mutate(Pysch_Int_data, BER_report_c = Pysch_Int_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`* 15)
Pysch_Int_data = Pysch_Int_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

Pysch_Int_data = mutate(Pysch_Int_data, pulled_days_c = Pysch_Int_data$`Enter the number of days you have been pulled from your duties this year.`* 420)
Pysch_Int_data = Pysch_Int_data %>% select(-c(`Enter the number of days you have been pulled from your duties this year.`))

Pysch_Int_data = mutate(Pysch_Int_data, sites_travel_c = Pysch_Int_data$`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`* 5400)
Pysch_Int_data = Pysch_Int_data %>% select(-c(`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`))

Pysch_Int_data = mutate(Pysch_Int_data, miles_travel_c = ((Pysch_Int_data$`Enter the number of miles driven between sites in a typical month.`*2)+15))
Pysch_Int_data = Pysch_Int_data %>% select(-c(`Enter the number of miles driven between sites in a typical month.`))


#creating the rankings

Pysch_Int_ranking = transmute(Pysch_Int_data, scale_1_5 = (Pysch_Int_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
Pysch_Int_ranking = mutate(Pysch_Int_ranking, min_sum = rowSums(Pysch_Int_data[ ,c(3,5:21)]))

write_xlsx(Pysch_Int_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/Psych_Int_ranking.xlsx")

#jpeg(file="Pysch_Int_ranking_plot.jpeg")
#plot(Pysch_Int_ranking$scale_1_5,Pysch_Int_ranking$min_sum)
#dev.off()



#High School#
Pysch_HS_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/Psych.xlsx", sheet = "HSPsych")
colnames(Pysch_HS_data) [1] = "grade_span_n"
colnames(Pysch_HS_data) [2] = "position_n"

#conversions
Pysch_HS_data = mutate(Pysch_HS_data, psychoeducational_assessment_c = Pysch_HS_data$`Enter the number of psychoeducational assessments you have written this year.`* 840)
Pysch_HS_data = Pysch_HS_data %>% select(-c(`Enter the number of psychoeducational assessments you have written this year.`))

Pysch_HS_data = mutate(Pysch_HS_data, IIS_assessment_c = Pysch_HS_data$`Enter the number of Intensive Individual Service (IIS) assessments you have written this year.`* 360)
Pysch_HS_data = Pysch_HS_data %>% select(-c(`Enter the number of Intensive Individual Service (IIS) assessments you have written this year.`))

Pysch_HS_data = mutate(Pysch_HS_data, BIP_assessment_c = Pysch_HS_data$`Enter the number of Behavior Intervention Plan (BIP) assessments you have written this year.  Please include any you have revised as well.`* 120)
Pysch_HS_data = Pysch_HS_data %>% select(-c(`Enter the number of Behavior Intervention Plan (BIP) assessments you have written this year.  Please include any you have revised as well.`))

Pysch_HS_data = mutate(Pysch_HS_data, FBA_assessment_c = Pysch_HS_data$`Enter the number of Functional Behavior Assessments (FBA) you have written this year.`* 480)
Pysch_HS_data = Pysch_HS_data %>% select(-c(`Enter the number of Functional Behavior Assessments (FBA) you have written this year.`))

Pysch_HS_data = mutate(Pysch_HS_data, MTSS_meeting_c = Pysch_HS_data$`Enter the number of MTSS T2/T3 meetings you have attended this year.`* 90)
Pysch_HS_data = Pysch_HS_data %>% select(-c(`Enter the number of MTSS T2/T3 meetings you have attended this year.`))

Pysch_HS_data = mutate(Pysch_HS_data, district_IEP_c = Pysch_HS_data$`Enter the number of District Staff Involved IEP meetings that you attended this school year.`* 480)
Pysch_HS_data = Pysch_HS_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you attended this school year.`))

Pysch_HS_data = mutate(Pysch_HS_data, annual_IEP_c = Pysch_HS_data$`Enter the number of annual IEP meetings that you attended this school year.`* 180)
Pysch_HS_data = Pysch_HS_data %>% select(-c(`Enter the number of annual IEP meetings that you attended this school year.`))

Pysch_HS_data = mutate(Pysch_HS_data, triennial_IEP_c = Pysch_HS_data$`Enter the number of Triennial IEP meetings that you attended this school year.`* 360)
Pysch_HS_data = Pysch_HS_data %>% select(-c(`Enter the number of Triennial IEP meetings that you attended this school year.`))

Pysch_HS_data = mutate(Pysch_HS_data, staff_meeting_c = Pysch_HS_data$`Enter the number of staffing meetings that you attended this school year.`* 60)
Pysch_HS_data = Pysch_HS_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

Pysch_HS_data = mutate(Pysch_HS_data, amendment_meeting_c = Pysch_HS_data$`Enter the number of amendment meetings that you attended this school year.`* 90)
Pysch_HS_data = Pysch_HS_data %>% select(-c(`Enter the number of amendment meetings that you attended this school year.`))

Pysch_HS_data = mutate(Pysch_HS_data, manifestation_meeting_c = Pysch_HS_data$`Enter the number of manifestation determination meetings that you attended this school year.`* 240)
Pysch_HS_data = Pysch_HS_data %>% select(-c(`Enter the number of manifestation determination meetings that you attended this school year.`))

Pysch_HS_data = mutate(Pysch_HS_data, transition_meeting_c = Pysch_HS_data$`Enter the number of Transition Meetings that you anticipate attending this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`* 120)
Pysch_HS_data = Pysch_HS_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate attending this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

Pysch_HS_data = mutate(Pysch_HS_data, interim_IEP_c = Pysch_HS_data$`Enter the number of Interim IEPs that you attended this school year.`* 90)
Pysch_HS_data = Pysch_HS_data %>% select(-c(`Enter the number of Interim IEPs that you attended this school year.`))

Pysch_HS_data = mutate(Pysch_HS_data, BER_report_c = Pysch_HS_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`* 15)
Pysch_HS_data = Pysch_HS_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

Pysch_HS_data = mutate(Pysch_HS_data, pulled_days_c = Pysch_HS_data$`Enter the number of days you have been pulled from your duties this year.`* 420)
Pysch_HS_data = Pysch_HS_data %>% select(-c(`Enter the number of days you have been pulled from your duties this year.`))

Pysch_HS_data = mutate(Pysch_HS_data, sites_travel_c = Pysch_HS_data$`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`* 5400)
Pysch_HS_data = Pysch_HS_data %>% select(-c(`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`))

Pysch_HS_data = mutate(Pysch_HS_data, miles_travel_c = ((Pysch_HS_data$`Enter the number of miles driven between sites in a typical month.`*2)+15))
Pysch_HS_data = Pysch_HS_data %>% select(-c(`Enter the number of miles driven between sites in a typical month.`))


#creating the rankings

Pysch_HS_ranking = transmute(Pysch_HS_data, scale_1_5 = (Pysch_HS_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
Pysch_HS_ranking = mutate(Pysch_HS_ranking, min_sum = rowSums(Pysch_HS_data[ ,c(3,5:21)]))

write_xlsx(Pysch_HS_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/Psych_HS_ranking.xlsx")

#jpeg(file="Pysch_HS_ranking_plot.jpeg")
#plot(Pysch_HS_ranking$scale_1_5,Pysch_HS_ranking$min_sum)
#dev.off()


#Mixed#
Pysch_M_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/Psych.xlsx", sheet = "MPsych")
colnames(Pysch_M_data) [1] = "grade_span_n"
colnames(Pysch_M_data) [2] = "position_n"

#conversions
Pysch_M_data = mutate(Pysch_M_data, psychoeducational_assessment_c = Pysch_M_data$`Enter the number of psychoeducational assessments you have written this year.`* 840)
Pysch_M_data = Pysch_M_data %>% select(-c(`Enter the number of psychoeducational assessments you have written this year.`))

Pysch_M_data = mutate(Pysch_M_data, IIS_assessment_c = Pysch_M_data$`Enter the number of Intensive Individual Service (IIS) assessments you have written this year.`* 360)
Pysch_M_data = Pysch_M_data %>% select(-c(`Enter the number of Intensive Individual Service (IIS) assessments you have written this year.`))

Pysch_M_data = mutate(Pysch_M_data, BIP_assessment_c = Pysch_M_data$`Enter the number of Behavior Intervention Plan (BIP) assessments you have written this year.  Please include any you have revised as well.`* 120)
Pysch_M_data = Pysch_M_data %>% select(-c(`Enter the number of Behavior Intervention Plan (BIP) assessments you have written this year.  Please include any you have revised as well.`))

Pysch_M_data = mutate(Pysch_M_data, FBA_assessment_c = Pysch_M_data$`Enter the number of Functional Behavior Assessments (FBA) you have written this year.`* 480)
Pysch_M_data = Pysch_M_data %>% select(-c(`Enter the number of Functional Behavior Assessments (FBA) you have written this year.`))

Pysch_M_data = mutate(Pysch_M_data, MTSS_meeting_c = Pysch_M_data$`Enter the number of MTSS T2/T3 meetings you have attended this year.`* 90)
Pysch_M_data = Pysch_M_data %>% select(-c(`Enter the number of MTSS T2/T3 meetings you have attended this year.`))

Pysch_M_data = mutate(Pysch_M_data, district_IEP_c = Pysch_M_data$`Enter the number of District Staff Involved IEP meetings that you attended this school year.`* 480)
Pysch_M_data = Pysch_M_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you attended this school year.`))

Pysch_M_data = mutate(Pysch_M_data, annual_IEP_c = Pysch_M_data$`Enter the number of annual IEP meetings that you attended this school year.`* 180)
Pysch_M_data = Pysch_M_data %>% select(-c(`Enter the number of annual IEP meetings that you attended this school year.`))

Pysch_M_data = mutate(Pysch_M_data, triennial_IEP_c = Pysch_M_data$`Enter the number of Triennial IEP meetings that you attended this school year.`* 360)
Pysch_M_data = Pysch_M_data %>% select(-c(`Enter the number of Triennial IEP meetings that you attended this school year.`))

Pysch_M_data = mutate(Pysch_M_data, staff_meeting_c = Pysch_M_data$`Enter the number of staffing meetings that you attended this school year.`* 60)
Pysch_M_data = Pysch_M_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

Pysch_M_data = mutate(Pysch_M_data, amendment_meeting_c = Pysch_M_data$`Enter the number of amendment meetings that you attended this school year.`* 90)
Pysch_M_data = Pysch_M_data %>% select(-c(`Enter the number of amendment meetings that you attended this school year.`))

Pysch_M_data = mutate(Pysch_M_data, manifestation_meeting_c = Pysch_M_data$`Enter the number of manifestation determination meetings that you attended this school year.`* 240)
Pysch_M_data = Pysch_M_data %>% select(-c(`Enter the number of manifestation determination meetings that you attended this school year.`))

Pysch_M_data = mutate(Pysch_M_data, transition_meeting_c = Pysch_M_data$`Enter the number of Transition Meetings that you anticipate attending this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`* 120)
Pysch_M_data = Pysch_M_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate attending this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

Pysch_M_data = mutate(Pysch_M_data, interim_IEP_c = Pysch_M_data$`Enter the number of Interim IEPs that you attended this school year.`* 90)
Pysch_M_data = Pysch_M_data %>% select(-c(`Enter the number of Interim IEPs that you attended this school year.`))

Pysch_M_data = mutate(Pysch_M_data, BER_report_c = Pysch_M_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`* 15)
Pysch_M_data = Pysch_M_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

Pysch_M_data = mutate(Pysch_M_data, pulled_days_c = Pysch_M_data$`Enter the number of days you have been pulled from your duties this year.`* 420)
Pysch_M_data = Pysch_M_data %>% select(-c(`Enter the number of days you have been pulled from your duties this year.`))

Pysch_M_data = mutate(Pysch_M_data, sites_travel_c = Pysch_M_data$`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`* 5400)
Pysch_M_data = Pysch_M_data %>% select(-c(`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`))

Pysch_M_data = mutate(Pysch_M_data, miles_travel_c = ((Pysch_M_data$`Enter the number of miles driven between sites in a typical month.`*2)+15))
Pysch_M_data = Pysch_M_data %>% select(-c(`Enter the number of miles driven between sites in a typical month.`))


#creating the rankings

Pysch_M_ranking = transmute(Pysch_M_data, scale_1_5 = (Pysch_M_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
Pysch_M_ranking = mutate(Pysch_M_ranking, min_sum = rowSums(Pysch_M_data[ ,c(3,5:21)]))

write_xlsx(Pysch_M_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/Psych_M_ranking.xlsx")

#jpeg(file="Pysch_M_ranking_plot.jpeg")
#plot(Pysch_M_ranking$scale_1_5,Pysch_M_ranking$min_sum)
#dev.off()



#Pre-School#
Pysch_PS_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/Psych.xlsx", sheet = "PSPsych")
colnames(Pysch_PS_data) [1] = "grade_span_n"
colnames(Pysch_PS_data) [2] = "position_n"

#conversions
Pysch_PS_data = mutate(Pysch_PS_data, psychoeducational_assessment_c = Pysch_PS_data$`Enter the number of psychoeducational assessments you have written this year.`* 840)
Pysch_PS_data = Pysch_PS_data %>% select(-c(`Enter the number of psychoeducational assessments you have written this year.`))

Pysch_PS_data = mutate(Pysch_PS_data, IIS_assessment_c = Pysch_PS_data$`Enter the number of Intensive Individual Service (IIS) assessments you have written this year.`* 360)
Pysch_PS_data = Pysch_PS_data %>% select(-c(`Enter the number of Intensive Individual Service (IIS) assessments you have written this year.`))

Pysch_PS_data = mutate(Pysch_PS_data, BIP_assessment_c = Pysch_PS_data$`Enter the number of Behavior Intervention Plan (BIP) assessments you have written this year.  Please include any you have revised as well.`* 120)
Pysch_PS_data = Pysch_PS_data %>% select(-c(`Enter the number of Behavior Intervention Plan (BIP) assessments you have written this year.  Please include any you have revised as well.`))

Pysch_PS_data = mutate(Pysch_PS_data, FBA_assessment_c = Pysch_PS_data$`Enter the number of Functional Behavior Assessments (FBA) you have written this year.`* 480)
Pysch_PS_data = Pysch_PS_data %>% select(-c(`Enter the number of Functional Behavior Assessments (FBA) you have written this year.`))

Pysch_PS_data = mutate(Pysch_PS_data, MTSS_meeting_c = Pysch_PS_data$`Enter the number of MTSS T2/T3 meetings you have attended this year.`* 90)
Pysch_PS_data = Pysch_PS_data %>% select(-c(`Enter the number of MTSS T2/T3 meetings you have attended this year.`))

Pysch_PS_data = mutate(Pysch_PS_data, district_IEP_c = Pysch_PS_data$`Enter the number of District Staff Involved IEP meetings that you attended this school year.`* 480)
Pysch_PS_data = Pysch_PS_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you attended this school year.`))

Pysch_PS_data = mutate(Pysch_PS_data, annual_IEP_c = Pysch_PS_data$`Enter the number of annual IEP meetings that you attended this school year.`* 180)
Pysch_PS_data = Pysch_PS_data %>% select(-c(`Enter the number of annual IEP meetings that you attended this school year.`))

Pysch_PS_data = mutate(Pysch_PS_data, triennial_IEP_c = Pysch_PS_data$`Enter the number of Triennial IEP meetings that you attended this school year.`* 360)
Pysch_PS_data = Pysch_PS_data %>% select(-c(`Enter the number of Triennial IEP meetings that you attended this school year.`))

Pysch_PS_data = mutate(Pysch_PS_data, staff_meeting_c = Pysch_PS_data$`Enter the number of staffing meetings that you attended this school year.`* 60)
Pysch_PS_data = Pysch_PS_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

Pysch_PS_data = mutate(Pysch_PS_data, amendment_meeting_c = Pysch_PS_data$`Enter the number of amendment meetings that you attended this school year.`* 90)
Pysch_PS_data = Pysch_PS_data %>% select(-c(`Enter the number of amendment meetings that you attended this school year.`))

Pysch_PS_data = mutate(Pysch_PS_data, manifestation_meeting_c = Pysch_PS_data$`Enter the number of manifestation determination meetings that you attended this school year.`* 240)
Pysch_PS_data = Pysch_PS_data %>% select(-c(`Enter the number of manifestation determination meetings that you attended this school year.`))

Pysch_PS_data = mutate(Pysch_PS_data, transition_meeting_c = Pysch_PS_data$`Enter the number of Transition Meetings that you anticipate attending this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`* 120)
Pysch_PS_data = Pysch_PS_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate attending this school year. (PreK, TK, K, 6th, 8th, 12th only-Summary of Performance (SOP))`))

Pysch_PS_data = mutate(Pysch_PS_data, interim_IEP_c = Pysch_PS_data$`Enter the number of Interim IEPs that you attended this school year.`* 90)
Pysch_PS_data = Pysch_PS_data %>% select(-c(`Enter the number of Interim IEPs that you attended this school year.`))

Pysch_PS_data = mutate(Pysch_PS_data, BER_report_c = Pysch_PS_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`* 15)
Pysch_PS_data = Pysch_PS_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

Pysch_PS_data = mutate(Pysch_PS_data, pulled_days_c = Pysch_PS_data$`Enter the number of days you have been pulled from your duties this year.`* 420)
Pysch_PS_data = Pysch_PS_data %>% select(-c(`Enter the number of days you have been pulled from your duties this year.`))

Pysch_PS_data = mutate(Pysch_PS_data, sites_travel_c = Pysch_PS_data$`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`* 5400)
Pysch_PS_data = Pysch_PS_data %>% select(-c(`Enter the number of sites you travel to that do not have a dedicated space/equipment for your services.`))

Pysch_PS_data = mutate(Pysch_PS_data, miles_travel_c = ((Pysch_PS_data$`Enter the number of miles driven between sites in a typical month.`*2)+15))
Pysch_PS_data = Pysch_PS_data %>% select(-c(`Enter the number of miles driven between sites in a typical month.`))


#creating the rankings

Pysch_PS_ranking = transmute(Pysch_PS_data, scale_1_5 = (Pysch_PS_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
Pysch_PS_ranking = mutate(Pysch_PS_ranking, min_sum = rowSums(Pysch_PS_data[ ,c(3,5:21)]))

write_xlsx(Pysch_PS_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/Psych_PS_ranking.xlsx")

#jpeg(file="Pysch_PS_ranking_plot.jpeg")
#plot(Pysch_PS_ranking$scale_1_5,Pysch_PS_ranking$min_sum)
#dev.off()



#Psych Group Ranking#

Pysch_ranking = bind_rows(Pysch_Elm_ranking, Pysch_Int_ranking, Pysch_HS_ranking, Pysch_M_ranking, Pysch_PS_ranking)

write_xlsx(Pysch_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/Psych_ranking.xlsx")

#jpeg(file="Pysch_ranking_plot.jpeg")
#plot(Pysch_ranking$scale_1_5,Pysch_ranking$min_sum)
#dev.off()