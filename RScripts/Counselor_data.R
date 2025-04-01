library(writexl)
library(readxl)
library(tidyverse)

#Elementary#
Couns_Elm_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/Counselors.xlsx", sheet = "ElmCouns")
colnames(Couns_Elm_data) [1] = "grade_span_n"
colnames(Couns_Elm_data) [2] = "position_n"

#conversions
Couns_Elm_data = mutate(Couns_Elm_data, meeting_504_c = Couns_Elm_data$`Enter the number of 504 meetings you have attended this year.`* 60)
Couns_Elm_data = Couns_Elm_data %>% select(-c(`Enter the number of 504 meetings you have attended this year.`))

Couns_Elm_data = mutate(Couns_Elm_data, district_IEP_c = Couns_Elm_data$`Enter the number of District Staff Involved IEP meetings that you attended this school year.`* 240)
Couns_Elm_data = Couns_Elm_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you attended this school year.`))

Couns_Elm_data = mutate(Couns_Elm_data, meeting_IEP_c = Couns_Elm_data$`Enter the number of IEP meetings that you attended this school year.`* 60)
Couns_Elm_data = Couns_Elm_data %>% select(-c(`Enter the number of IEP meetings that you attended this school year.`))

Couns_Elm_data = mutate(Couns_Elm_data, staff_meeting_c = Couns_Elm_data$`Enter the number of staffing meetings that you attended this school year.`* 60)
Couns_Elm_data = Couns_Elm_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

Couns_Elm_data = mutate(Couns_Elm_data, manifestation_meeting_c = Couns_Elm_data$`Enter the number of manifestation determination meetings that you attended this school year.`* 60)
Couns_Elm_data = Couns_Elm_data %>% select(-c(`Enter the number of manifestation determination meetings that you attended this school year.`))

Couns_Elm_data = mutate(Couns_Elm_data, transition_meeting_c = Couns_Elm_data$`Enter the number of Transition Meetings that you anticipate attending this school year. (PreK, TK, 6th, 8th, 12th only-Summary of Performance (SOP))`* 60)
Couns_Elm_data = Couns_Elm_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate attending this school year. (PreK, TK, 6th, 8th, 12th only-Summary of Performance (SOP))`))

Couns_Elm_data = mutate(Couns_Elm_data, BER_report_c = Couns_Elm_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`* 15)
Couns_Elm_data = Couns_Elm_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

Couns_Elm_data = mutate(Couns_Elm_data, crisis_response_c = Couns_Elm_data$`Enter the number of crisis responses you have conducted this year.`* 90)
Couns_Elm_data = Couns_Elm_data %>% select(-c(`Enter the number of crisis responses you have conducted this year.`))

Couns_Elm_data = mutate(Couns_Elm_data, RAT_meeting_c = Couns_Elm_data$`Enter the number of Risk Assessment Team Meetings you have participated in this year.`* 90)
Couns_Elm_data = Couns_Elm_data %>% select(-c(`Enter the number of Risk Assessment Team Meetings you have participated in this year.`))

Couns_Elm_data = mutate(Couns_Elm_data, individual_session_c = Couns_Elm_data$`Enter the number of individual counseling sessions you have conducted this year.`* 30)
Couns_Elm_data = Couns_Elm_data %>% select(-c(`Enter the number of individual counseling sessions you have conducted this year.`))

Couns_Elm_data = mutate(Couns_Elm_data, group_session_c = Couns_Elm_data$`Enter the number of group counseling sessions you have conducted this year.`* 30)
Couns_Elm_data = Couns_Elm_data %>% select(-c(`Enter the number of group counseling sessions you have conducted this year.`))

Couns_Elm_data = mutate(Couns_Elm_data, SEL_presentation_c = Couns_Elm_data$`Enter the number of SEL presentations you have given this year.`* 50)
Couns_Elm_data = Couns_Elm_data %>% select(-c(`Enter the number of SEL presentations you have given this year.`))

Couns_Elm_data = mutate(Couns_Elm_data, academic_presentation_c = Couns_Elm_data$`Enter the number of academic presentations you have given this year.`* 50)
Couns_Elm_data = Couns_Elm_data %>% select(-c(`Enter the number of academic presentations you have given this year.`))

Couns_Elm_data = mutate(Couns_Elm_data, intakes__c = Couns_Elm_data$`Enter the number of intakes you have conducted this year.`* 30)
Couns_Elm_data = Couns_Elm_data %>% select(-c(`Enter the number of intakes you have conducted this year.`))

Couns_Elm_data = mutate(Couns_Elm_data, days_pulled_c = Couns_Elm_data$`Enter the number of days you have been pulled from your duties this year.`* 420)
Couns_Elm_data = Couns_Elm_data %>% select(-c(`Enter the number of days you have been pulled from your duties this year.`))





#creating the rankings

Couns_Elm_ranking = transmute(Couns_Elm_data, scale_1_5 = (Couns_Elm_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
Couns_Elm_ranking = mutate(Couns_Elm_ranking, min_sum = rowSums(Couns_Elm_data[ ,c(7,9:23)]))

write_xlsx(Couns_Elm_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/Counselors_Elm_ranking.xlsx")

jpeg(file="Couns_Elm_ranking_plot.jpeg")
plot(Couns_Elm_ranking$scale_1_5,Couns_Elm_ranking$min_sum)
dev.off()



#Intermediate#
Couns_Int_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/Counselors.xlsx", sheet = "IntCouns")
colnames(Couns_Int_data) [1] = "grade_span_n"
colnames(Couns_Int_data) [2] = "position_n"

#conversions
Couns_Int_data = mutate(Couns_Int_data, meeting_504_c = Couns_Int_data$`Enter the number of 504 meetings you have attended this year.`* 60)
Couns_Int_data = Couns_Int_data %>% select(-c(`Enter the number of 504 meetings you have attended this year.`))

Couns_Int_data = mutate(Couns_Int_data, district_IEP_c = Couns_Int_data$`Enter the number of District Staff Involved IEP meetings that you attended this school year.`* 240)
Couns_Int_data = Couns_Int_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you attended this school year.`))

Couns_Int_data = mutate(Couns_Int_data, meeting_IEP_c = Couns_Int_data$`Enter the number of IEP meetings that you attended this school year.`* 60)
Couns_Int_data = Couns_Int_data %>% select(-c(`Enter the number of IEP meetings that you attended this school year.`))

Couns_Int_data = mutate(Couns_Int_data, staff_meeting_c = Couns_Int_data$`Enter the number of staffing meetings that you attended this school year.`* 60)
Couns_Int_data = Couns_Int_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

Couns_Int_data = mutate(Couns_Int_data, manifestation_meeting_c = Couns_Int_data$`Enter the number of manifestation determination meetings that you attended this school year.`* 60)
Couns_Int_data = Couns_Int_data %>% select(-c(`Enter the number of manifestation determination meetings that you attended this school year.`))

Couns_Int_data = mutate(Couns_Int_data, transition_meeting_c = Couns_Int_data$`Enter the number of Transition Meetings that you anticipate attending this school year. (PreK, TK, 6th, 8th, 12th only-Summary of Performance (SOP))`* 60)
Couns_Int_data = Couns_Int_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate attending this school year. (PreK, TK, 6th, 8th, 12th only-Summary of Performance (SOP))`))

Couns_Int_data = mutate(Couns_Int_data, BER_report_c = Couns_Int_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`* 15)
Couns_Int_data = Couns_Int_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

Couns_Int_data = mutate(Couns_Int_data, crisis_response_c = Couns_Int_data$`Enter the number of crisis responses you have conducted this year.`* 90)
Couns_Int_data = Couns_Int_data %>% select(-c(`Enter the number of crisis responses you have conducted this year.`))

Couns_Int_data = mutate(Couns_Int_data, RAT_meeting_c = Couns_Int_data$`Enter the number of Risk Assessment Team Meetings you have participated in this year.`* 90)
Couns_Int_data = Couns_Int_data %>% select(-c(`Enter the number of Risk Assessment Team Meetings you have participated in this year.`))

Couns_Int_data = mutate(Couns_Int_data, individual_session_c = Couns_Int_data$`Enter the number of individual counseling sessions you have conducted this year.`* 30)
Couns_Int_data = Couns_Int_data %>% select(-c(`Enter the number of individual counseling sessions you have conducted this year.`))

Couns_Int_data = mutate(Couns_Int_data, group_session_c = Couns_Int_data$`Enter the number of group counseling sessions you have conducted this year.`* 60)
Couns_Int_data = Couns_Int_data %>% select(-c(`Enter the number of group counseling sessions you have conducted this year.`))

Couns_Int_data = mutate(Couns_Int_data, SEL_presentation_c = Couns_Int_data$`Enter the number of SEL presentations you have given this year.`* 50)
Couns_Int_data = Couns_Int_data %>% select(-c(`Enter the number of SEL presentations you have given this year.`))

Couns_Int_data = mutate(Couns_Int_data, academic_presentation_c = Couns_Int_data$`Enter the number of academic presentations you have given this year.`* 50)
Couns_Int_data = Couns_Int_data %>% select(-c(`Enter the number of academic presentations you have given this year.`))

Couns_Int_data = mutate(Couns_Int_data, intakes__c = Couns_Int_data$`Enter the number of intakes you have conducted this year.`* 30)
Couns_Int_data = Couns_Int_data %>% select(-c(`Enter the number of intakes you have conducted this year.`))

Couns_Int_data = mutate(Couns_Int_data, days_pulled_c = Couns_Int_data$`Enter the number of days you have been pulled from your duties this year.`* 420)
Couns_Int_data = Couns_Int_data %>% select(-c(`Enter the number of days you have been pulled from your duties this year.`))





#creating the rankings

Couns_Int_ranking = transmute(Couns_Int_data, scale_1_5 = (Couns_Int_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
Couns_Int_ranking = mutate(Couns_Int_ranking, min_sum = rowSums(Couns_Int_data[ ,c(7,9:23)]))

write_xlsx(Couns_Int_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/Counselors_Int_ranking.xlsx")

jpeg(file="Couns_Int_ranking_plot.jpeg")
plot(Couns_Int_ranking$scale_1_5,Couns_Int_ranking$min_sum)
dev.off()



#High School#
Couns_HS_data = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/Counselors.xlsx", sheet = "HSCouns")
colnames(Couns_HS_data) [1] = "grade_span_n"
colnames(Couns_HS_data) [2] = "position_n"

#conversions
Couns_HS_data = mutate(Couns_HS_data, meeting_504_c = Couns_HS_data$`Enter the number of 504 meetings you have attended this year.`* 60)
Couns_HS_data = Couns_HS_data %>% select(-c(`Enter the number of 504 meetings you have attended this year.`))

Couns_HS_data = mutate(Couns_HS_data, district_IEP_c = Couns_HS_data$`Enter the number of District Staff Involved IEP meetings that you attended this school year.`* 240)
Couns_HS_data = Couns_HS_data %>% select(-c(`Enter the number of District Staff Involved IEP meetings that you attended this school year.`))

Couns_HS_data = mutate(Couns_HS_data, meeting_IEP_c = Couns_HS_data$`Enter the number of IEP meetings that you attended this school year.`* 60)
Couns_HS_data = Couns_HS_data %>% select(-c(`Enter the number of IEP meetings that you attended this school year.`))

Couns_HS_data = mutate(Couns_HS_data, staff_meeting_c = Couns_HS_data$`Enter the number of staffing meetings that you attended this school year.`* 60)
Couns_HS_data = Couns_HS_data %>% select(-c(`Enter the number of staffing meetings that you attended this school year.`))

Couns_HS_data = mutate(Couns_HS_data, manifestation_meeting_c = Couns_HS_data$`Enter the number of manifestation determination meetings that you attended this school year.`* 60)
Couns_HS_data = Couns_HS_data %>% select(-c(`Enter the number of manifestation determination meetings that you attended this school year.`))

Couns_HS_data = mutate(Couns_HS_data, transition_meeting_c = Couns_HS_data$`Enter the number of Transition Meetings that you anticipate attending this school year. (PreK, TK, 6th, 8th, 12th only-Summary of Performance (SOP))`* 60)
Couns_HS_data = Couns_HS_data %>% select(-c(`Enter the number of Transition Meetings that you anticipate attending this school year. (PreK, TK, 6th, 8th, 12th only-Summary of Performance (SOP))`))

Couns_HS_data = mutate(Couns_HS_data, BER_report_c = Couns_HS_data$`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`* 15)
Couns_HS_data = Couns_HS_data %>% select(-c(`Enter the number of Behavioral Emergency Reports (BER) - also known as the Hands-on Report you have written this year.`))

Couns_HS_data = mutate(Couns_HS_data, crisis_response_c = Couns_HS_data$`Enter the number of crisis responses you have conducted this year.`* 90)
Couns_HS_data = Couns_HS_data %>% select(-c(`Enter the number of crisis responses you have conducted this year.`))

Couns_HS_data = mutate(Couns_HS_data, RAT_meeting_c = Couns_HS_data$`Enter the number of Risk Assessment Team Meetings you have participated in this year.`* 90)
Couns_HS_data = Couns_HS_data %>% select(-c(`Enter the number of Risk Assessment Team Meetings you have participated in this year.`))

Couns_HS_data = mutate(Couns_HS_data, individual_session_c = Couns_HS_data$`Enter the number of individual counseling sessions you have conducted this year.`* 30)
Couns_HS_data = Couns_HS_data %>% select(-c(`Enter the number of individual counseling sessions you have conducted this year.`))

Couns_HS_data = mutate(Couns_HS_data, group_session_c = Couns_HS_data$`Enter the number of group counseling sessions you have conducted this year.`* 60)
Couns_HS_data = Couns_HS_data %>% select(-c(`Enter the number of group counseling sessions you have conducted this year.`))

Couns_HS_data = mutate(Couns_HS_data, SEL_presentation_c = Couns_HS_data$`Enter the number of SEL presentations you have given this year.`* 50)
Couns_HS_data = Couns_HS_data %>% select(-c(`Enter the number of SEL presentations you have given this year.`))

Couns_HS_data = mutate(Couns_HS_data, academic_presentation_c = Couns_HS_data$`Enter the number of academic presentations you have given this year.`* 50)
Couns_HS_data = Couns_HS_data %>% select(-c(`Enter the number of academic presentations you have given this year.`))

Couns_HS_data = mutate(Couns_HS_data, intakes__c = Couns_HS_data$`Enter the number of intakes you have conducted this year.`* 30)
Couns_HS_data = Couns_HS_data %>% select(-c(`Enter the number of intakes you have conducted this year.`))

Couns_HS_data = mutate(Couns_HS_data, days_pulled_c = Couns_HS_data$`Enter the number of days you have been pulled from your duties this year.`* 420)
Couns_HS_data = Couns_HS_data %>% select(-c(`Enter the number of days you have been pulled from your duties this year.`))





#creating the rankings

Couns_HS_ranking = transmute(Couns_HS_data, scale_1_5 = (Couns_HS_data$`On a scale from 1 - 5, what would you rate your current level of workload?`))
Couns_HS_ranking = mutate(Couns_HS_ranking, min_sum = rowSums(Couns_HS_data[ ,c(7,9:23)]))

write_xlsx(Couns_HS_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/Counselors_HS_ranking.xlsx")

jpeg(file="Couns_HS_ranking_plot.jpeg")
plot(Couns_HS_ranking$scale_1_5,Couns_HS_ranking$min_sum)
dev.off()





#Counselor Group Ranking#

Couns_ranking = bind_rows(Couns_Elm_ranking, Couns_Int_ranking, Couns_HS_ranking)

write_xlsx(Couns_ranking, path = "~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/FINALData/Counselors_ranking.xlsx")

jpeg(file="Couns_ranking_plot.jpeg")
plot(Couns_ranking$scale_1_5,Couns_ranking$min_sum)
dev.off()