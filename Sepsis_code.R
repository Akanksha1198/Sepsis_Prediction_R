## Simulate data to use in the datacamp project
library(data.table)
setwd('~/Desktop/Projects/MedTourEasy')
set.seed(5)
# Simulate patient ids
n = 890
ids = formatC(sample(1:3000, size = n, replace = FALSE),
  	width = 4, flag = '0') 




## Simulate antibiotic data
antibiotic_types = c('amoxicillin', 'ciprofloxacin', 'doxycycline', 'penicillin')
antibiotic_set_up = data.table(
  id = ids,
  number_of_abx = rnbinom(n, 0.8, 0.08)
	)
samp_days = function(num_days){
  sample(1:19, prob = 19:1, size = num_days, replace = TRUE)}
antibioticDT = data.table(
  patient_id = antibiotic_set_up[ , rep(id, number_of_abx)],
  day_given = unlist(lapply(antibiotic_set_up$number_of_abx,
  	FUN = samp_days))
	)
invisible(antibioticDT[ , antibiotic_type := sample(antibiotic_types, 
  size = .N,
  replace = TRUE,
  prob = c(1, 2, 6, 2))])
setorder(antibioticDT, patient_id, day_given, antibiotic_type)
invisible(antibioticDT[ , route := sample(c('PO', 'IV'), 
  prob = c(1, 4), 
  size = .N, replace = TRUE)])
antibioticDT = unique(antibioticDT)



# Simulate blood culture data
blood_culture_set_up = data.table(
  patient_id = ids,
  number_of_cultures = sample(0:5, 
    size = n,
    prob = c(22, 10, 5, 3, 1, 1),
    replace = TRUE)
	)

blood_cultureDT = unique(data.table(
  patient_id = blood_culture_set_up[ , rep(patient_id, number_of_cultures)],
  blood_culture_day = unlist(lapply(blood_culture_set_up$number_of_cultures, 
	FUN = samp_days))
	))

setorder(blood_cultureDT, patient_id, blood_culture_day)
blood_cultureDT[1:20]

length(union(antibioticDT$patient_id, blood_cultureDT$patient_id))


write.table(
  blood_cultureDT,
  file = 'blood_cultureDT.csv',
  #file = 'C:/Users/joann.alvarez/datacamp/blood_cultureDT.csv', 
  sep = ",",
  row.names = FALSE)
write.table(
  antibioticDT,
  file = 'antibioticDT.csv', 
  sep = ",",
  row.names = FALSE)
write.table(
  data.table(patient_id = ids)
  , file = 'all_patients.csv'
  , sep = ','
  , row.names = FALSE)

#import antibioticDT dataset
antibioticDT2 = fread('antibioticDT.csv')

# Look at the first 30 rows
antibioticDT2[1:30, ]

# Sort the data by id, antibiotic type, day
setorder(antibioticDT2, patient_id, antibiotic_type, day_given)
antibioticDT[1:40, ]

# Use shift to calculate the last day a particular drug was administered
antibioticDT2[ , last_administration_day := shift(day_given, 1), 
              by = .(patient_id, antibiotic_type)]

# Calculate the number of days since the drug was last administered
antibioticDT2[ , days_since_last_admin := day_given - last_administration_day]

# Create antibiotic_new with an initial value of one, then reset it to zero as needed
antibioticDT2[ , antibiotic_new := 1]
antibioticDT2[days_since_last_admin <= 2, antibiotic_new := 0]

# Read in blood_cultureDT.csv
blood_cultureDT <- fread('blood_cultureDT.csv')

# Print the first 30 rows
blood_cultureDT[1:30]

# Merge antibioticDT with blood_cultureDT
combinedDT <- merge(blood_cultureDT, antibioticDT2, all = FALSE, by = 'patient_id')

# Sort by patient_id, blood_culture_day, day_given, and antibiotic_type
setorder(combinedDT, patient_id, blood_culture_day, day_given, antibiotic_type)

# Print and examine the first 30 rows
combinedDT[1:40]

# Make a new variable called drug_in_bcx_window
combinedDT[ , 
            drug_in_bcx_window := 
              as.numeric(
                day_given - blood_culture_day <= 2 
                & 
                  day_given - blood_culture_day >= -2)]

# Create a variable indicating if there was at least one I.V. drug given in the window
combinedDT[ , 
            any_iv_in_bcx_window := as.numeric(any(route == 'IV' & drug_in_bcx_window == 1)),
            by = .(patient_id, blood_culture_day)]

# Exclude rows in which the blood_culture_day does not have any I.V. drugs in window 
combinedDT <- combinedDT[any_iv_in_bcx_window == 1]

# Create a new variable called day_of_first_new_abx_in_window
combinedDT[ , 
            day_of_first_new_abx_in_window := 
              day_given[antibiotic_new == 1 & drug_in_bcx_window == 1][1],
            by = .(patient_id, blood_culture_day)]

# Remove rows where the day is before this first qualifying day
combinedDT <- combinedDT[day_given >= day_of_first_new_abx_in_window]

# Create a new data.table containing only patient_id, blood_culture_day, and day_given
simplified_data <- combinedDT[ , .(patient_id, blood_culture_day, day_given)]

# Remove duplicate rows
simplified_data <- unique(simplified_data)

# Count the antibiotic days within each patient/blood culture day combination
simplified_data[ , num_antibiotic_days := .N, by = .(patient_id, blood_culture_day)]

# Remove blood culture days with less than four rows 
simplified_data <- simplified_data[num_antibiotic_days >= 4]

# Select the first four days for each blood culture
first_four_days <- simplified_data[ , .SD[1:4], by = .(patient_id, blood_culture_day)]

# Make the indicator for consecutive sequence
first_four_days[ , four_in_seq := as.numeric(max(diff(day_given)) < 3), by = .(patient_id, blood_culture_day)]

# Select the rows which have four_in_seq equal to 1
suspected_infection <- first_four_days[four_in_seq == 1]

# Retain only the patient_id column
suspected_infection <- suspected_infection[ , .(patient_id)]

# Remove duplicates
suspected_infection <- unique(suspected_infection)

# Make an infection indicator
suspected_infection[ , infection := 1]

# Read in "all_patients.csv"
all_patientsDT <- fread("all_patients.csv")

# Merge this with the infection flag data
all_patientsDT <- merge(
  all_patientsDT,
  suspected_infection,
  by = "patient_id",
  all = TRUE
)

# Set any missing values of the infection flag to 0
all_patientsDT[is.na(infection) , infection := 0]

# Calculate the percentage of patients who met the criteria for presumed infection
ans <- all_patientsDT[ , 100*mean(infection == 1)]


