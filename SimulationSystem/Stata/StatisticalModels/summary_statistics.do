// summary_statistics.do

// Clear existing data
clear

// Load the dataset
use "data.dta", clear

// Check if the dataset loaded successfully
if _rc != 0 {
    display "Error: Could not load the dataset."
    exit 1
}

// Display basic information about the dataset
describe

// Summarize all variables in the dataset
display "Summary Statistics for All Variables:"
summarize

// Check for missing values in the dataset
foreach var of varlist _all {
    local missing_count = missing(`var')
    if `missing_count' > 0 {
        display "Variable `var' has `missing_count' missing values."
    }
}

// Optionally, summarize specific variables (customize as needed)
local specific_vars "outcome_variable predictor_variable"
display "Summary Statistics for Specific Variables:"
summarize `specific_vars'

// Save summary statistics to a text file
file open myfile using "summary_statistics.txt", write replace

// Write header to the file
file write myfile "Summary Statistics:\n"
file write myfile "--------------------------------\n"

// Capture summary statistics and write them to the file
foreach var of varlist _all {
    summarize `var'
    file write myfile "`var': N = `r(N)', Mean = `r(mean)', Std. Dev. = `r(sd)', Min = `r(min)', Max = `r(max)'\n"
}

// Close the file
file close myfile

// Notify user that results have been saved
display "Summary statistics saved to 'summary_statistics.txt'."
