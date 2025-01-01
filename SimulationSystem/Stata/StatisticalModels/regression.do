// regression.do

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

// Summarize the outcome and predictor variables
summarize outcome_variable predictor_variable

// Perform regression analysis
regress outcome_variable predictor_variable

// Check for regression results and display them clearly
if e(rc) == 0 {
    // Display regression results
    display "Regression Analysis Results:"
    display "--------------------------------"
    display "Coefficient for Predictor Variable: " _b[predictor_variable]
    display "Standard Error: " _se[predictor_variable]
    display "t-Statistic: " _b[predictor_variable] / _se[predictor_variable]
    display "P-value: " 2 * ttail(e(df_r), abs(_b[predictor_variable] / _se[predictor_variable]))

    // Optionally, display R-squared value
    display "R-squared: " e(r2)
} else {
    display "Error: Regression analysis failed."
}

// Save regression output to a text file
file open myfile using "regression_results.txt", write replace
file write myfile "Regression Analysis Results:\n"
file write myfile "--------------------------------\n"
file write myfile "Coefficient for Predictor Variable: " + string(_b[predictor_variable]) + "\n"
file write myfile "Standard Error: " + string(_se[predictor_variable]) + "\n"
file write myfile "t-Statistic: " + string(_b[predictor_variable] / _se[predictor_variable]) + "\n"
file write myfile "P-value: " + string(2 * ttail(e(df_r), abs(_b[predictor_variable] / _se[predictor_variable]))) + "\n"
file write myfile "R-squared: " + string(e(r2)) + "\n"
file close myfile

// Notify user that results have been saved
display "Regression results saved to 'regression_results.txt'."
