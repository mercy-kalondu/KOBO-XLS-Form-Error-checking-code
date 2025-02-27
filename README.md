# KOBO-XLS-Form-Error-checking-code
The code generates an app to check errors in a KOBO XLS form 
This R Shiny application is designed to validate XLSForms used in KoboToolbox, ensuring data quality and integrity by identifying errors and inconsistencies in survey forms.
Key Features & Functionality
Load & Read XLSForm
•	The script reads three sheets from an XLSForm Excel file:
o	"survey" (questionnaire structure)
o	"choices" (multiple-choice options)
o	"settings" (optional configuration)
Data Validation & Error Checking
The check_kobo_errors() function performs multiple data integrity checks, including:
•	Missing question names → Ensures all questions have a valid "name".
•	Duplicate question names → Ensures "name" fields are unique (excluding "begin_group" and "end_group").
•	Duplicate choices in the "choices" sheet → Checks if "name" or "label" fields are repeated within the same "list_name".
•	Integer questions missing constraints → Ensures "integer" type questions have constraints for valid input.
•	Inappropriate labels for question types → Checks if "label" fields are correctly formatted based on question types.
•	Conflicting choices ("Do not know", "Prefer not to answer") → Ensures they are not selected with other options.
•	Missing follow-up text questions for 'Other' options → Ensures "select_one" and "select_multiple" questions with "Other" have an associated text field.
Shiny User Interface (UI)
•	Allows users to upload an XLSForm file (fileInput).
•	Displays identified errors in a table format (tableOutput).
•	Provides an option to download the error report and a README file (downloadButton).
Shiny Server Logic
•	Processes the uploaded file using reactive() functions.
•	Displays detected errors in a table (renderTable).
•	Generates a downloadable Excel file with errors and a README (downloadHandler).
Exporting Results
•	Errors are compiled into an Excel file (openxlsx::createWorkbook()), where each error type is saved in a separate sheet.
•	A README sheet explains the validation rules.
