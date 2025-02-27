# Load necessary libraries
library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)

# Function to check for errors in the XLSForm
check_kobo_errors <- function(file_path) {
  survey <- read_excel(file_path, sheet = "survey")
  choices <- read_excel(file_path, sheet = "choices")
  settings <- tryCatch(read_excel(file_path, sheet = "settings"), error = function(e) NULL)
  
  errors <- list()
  
  # 1. Check for missing or invalid values
  missing_names <- survey %>%
    filter(is.na(name) | name == "") %>%
    select(type, name, label)
  
  if(nrow(missing_names) > 0) {
    errors$missing_names <- missing_names
  }
  
  # 3. Check for duplicate names (exempting begin_group and end_group)
  duplicate_names <- survey %>%
    filter(!type %in% c("begin_group", "end_group")) %>%
    group_by(name) %>%
    filter(n() > 1) %>%
    select(type, name, label)
  
  if(nrow(duplicate_names) > 0) {
    errors$duplicate_names <- duplicate_names
  }
  
  # # 4. Check cascading selects
  # missing_choices <- choices %>%
  #   filter(!name %in% unique(survey$name)) %>%
  #   select(list_name, name, label)
  # 
  # if(nrow(missing_choices) > 0) {
  #   errors$missing_choices <- missing_choices
  # }
  
  # 6. Check for duplicated choices in 'choices' sheet (name and label)
  duplicate_choices_name <- choices %>%
    group_by(list_name, name) %>%
    filter(n() > 1) %>%
    select(list_name, name, label)
  
  if(nrow(duplicate_choices_name) > 0) {
    errors$duplicate_choices_name <- duplicate_choices_name
  }
  
  duplicate_choices_label <- choices %>%
    group_by(list_name, label) %>%
    filter(n() > 1) %>%
    select(list_name, name, label)
  
  if(nrow(duplicate_choices_label) > 0) {
    errors$duplicate_choices_label <- duplicate_choices_label
  }
  
  # # 7. Check for mismatched names and labels in 'choices' sheet
  # mismatched_choices <- choices %>%
  #   filter(name != label) %>%
  #   select(list_name, name, label)
  # 
  # if(nrow(mismatched_choices) > 0) {
  #   errors$mismatched_choices <- mismatched_choices
  # }
  
  # 8. Check for integer questions without constraints
  missing_constraints <- survey %>%
    filter(type == "integer" & (is.na(constraint) | constraint == "")) %>%
    select(type, name, label)
  
  if(nrow(missing_constraints) > 0) {
    errors$missing_constraints <- missing_constraints
  }
  # 9. Check if label is appropriate for type
  inappropriate_labels <- survey %>%
    rowwise() %>%
    mutate(
      label_issue = case_when(
        type %in% c("text", "textarea") & !is.character(label) ~ "Label should be textual",
        type %in% c("integer") & !is.character(label) ~ "Label should be textual but can contain numbers",
        type %in% c("select_one", "select_multiple") & is.na(label) ~ "Label should be descriptive and non-empty",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(label_issue)) %>%
    select(type, name, label, label_issue)
  
  if(nrow(inappropriate_labels) > 0) {
    errors$inappropriate_labels <- inappropriate_labels
  }
  
  # 10. Check for cases where "Do not know" and "Prefer not to answer" are selected with other choices
  conflict_choices <- choices %>%
    filter(label %in% c("Do not know", "Prefer not to answer", "nothing", "Don’t know", "Prefer not to answer")) %>%
    select(list_name, name, label)
  
  if(nrow(conflict_choices) > 0) {
    errors$conflict_choices <- conflict_choices
  }
  
  # 10. Check for "Other" options without follow-up text question
  other_choices <- choices %>%
    filter(str_detect(label, regex("\\b(Other|others|other)\\b", ignore_case = TRUE)))
  
  # Join 'survey' and 'choices' on 'list_name' to find corresponding questions
  missing_followup <- survey %>%
    filter(name %in% other_choices$list_name & type %in% c("select_one", "select_multiple")) %>%
    mutate(next_question = lead(type),
           next_name = lead(name)) %>%
    filter(is.na(next_question) | !next_question %in% c("text", "textarea")) %>%
    select(type, name, label)
  
  if(nrow(missing_followup) > 0) {
    errors$missing_followup <- missing_followup
  }
  
  return(errors)
}

# Shiny UI
ui <- fluidPage(
  titlePanel("KoboToolbox XLSForm Error Checker"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload XLSForm", accept = c(".xlsx")),
      downloadButton("downloadData", "Download Errors and README")
    ),
    
    mainPanel(
      tableOutput("errorTable"),
      textOutput("noErrors")
    )
  )
)

# Shiny Server
server <- function(input, output) {
  errors <- reactive({
    req(input$file)
    check_kobo_errors(input$file$datapath)
  })
  
  output$errorTable <- renderTable({
    req(errors())
    error_list <- errors()
    
    if (length(error_list) == 0) {
      return(NULL)
    }
    
    error_df <- bind_rows(lapply(error_list, function(x) {
      if (is.data.frame(x)) {
        return(x)
      } else {
        return(data.frame())
      }
    }), .id = "Error_Type")
    
    error_df
  })
  
  output$noErrors <- renderText({
    req(errors())
    
    if (length(errors()) == 0) {
      return("No errors found!")
    } else {
      return("")
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("kobo_errors_and_readme", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(errors())
      error_list <- errors()
      
      if (length(error_list) == 0) {
        return(NULL)
      }
      
      wb <- createWorkbook()
      
      # Add error sheets
      for (error_name in names(error_list)) {
        addWorksheet(wb, error_name)
        writeData(wb, error_name, error_list[[error_name]], startCol = 1, startRow = 1)
      }
      
      # Add README sheet
      addWorksheet(wb, "README")
      writeData(wb, "README", c(
        "KoboToolbox XLSForm Error Checker - README",
        "",
        "1. Missing Names",
        "   - This error indicates that there are questions in the survey sheet where the 'name' column is either empty or missing. Every question must have a valid 'name'.",
        "",
        "2. Duplicate Names",
        "   - This error shows questions in the survey sheet that have the same 'name', excluding 'begin_group' and 'end_group'. Each question 'name' should be unique.",
        "",
        "3. Missing Choices",
        "   - This error indicates that there are choices listed in the 'choices' sheet that are not referenced in the 'survey' sheet. All choices should be correctly referenced.",
        "",
        "4. Duplicate Choices (by Name)",
        "   - This error lists choices in the 'choices' sheet where the 'name' is duplicated within the same 'list_name'. Each choice 'name' should be unique within a list.",
        "",
        "5. Duplicate Choices (by Label)",
        "   - This error lists choices in the 'choices' sheet where the 'label' is duplicated within the same 'list_name'. Each choice 'label' should be unique within a list.",
        "",
        "6. Mismatched Names and Labels",
        "   - This error shows choices where the 'name' does not match the 'label'. Typically, 'name' should be a unique identifier, while 'label' is the human-readable text.",
        "",
        "7. Integer Questions Without Constraints",
        "   - This error indicates integer type questions in the survey sheet that do not have constraints defined. Integer questions should have constraints to ensure valid input.",
       "",
       "8. Appropriate label columns",
        "   - This error indicates if the label column's values are suitable based on the type of each question.",
       "",
       "9. conflict_choices",
       "   - Indicates slect multiple questions which needs a constraint because they contain the choices do not know and prefer not to answer which cannot be selected with any other choices in the list.",
       "",
       "9. missing_followup",
       "   - checks for questions containig the other option and do not allow for typing the other option ."
        ), startCol = 1, startRow = 1)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
