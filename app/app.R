# ------------------------------
#  SEMANTICA
#  Authors: Pedro L Rodriguez
#  Last modified: 02-25-2019
#
#   CODE SECTIONS
#
#   SECTION A: PRELIMS
#   SECTION B: USER INTERFACE
#   SECTION C: SERVER
#
# ------------------

# --------------------------------
#
#
# SECTION A: PRELIMS    ----
#
#
# --------------------------------

# --------------------------
# SECTION A1: load libraries ----
# --------------------------
library(shiny)
library(rdrop2)
library(dplyr)
library(shiny.semantic)
library(purrr)
library(magrittr)
library(data.table)

# unique mturk id: glove - human & 6_300 - human: b57b888b6cb9a6cb456092deb3767bef
# unique mturk id: glove - 6_300: 9c64e87374d0e7a641f1707665cfbfaf
# unique mturk id: 6-300 48-300 d112d021c7964d7319326f79c9c5abc7
# w2v vs glove: e30563d22a1cc478cdfac01af96cfc9c

# --------------------------
# SECTION A2: load data and functions----
# --------------------------
models <- c("glove", "w2v")
nn_list <- readRDS("data/nn_list.rds")
screening_data <- readRDS("data/screening_data.rds")
trial_data <- readRDS("data/trial_data.rds")
source("TriadTask.R")
source("triad_pairs.R")

# subset nn_list models of interest
nn_list <- nn_list[models]

# parameter choices
options(digits.secs = 6) # set timer precision
compensation <- "1.00"

# --------------------------
# SECTION A3: data saving    -----
# --------------------------
saveDataLocation <- "dropbox"  # either dropbox, email, or local
#outputDir <- "GitHub/EmbeddingsProject/RShiny/Triad Task/Output"  # directory to save data
outputDir <- "NYU/Teaching/Text as Data/Embeddings/MTurk"  # directory to save data

# Dropbox
droptoken <- readRDS("droptoken.rds")   # reads in authentication for dropbox (must be store in same folder as this code)

# --------------------------
# SECTION A4: java scripts   -----
# --------------------------

# js to register when keyboard is used
# Shiny.onInputChange only reacts when the js object it references changes
# to solve this we add a random number generator
# see: https://stackoverflow.com/questions/35831811/register-repeated-keyboard-presses-in-shiny
keyboard <- ' $(document).on("keydown", function (e) {
Shiny.onInputChange("lastkeypresscode", [e.which, Math.random()]); 
});
'

# --------------------------------
#
#
# SECTION B: USER INTERFACE    ----
#
#
# --------------------------------
ui <- fluidPage(
  theme = "cosmo.css",   # css theme dowloaded from bootswatch.com (https://bootswatch.com/3/)
  tags$script(keyboard),     # load java scripts
  title = "Semantica",       # define title
  uiOutput("MainAction"),    # render function for dynamic ui
  tags$style(type = "text/css", ".recalculating {opacity: 1.0;}")   # prevents gray screen during Sys.sleep()
  #tags$style("input[type=checkbox] {
  #                  transform: scale(2.5);
  #           }") # checkbox size # added it directly to css
)

# --------------------------------
#
#
# SECTION C: SERVER FUNCTION     ----
#
#
# --------------------------------
server <- function(input, output, session) {
  
  # --------------------------
  # SECTION A6: tt data.table      -----
  # --------------------------
  N <- 1  # number of candidates to sample for each cue
  cues <- sample(names(nn_list[[1]])) # randomize order of cues
  tt_data <- lapply(cues, function(x) triad_pairs(nn1 = nn_list[[models[1]]][[x]], nn2 = nn_list[[models[2]]][[x]], model_names = models, num_pairs = N, rm_same = TRUE, rand_order = TRUE) %>% cbind(cue = x, .)) %>% do.call(rbind, .)
  # add additional workerid etc.
  tt_data <- tt_data %>% mutate(workerid = as.character(NA), screener = FALSE, left.correct = as.character(NA), right.correct = as.character(NA))
  # reorder columns
  tt_data <- tt_data[c("workerid", "cue", "left.source", "right.source", "left.word", "right.word", "screener", "left.rank", "right.rank", "left.correct", "right.correct")]
  
  
  # add screening data
  sd_data <- lapply(screening_data, function(screeni) tibble("workerid" = as.character(NA), 
                                                             "cue" = screeni[1], 
                                                             "left.source" = NA,  
                                                             "right.source" = NA, 
                                                             "left.word" = screeni[2], 
                                                             "right.word" = screeni[3], 
                                                             "screener" = TRUE,
                                                             "left.rank" = NA,
                                                             "right.rank" = NA,
                                                             "left.correct" = screeni[4],
                                                             "right.correct" = screeni[5])) %>% do.call(rbind, .)
  
  tt_data <- rbind(tt_data, sd_data)
  # shuffle order
  tt_data <- tt_data %>% sample_n(nrow(tt_data))
  # create page names
  tt_data <- tt_data %>% mutate(variable = c(paste0(rep("lexical", nrow(tt_data)), seq(1,nrow(tt_data),1))))
  # add nextInputID (defines next page)
  tt_data <-  tt_data %>% mutate(nextInputID = c(tt_data$variable[2:nrow(tt_data)], "survey"))
  num_tt <- nrow(tt_data)
  
  # --------------------------
  # SECTION A6: trial words      -----
  # --------------------------
  tt_trial <- lapply(trial_data, function(triali) tibble("cue" = triali[1], "left.word" = triali[2], "right.word" = triali[3], "left.correct" = triali[4], "right.correct" = triali[5])) %>% do.call(rbind, .)
  # shuffle order
  tt_trial <- tt_trial %>% sample_n(nrow(tt_trial))
  # create page names
  tt_trial <- tt_trial %>% mutate(variable = c(paste0(rep("trial", nrow(tt_trial)), seq(1,nrow(tt_trial),1))))
  # add nextInputID (defines next page)
  if(length(trial_data) == 1){tt_trial$nextInputID <- "instructions3"}else{tt_trial$nextInputID <- c(tt_trial$variable[2:nrow(tt_trial)], "trial")}
  num_trials <- nrow(tt_trial)
  total_tasks <- num_trials + num_tt
  
  # --------------------------------
  #   SECTION C1: Define Reactive Values ----
  #   These store the main values in the game
  # --------------------------------
  
  # CurrentValues stores scalars representing the latest game outcomes and values
  
  CurrentValues <- reactiveValues(page = "welcome",
                                  errors = "none",
                                  trial_index = 1,
                                  index = 1)

  LexicalData <- reactiveValues()
  
  # --------------------------------
  # SECTION C2: Page Layouts         ----
  # --------------------------------
  
  # Send dynamic UI to ui - DON'T CHANGE!
  output$MainAction <- renderUI( {
    PageLayouts()
  })
  
  PageLayouts <- reactive({
    # --------------------------------
    # (1) WELCOME PAGE                ---
    # --------------------------------
    if (CurrentValues$page == "welcome") {   # conditionl determining whether page is displayed
      
      # conditional: if ID not entered, ouput inputLabel text in red to remind user that he must enter an ID
      if (CurrentValues$errors == "Blank_Name") {
        inputLabel <- p(style = "color:Red", "Please enter your MTurk ID!")   
      } else {
        inputLabel <- p("Please enter your MTurk ID")
      }
      
      # page content
      return(
        list(
          br(),
          h1(span(strong("Context Words"), style="color:#2780e3")),   # title
          br(),
            mainPanel(    
          p(span(strong("Purpose:"), style="color:#2780e3"),"evaluate context words."),
          p(span(strong("Confidentiality:"), style="color:#2780e3"), "responses are anonymous, we have no way of linking the data to individual identities."),
          p(span(strong("Length:"), style="color:#2780e3"), "task takes on average less than 5 minutes to complete."),
          p(span(strong("Compensation:"), style="color:#2780e3"), paste0("$", compensation)),
          br(),
          p("If you consent to participate in this study, please enter your MTurk ID and press ''Start''.")),
          # main panel contents
          mainPanel(
            # text input control
          textInput(inputId = "workerid",   # control ID
                    label = inputLabel,     # label to appear on top of control (color conditioned above)
                    placeholder = "enter MTurk ID here"), # text to appear as placeholder inside control (an example of a unique ID)
          # action button to be pressed by user to continue
          actionButton(inputId = "consent",   # button ID
                       label = "Start",   # button label (text displayed to the user)
                       class = "btn btn-primary")   # css class (defines button color as specified in the css file)
          )
        )
      )}
    
    # --------------------------------
    # (4) INSTRUCTIONS - 1     ---
    # --------------------------------
    if (CurrentValues$page == "instructions1") {   # conditionl determining whether page is displayed
      
      # content
      return(
        list(
          br(),
          span(h2(strong("Context Words")), style="color:#2780e3"),   # page title (smaller h2)
          p("A famous maxim in the study of linguistics states that:"),
          p(strong(em("You shall know a word by the company it keeps.")), "(Firth, 1957)"),
          p("This task is designed to help us understand the nature of the ''company'' that words ''keep'': that is, their CONTEXT."),
          br(),
          p("Specifically, for a CUE WORD, its CONTEXT WORDS include words that:"),
          column(12,
                 wellPanel(
                   tags$ul(
                     tags$li("Tend to occur in the vicinity of the CUE WORD. That is, they are words that appear close to the CUE WORD in written or spoken language.")),
                   p("AND/OR", align = "center"),
                   tags$ul(
                     tags$li("Tend to occur in similar situations to the CUE WORD in spoken and written language. That is, they are words that regularly appear with other words that are closely related to the CUE WORD.")))),
          br(),
          p("For example, CONTEXT WORDS for the cue word COFFEE include:"),
          tags$ol(
            tags$li(em("cup"), "(tends to occur in the vicinity of COFFEE)."), 
            tags$li(em("tea"), "(tends to occur in similar situations to COFFEE, for example when discussing drinks).")
          ),
          br(),
          p("Click ''Next'' to continue"),
          # action button to be pressed by user to continue
          actionButton(inputId = "goto.instructions2",   # button ID 
                       label = "Next",   # button label (text displayed to the user) 
                       class = "btn btn-primary"),   # css class (defines button color as specified in the css file)
          br()
        )
      )}
    
    # --------------------------------
    # (4) INSTRUCTIONS - 1     ---
    # --------------------------------
    if (CurrentValues$page == "instructions2") {   # conditionl determining whether page is displayed
      
      # content
      return(
        list(
          br(),
          span(h2(strong("Task Description")), style="color:#2780e3"),   # page title (smaller h2)
          br(),
          p("For each iteration of the task (", total_tasks, " in total including trial and screener tasks):"),
          br(),
          tags$ol(
            tags$li("You will be given a cue word (top center of the screen) and two candidate context words (on either side of the cue word)."),
            br(),
            tags$li("Please select the candidate context word that you find best meets the definition of a context word."),
            br(),
            tags$li("We are especially interested in context words likely to appear in", strong("political discourse.")),
            br(),
            tags$li("If both are reasonable context words, please select whichever you find most intuitive."),
            br(),
            tags$li("You must select", strong("one and only one"), "of the two candidate context words.")),
            br(),
          p("Keep in mind, some iterations are for screening purposes. These are tasks for which there is clearly a correct answer."),
          br(),
          p("Wrong answers in these screening tasks will automatically end your participation so", strong("be sure to read carefully.")),
          br(),
          p("The trial task that follows is meant for you to practice. Like screening tasks, the trial task has a correct answer."),
          br(),
          p("Click ''Next'' to continue to the trial runs"),
          # action button to be pressed by user to continue
          actionButton(inputId = "goto.trial1",   # button ID 
                       label = "Next",   # button label (text displayed to the user) 
                       class = "btn btn-primary"),   # css class (defines button color as specified in the css file)
          br()
        )
      )}
    
    # --------------------------------
    # (5) TEST ROUNDS - TT TASK ---
    # --------------------------------

    # --------------------------------
    # (4) trial - 2     ---
    # --------------------------------
    
    if (CurrentValues$page %in% tt_trial$variable){   # conditionl determining whether page is displayed
      CurrentValues$trial_index <- which(tt_trial$variable == CurrentValues$page)
      return(
        TriadTask(title = paste0("Trial ", CurrentValues$trial_index, " of ", num_trials), variable = tt_trial$variable[CurrentValues$trial_index], 
                            cue = tt_trial$cue[CurrentValues$trial_index], context = c(tt_trial$left.word[CurrentValues$trial_index], tt_trial$right.word[CurrentValues$trial_index]), 
                            nextInputID = tt_trial$nextInputID[CurrentValues$trial_index], CurrentValues = CurrentValues)
      )}
    
    # --------------------------------
    # (7) INSTRUCTIONS - 4     ---
    # --------------------------------
    if (CurrentValues$page == "instructions3") {   # conditionl determining whether page is displayed
      
      # content
      return(
        list(
          br(),
          span(h2(strong("Ready to Start?")), style="color:#2780e3"),   # page title (smaller h2)
          br(),
          p("Thank you for completing the trial runs, if you feel ready to begin the real tasks, click ''Continue''. The first cue will immediately appear on the screen."),
          br(),
          p("If you want to return to the instructions click on ''Back to instructions''."), 
          br(),
          # action button to be pressed by user to continue
          
          actionButton(inputId = "goto.lexical1",   # button ID 
                       label = "Continue",   # button label (text displayed to the user) 
                       class = "btn btn-primary"),   # css class (defines button color as specified in the css file)
          HTML("<br><br>"),
          actionButton(inputId = "goto.instructions2",   # button ID 
                       label = "Back to instructions",   # button label (text displayed to the user) 
                       class = "btn btn-primary"))   # css class (defines button color as specified in the css file)
      )}
    
    
    # --------------------------------
    # (4) TT-1    ---
    # --------------------------------
    
    if (CurrentValues$page %in% tt_data$variable){   # conditionl determining whether page is displayed
    CurrentValues$index <- which(tt_data$variable == CurrentValues$page)
      return(
        TriadTask(title = paste0("Task ", CurrentValues$index, " of ", num_tt), variable = tt_data$variable[CurrentValues$index], 
                            cue = tt_data$cue[CurrentValues$index], context = c(tt_data$left.word[CurrentValues$index], tt_data$right.word[CurrentValues$index]), 
                            nextInputID = tt_data$nextInputID[CurrentValues$index], CurrentValues = CurrentValues)
      )}
    
    # --------------------------------
    # (4) SAVE DATA    ---
    # --------------------------------
    
    if (CurrentValues$page == "savedata") {   # conditionl determining whether page is displayed
      
      # content
      return(
        list(
          br(),
          span(h2(strong("Save Data")), style="color:#2780e3"),   # page title (smaller h2)
          br(),
          p("You have completed all the required tasks. To save your data and get your HIT completion code press ''Save my data''."),
          br(),
          p("If for some reason you do not want to have your data saved, simply close this window (NOTE: you will not receive compensation)."), 
          br(),
          # action button to be pressed by user to continue
          
          actionButton(inputId = "goto.goodbye",   # button ID 
                       label = "Save my data",   # button label (text displayed to the user) 
                       class = "btn btn-primary"))   # css class (defines button color as specified in the css file)
      )}
    
    # --------------------------------
    # (8) SURVEY        ---
    # --------------------------------
    
    if (CurrentValues$page == "survey") {
      
      # Throw an error if not all question have been answered.
      if (CurrentValues$errors == "answerQuestions") {
        answerQuestions <- p(style = "color:Red", "Please answer all required questions!")
      } else {
        answerQuestions <- ""
      }
      
      return(list(
        br(),
        span(h2(strong("Survey (1 of 1)")), style="color:#2780e3"),
        
        br(),
        
        p("To conclude please fill out this short survey."),
        
        br(),
        
        radioButtons("party", 
                     label = "Generally speaking, do you usually think of yourself as a Democrat, a Republican, an Independent, or what?",
                     choices = c("Strong Democrat" =  1,
                                 "Weak Democrat" = 2,
                                 "Independent Democrat" = 3,
                                 "Independent Independent" = 4,
                                 "Independent Republican" = 5,
                                 "Weak Republican" = 6,
                                 "Strong Republican" = 7,
                                 "Other party" = 8,
                                 "No preference" = 9
                     ), selected = 99, width = "100%"),
        
        br(),
        
        radioButtons("ideology", 
                     label = "Generally speaking, do you usually think of yourself as a Liberal, a Conservative, a Moderate, or what?",
                     choices = c("Extremely liberal" =  1,
                                 "Liberal" = 2,
                                 "Slightly liberal" = 3,
                                 "Moderate; middle of the road" = 4,
                                 "Slightly conservative" = 5,
                                 "Conservative" = 6,
                                 "Extremely conservative" = 7,
                                 "Haven't thought much about this" = 8
                     ), selected = 99, width = "100%"),
        
        br(),
        
        radioButtons("sex",
                     label = "What is your sex?",
                     choices = list("Male" = 1, "Female" = 2, "Other" = 3),
                     selected = 99, width = "100%"),
        
        br(),
        
        radioButtons("interesting", 
                     label = "How engaging did you find the HIT?",
                     choices = c("1 - Not at all engaging" =  1,
                                 "2" = 2,
                                 "3" = 3,
                                 "4" = 4,
                                 "5 - Very engaging" = 5
                     ), selected = 99, width = "100%"),
        
        br(),
        
        radioButtons("fair", 
                     label = paste0("How fair would you say $", compensation, " is as compensation for this HIT?"),
                     choices = c("1 - Very unfair" =  1,
                                 "2" = 2,
                                 "3" = 3,
                                 "4" = 4,
                                 "5 - More than fair" = 5
                     ), selected = 99, width = "100%"),
        
        br(),
        
        textAreaInput("comments",
                      label = "If you have any additional comments (e.g. that can help us improve the task), please enter them below.",
                      resize = "both"),
        
        br(),
        
        p(answerQuestions),
        
        actionButton(inputId = "savedata",
                     label = "Next", 
                     class = "btn btn-primary"),
        
        HTML("<br><br><br>"))
      )}

    # --------------------------------
    # (18) GOODBYE    ---
    # --------------------------------
    if (CurrentValues$page == "goodbye") {
      
      # CALCULATE COMPLETION CODE  
      completion.code <- paste0("TT-", sample(100:999, size = 1), "-", sample(100:999, size = 1), "-", sample(100:999, size = 1))
      
      return(list(
        br(),
        h3("Thank you for your participation!"),
        br(),
        p("Here is your randomly generated study completion code. Please enter this code to submit your HIT."),
        h3(completion.code),
        br(),
        h3("What was this survey about?"),
        br(),
        p("This survey is part of an academic study exploring context words."),
        br(),
        p("You may proceed to close this window.")
      ))
    }
    
    # --------------------------------
    # (18) BOOTED   ---
    # --------------------------------
    if (CurrentValues$page == "booted1") {
      
      return(list(
        br(),
        h3("SORRY!"),
        br(),
        p("You failed a trial task. You can either:"),
        br(),
        tags$ul(
          tags$li("Restart the HIT by refreshing your browser (you will be taken back to the consent form)."),
          br(),
          p("OR"),
          br(),
          tags$li("Close this window and not complete the HIT (NOTE: you will not receive compensation).")),
        br()
        #br(),
        #p("You may proceed to close this window.")
      ))
    }
    
    if (CurrentValues$page == "booted2") {
      
      return(list(
        br(),
        h3("SORRY!"),
        br(),
        p("You failed a screener task. You can either:"),
        br(),
        tags$ul(
          tags$li("Restart the HIT by refreshing your browser (you will be taken back to the consent form)."),
          br(),
          p("OR"),
          br(),
          tags$li("Close this window and not complete the HIT (NOTE: you will not receive compensation).")),
        br()
        #br(),
        #p("You may proceed to close this window.")
      ))
    }
    
    
  })
  
  # --------------------------------
  # SECTION C3: PAGE NAVIGATION        ----
  # --------------------------------
  
  # consent page
  observeEvent(input$consent, {
    if (input$workerid == ""){
      CurrentValues$errors <- "Blank_Name"
    } else {
      CurrentValues$page <- "instructions1"
    }
  })
  
  # TTs trial
  observeEvent(input[[tt_trial$nextInputID[CurrentValues$trial_index]]], {
    if (input[[paste0(tt_trial$variable[CurrentValues$trial_index],".left")]] == input[[paste0(tt_trial$variable[CurrentValues$trial_index],".right")]]) {
      CurrentValues$errors <- paste0(tt_trial$variable[CurrentValues$trial_index], ".error")
    } else {
      if(tt_trial$left.correct[CurrentValues$trial_index] != input[[paste0(tt_trial$variable[CurrentValues$trial_index],".left")]]){
        CurrentValues$page <- "booted1"
      }else{
      
      CurrentValues$page <- tt_trial$nextInputID[CurrentValues$trial_index]
      }
    }
  })
  
  # TTs
  observeEvent(input[[tt_data$nextInputID[CurrentValues$index]]], {
    if (input[[paste0(tt_data$variable[CurrentValues$index],".left")]] == input[[paste0(tt_data$variable[CurrentValues$index],".right")]]) {
      CurrentValues$errors <- paste0(tt_data$variable[CurrentValues$index], ".error")
    } else {
      if(tt_data$screener[CurrentValues$index] & (tt_data$left.correct[CurrentValues$index] != input[[paste0(tt_data$variable[CurrentValues$index],".left")]])){
        CurrentValues$page <- "booted2"
      }else{
      
      LexicalData[[tt_data$variable[CurrentValues$index]]] <- data.table(input[[paste0(tt_data$variable[CurrentValues$index],".left")]],
                                                                          input[[paste0(tt_data$variable[CurrentValues$index],".right")]])
      
      CurrentValues$page <- tt_data$nextInputID[CurrentValues$index]
      }
    }
  })
  
  # survey
  observeEvent(input$savedata, {
    # check wether all questions have been answered:
    if (any(input$age == 0, is.null(input$sex), is.null(input$party), is.null(input$ideology), is.null(input$fair), is.null(input$interesting))) {
      CurrentValues$errors <- "answerQuestions"
    } else {
      CurrentValues$page <- "savedata"
    }})
  
  # other
  observeEvent(input$goto.instructions2, CurrentValues$page <- "instructions2")
  observeEvent(input$goto.trial1, CurrentValues$page <- "trial1")
  observeEvent(input$goto.lexical1, CurrentValues$page <- "lexical1")

  # --------------------------------
  # SECTION C5: SAVE DATA        ----
  # --------------------------------
  observeEvent(input[["goto.goodbye"]], {
   
    # Create progress message
    withProgress(message = "Saving data...",
                 value = 0, {
                   
                   incProgress(.25)
                   
                   # Write associations data
                   LexicalData.i <- lapply(tt_data$variable, function(x) LexicalData[[x]])
                   LexicalData.i <- do.call(rbind, LexicalData.i) %>% set_colnames(., c("left.choice", "right.choice"))
                   tt_data$workerid <- input$workerid
                   LexicalData.i <- cbind(tt_data, LexicalData.i)
                   LexicalData.i <- LexicalData.i[LexicalData.i$screener == FALSE,] # keep only the non-screeners
                   LexicalData.i <- LexicalData.i[, c("workerid", "cue", "left.source", "right.source", "left.word", "right.word", "left.rank", "right.rank", "left.choice", "right.choice")]
                   
                   
                   
                   # Write survey data to a datatable
                   SurveyData.i <- data.table("workerid" = input$workerid,
                                              "sex" = input$sex,
                                              "party" = input$party,
                                              "ideology" = input$ideology,
                                              "fair" = input$fair,
                                              "interesting" = input$interesting,
                                              "comments" = input$comments
                                              #"interesting" = input$interesting
                   )
                   
                   incProgress(.5)
                   
                   LexicalDatafileName <- paste0(input$workerid, as.integer(Sys.time()), digest::digest(LexicalData.i), "_tt.csv")
                   SurveyDatafileName <- paste0(input$workerid, as.integer(Sys.time()), digest::digest(SurveyData.i), "_survey.csv")
                   
                   # Create The Filepath and save the data depending on the method chosen:
                   
                   LexicalDatafilePath <- file.path(tempdir(), LexicalDatafileName)
                   write.csv(LexicalData.i, LexicalDatafilePath, row.names = TRUE, quote = TRUE)
                   rdrop2::drop_upload(LexicalDatafilePath, path = outputDir, dtoken = droptoken)
                   
                   SurveyDatafilePath <- file.path(tempdir(), SurveyDatafileName)
                   write.csv(SurveyData.i, SurveyDatafilePath, row.names = FALSE, quote = TRUE)
                   rdrop2::drop_upload(SurveyDatafilePath, path = outputDir, dtoken = droptoken)
                   
                   # report progress (of data saving) to the user
                   incProgress(.40)
                   
                   # go to goodbye page
                   CurrentValues$page <- "goodbye"
                   Sys.sleep(.25)
                   incProgress(1)
                 })
  })
  
  # --------------------------------
  # SECTION C6: WHAT TO DO WHEN A KEY IS PRESSED ----
  # http://www.javascripter.net/faq/keycodes.htm
  # --------------------------------
  # upon observing a key event
  observeEvent(input$lastkeypresscode, {
    # isolate keyboard event
    n <- input$lastkeypresscode[1]
    # if we are on the welcome page
    if (CurrentValues$page == "welcome") {
      if (n == 13) {   # if the enter key is pressed
        CurrentValues$page <- "instructions1"   # go to the literature page
      }
    }
  })
  
}

# Create app!
shinyApp(ui = ui, server = server)
