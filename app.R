library(shiny)
library(shinyjs)
library(bslib)
library(writexl)
library(tibble)
library(redcapAPI)

theme <- bs_add_rules(bs_theme(preset = "journal", primary = "#2FA4E7", font_scale = 1.5), sass::sass_file("www/style.scss"))

ui <- page_fluid(
  theme = theme,
  useShinyjs(),
  title = "SPRouT-D",
  uiOutput("MainAction")
)

server <- function(input, output, session) {
  
  #bs_themer()
  
  CurrentValues <- reactiveValues(page = "welcome",
                                  block = 1,
                                  practblock = 1,
                                  lastClick = Sys.time(),
                                  starttime = Sys.time(),
                                  thisresp = NA,
                                  thistime = NA,
                                  thiscorr = NA,
                                  thistype = NA
  )
  SubData <- reactiveValues(subject = c(),
                            block = c(),          
                            time = c(),
                            resp = c(),
                            corr = c(),
                            totalscore = NA,
                            comp1time = NA,
                            comp2time = NA,
                            comp3time = NA)
  output$MainAction <- renderUI( {
    PageLayouts()
  })
  PageLayouts <- reactive({
    
    # WELCOME PAGE
    if (CurrentValues$page == "welcome") {
      
      return(
        list(
          h1("Screening Pediatric Patients for Reading difficulties Test: Draft (SPRouT-D)"),
          h3("Developed by Eric Q. Tridas, Yaacov Petscher, Christopher Stanley, Jospeh Sanfilippo, and Nadine Gaab"),
          p("Disclaimer: This is a newly created checklist for use in a pediatric medical practice. The authors are currently in the process of further developing and refining the scoring criteria. We strongly encourage the clinical and scientific communities to provide feedback in order to help us with the further development of the checklist and its validation."),
          
          
          column(12, actionButton(inputId = "gt_begin",
                                  label = "Start!"), align = "center"),
          br(),
          br(),
          column(12, img(src='images/ped.png', height = "50%", width = "50%"), align = "center")
        )
      )}
    
    if (CurrentValues$page == "begin") {
      
      return(
        list(
          h1("SPRouT-D"),
          p("Please enter the patient ID, Clinician ID, and select the appropriate form"),
          textInput("pid", "Patient ID"),
          textInput("cid", "Clinician ID"),
          selectInput("form", "Form", choices = c("Form A: 4 Years Old: No Formal Schooling", 
                                                  "Form B: Mid-Late Pre-Kindergarten to Early Kindergarten", 
                                                  "Form C: Mid-Late Kindergarten to Early 1st Grade", 
                                                  "Form D: Mid-Late 1st Grade to Entering 2nd Grade", 
                                                  "Form E: Mid-Late 2nd Grade")),
          
          actionButton(inputId = "gt_form",
                       label = "Start!")
        )
      )}
    
    if(CurrentValues$page == "formA_hist"){
      return(
        list(
          h1("History"),
          actionLink("test", label = "(Play Audio)", icon = icon("play", lib = "font-awesome"), style = "display:none;"),
          radioButtons("formA_hist_1", HTML("Has anyone in your family had problems with reading, or learning to read or spell?", as.character(actionLink("formA_hist_1_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formA_hist_2", HTML("Has anyone in your family been diagnosed with dyslexia or a reading disability?", as.character(actionLink("formA_hist_2_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formA_hist_3a", HTML("At 2 years old, did your child speak less than 50 words in any language?", as.character(actionLink("formA_hist_3a_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formA_hist_3b", HTML('At 2 years old, was your child not able to speak using two-word phrases like "go home" or "want cookie"?', as.character(actionLink("formA_hist_3b_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formA_hist_4", HTML("Has your child been diagnosed with a language or articulation or speech delay?", as.character(actionLink("formA_hist_4_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formA_hist_5", HTML("Does your child speak another language at home?", as.character(actionLink("formA_hist_5_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formA_hist_6", HTML("Do you or others worry about how your child talks or uses language overall?", as.character(actionLink("formA_hist_6_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formA_hist_7", HTML("Does your child have problems describing events, telling stories, or explaining what's happening?", as.character(actionLink("formA_hist_7_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formA_hist_8", HTML("Does your child have trouble following directions, often ask you to repeat or explain things in a simpler way, or have problems answering questions correctly?", as.character(actionLink("formA_hist_8_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formA_letnam",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formA_letnam"){
      return(
        list(
          h1("Letter naming"),
          p("Tell me the names of these letters", style = "display: inline;"),
          actionLink("formB_letrec_title_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"), style = "display: inline;"),
          br(),br(),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,
              card_body(
              card_image(file = "www/images/formA_letnam_1_img.png", alt = "C"),
              max_height = "100%", fill = FALSE)
              #add a max width: max-content;
            ),
            card(
              full_screen = TRUE,
              card_body(
                card_image(file = "www/images/formA_letnam_2_img.png", alt = "D"),
                max_height = "100%", fill = FALSE)
              
              
            ),
            card(
              full_screen = TRUE,
              card_body(
                card_image(file = "www/images/formA_letnam_3_img.png", alt = "A"),
                           max_height = "100%", fill = FALSE)
              
            ),
            card(
              full_screen = TRUE,
              card_body(
                card_image(file = "www/images/formA_letnam_4_img.png", alt = "B"),
                           max_height = "100%", fill = FALSE)
              
            )
          ),
          layout_column_wrap(
            width = 1/6,
            height = "100%",
            radioButtons("formA_letnam_1", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
            radioButtons("formA_letnam_2", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
            radioButtons("formA_letnam_3", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
            radioButtons("formA_letnam_4", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
            
          ),
          
          # radioButtons("formA_letnam_1", "C", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formA_letnam_2", "D", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formA_letnam_3", "A", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formA_letnam_4", "B", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formA_rhyme",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formA_rhyme"){
      return(
        list(
          h1("Rhyming"),
          p("I am going to say 3 words and I want you to tell me which 2 sound most alike or rhyme. For example, if I say 'tab', 'big', and 'dig', 'big' and 'dig' are two words that sound alike or rhyme. Do you understand? (If not, repeat example).", style = "display: inline;"),
          actionLink("formA_rhyme_title1_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"), style = "display: inline;"),
          br(),
          br(),
          p("You try. What two words sound most alike or rhyme:", style = "display: inline;"),
          actionLink("formA_rhyme_title2_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"), style = "display: inline;"),
          br(), br(),
          radioButtons("formA_rhyme_1", HTML("cat, rat, log", as.character(actionLink("formA_rhyme_1_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formA_rhyme_2", HTML("pot, men, rot", as.character(actionLink("formA_rhyme_2_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formA_sylldel",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formA_sylldel"){
      return(
        list(
          h1("Syllable Deletion"),
          p("I am going to tell you a word and want you to take part of the word away to make a new word. For example, I say the word FIRETRUCK. Now I say FIRETRUCK but I don't say FIRE. What is left is the word TRUCK. Do you understand? (If not, repeat example.)", style = "display: inline;"),
          actionLink("formA_sylldel_title_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"), style = "display: inline;"),
          br(),br(),
          radioButtons("formA_sylldel_1", HTML("Say the word ICECREAM.", as.character(actionLink("formA_sylldel_1_1_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"))), " Now say it but don't say ICE. What word is left? (cream)", as.character(actionLink("formA_sylldel_1_2_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formA_sylldel_2", HTML("Say the word BASEBALL.", as.character(actionLink("formA_sylldel_2_1_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"))), " Now say it but don't say BASE. What word is left? (ball)", as.character(actionLink("formA_sylldel_2_2_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formA_clin",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formA_clin"){
      return(
        list(
          h1("Clinical Assessment"),
          radioButtons("formA_clin_1", "Hearing screening concern?", choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formA_clin_2", "Past/current clinical concern?", choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formA_submit",
                       label = "Submit!")
        )
      )
    }
    
    if(CurrentValues$page == "formB_hist"){
      return(
        list(
          h1("History"),
          actionLink("test", label = "(Play Audio)", icon = icon("play", lib = "font-awesome"), style = "display:none;"),
          radioButtons("formB_hist_1", HTML("Has anyone in your family had problems with reading, or learning to read or spell?", as.character(actionLink("formA_hist_1_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formB_hist_2", HTML("Has anyone in your family been diagnosed with dyslexia or a reading disability?", as.character(actionLink("formA_hist_2_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formB_hist_3a", HTML("At 2 years old, did your child speak less than 50 words in any language?", as.character(actionLink("formA_hist_3a_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formB_hist_3b", HTML('At 2 years old, was your child not able to speak using two-word phrases like "go home" or "want cookie"?', as.character(actionLink("formA_hist_3b_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formB_hist_4", HTML("Has your child been diagnosed with a language or articulation or speech delay?", as.character(actionLink("formA_hist_4_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formB_hist_5", HTML("Does your child speak another language at home?", as.character(actionLink("formA_hist_5_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formB_hist_6", HTML('Does your child have trouble telling if words rhyme like "run" and "fun"?', as.character(actionLink("formB_hist_6_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formB_hist_7", HTML("Is your child unable to name more than one letter?", as.character(actionLink("formB_hist_7_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formB_hist_8", HTML("Do you or others worry about how your child is learning the sounds letters make?", as.character(actionLink("formB_hist_8_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formB_hist_9", HTML("Do you or others worry about how your child talks or uses language overall?", as.character(actionLink("formA_hist_6_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formB_hist_10", HTML("Does your child have problems describing events, telling stories, or explaining what's happening?", as.character(actionLink("formA_hist_7_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formB_hist_11", HTML("Does your child have trouble following directions, often ask you to repeat or explain things in a simpler way, or have problems answering questions correctly?", as.character(actionLink("formA_hist_8_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formB_letrec",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formB_letrec"){
      return(
        list(
          h1("Letter Recognition"),
          p("Tell me the names of these letters (point to each letter):", style = "display: inline;"),
          actionLink("formB_letrec_title_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"), style = "display: inline;"),
          br(),
          p("(If a child gives letter sound, ask for letter name - need name for credit)."),
          br(),br(),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,
              card_body(
                card_image(file = "www/images/formA_letnam_3_img.png", alt = "A"),
                max_height = "100%", fill = FALSE)
              
              
            ),
            card(
              full_screen = TRUE,
              card_body(
                card_image(file = "www/images/formB_letrec_2_img.png", alt = "F"),
                max_height = "100%", fill = FALSE)
              
             
            ),
            card(
              full_screen = TRUE,
              card_body(
                card_image(file = "www/images/formB_letrec_3_img.png", alt = "M"),
                max_height = "100%", fill = FALSE)
              
              
            ),
            card(
              full_screen = TRUE,
              card_body(
                card_image(file = "www/images/formB_letrec_4_img.png", alt = "P"),
                max_height = "100%", fill = FALSE)
              
              
            )
          ),
          layout_column_wrap(
            width = 1/6,
            height = "100%",
            radioButtons("formB_letrec_1", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
            radioButtons("formB_letrec_2", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
            radioButtons("formB_letrec_3", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
            radioButtons("formB_letrec_4", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
          ),
          # radioButtons("formB_letrec_1", "A", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formB_letrec_2", "F", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formB_letrec_3", "M", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formB_letrec_4", "P", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formB_rhyme",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formB_rhyme"){
      return(
        list(
          h1("Rhyming"),
          p("I am going to say 3 words and I want you to tell me which 2 sound most alike or rhyme. For example, if I say 'tab', 'big', and 'dig', 'big' and 'dig' are two words that sound alike or rhyme. Do you understand? (If not, repeat example).", style = "display: inline;"),
          actionLink("formA_rhyme_title1_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"), style = "display: inline;"),
          br(),
          br(),
          p("You try. What two words sound most alike or rhyme:", style = "display: inline;"),
          actionLink("formA_rhyme_title2_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"), style = "display: inline;"),
          br(), br(),
          radioButtons("formB_rhyme_1", HTML("cat, rat, log", as.character(actionLink("formA_rhyme_1_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formB_rhyme_2", HTML("pot, men, rot", as.character(actionLink("formA_rhyme_2_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formB_sylldel",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formB_sylldel"){
      return(
        list(
          h1("Syllable Deletion"),
          p("I am going to tell you a word and want you to take part of the word away to make a new word. For example, I say the word FIRETRUCK. Now I say FIRETRUCK but I don't say FIRE. What is left is the word TRUCK. Do you understand? (If not, repeat example.)", style = "display: inline;"),
          actionLink("formA_sylldel_title_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"), style = "display: inline;"),
          br(),br(),
          radioButtons("formB_sylldel_1", HTML("Say the word RAINBOW.", as.character(actionLink("formB_sylldel_1_1_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"))), " Now say RAINBOW but don't say RAIN. What word is left? (bow)", as.character(actionLink("formB_sylldel_1_2_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formB_sylldel_2", HTML("Say the word CARTOON.", as.character(actionLink("formB_sylldel_2_1_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"))), " Now say CARTOON but don't say TOON. What word is left? (car)", as.character(actionLink("formB_sylldel_2_2_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formB_msd",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formB_msd"){
      return(
        list(
          h1("Multistep Directions"),
          p("(Do not repeat any items))"),
          radioButtons("formB_msd_1", HTML("Point to your nose then tap the top of your head", as.character(actionLink("formB_msd_1_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formB_msd_2", HTML("Touch your ear then point to your mouth", as.character(actionLink("formB_msd_2_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formB_clin",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formB_clin"){
      return(
        list(
          h1("Clinical Assessment"),
          radioButtons("formB_clin_1", "Hearing screening concern?", choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formB_clin_2", "Past/current clinical concern?", choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formB_submit",
                       label = "Submit!")
        )
      )
    }
    
    if(CurrentValues$page == "formC_hist"){
      return(
        list(
          h1("History"),
          actionLink("test", label = "(Play Audio)", icon = icon("play", lib = "font-awesome"), style = "display:none;"),
          radioButtons("formC_hist_1", HTML("Has anyone in your family had problems with reading, or learning to read or spell?", as.character(actionLink("formA_hist_1_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formC_hist_2", HTML("Has anyone in your family been diagnosed with dyslexia or a reading disability?", as.character(actionLink("formA_hist_2_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formC_hist_3a", HTML("At 2 years old, did your child speak less than 50 words in any language?", as.character(actionLink("formA_hist_3a_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formC_hist_3b", HTML('At 2 years old, was your child not able to speak using two-word phrases like "go home" or "want cookie"?', as.character(actionLink("formA_hist_3b_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formC_hist_4", HTML("Has your child been diagnosed with a language or articulation or speech delay?", as.character(actionLink("formA_hist_4_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formC_hist_5", HTML("Does your child speak another language at home?", as.character(actionLink("formA_hist_5_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formC_hist_6a", HTML('Does your child have trouble telling if words rhyme like "run" and "fun"?', as.character(actionLink("formB_hist_6_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formC_hist_6b", HTML("Does your child have trouble naming most of the letters in the English alphabet and the sounds they make?", as.character(actionLink("formC_hist_6b_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formC_hist_6c", HTML('Does your child have problems sounding out simple words while reading like “mom”, “cat”, or “hot”?', as.character(actionLink("formC_hist_6c_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formC_hist_6d", HTML('Does your child have problems spelling simple words like “hot”, “dad”, or “cub”?', as.character(actionLink("formC_hist_6d_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formC_hist_7", HTML("Do you or others worry about how your child talks or uses language overall?", as.character(actionLink("formA_hist_6_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formC_hist_8", HTML("Does your child have problems describing events, telling stories, or explaining what's happening?", as.character(actionLink("formA_hist_7_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formC_hist_9", HTML("Does your child have trouble following directions, often ask you to repeat or explain things in a simpler way, or have problems answering questions correctly?", as.character(actionLink("formA_hist_8_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formC_letsoundnam",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formC_letsoundnam"){
      return(
        list(
          h1("Letter/Sound Naming"),
          p("Tell me the names and the sounds of these letters:", style = "display: inline;"),
          actionLink("formC_letsoundnam_title_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"), style = "display: inline;"),
          br(),br(),
          p("(Child must say both name and sound for item to be correct)"),
          br(),br(),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,
              
              card_body(
                card_image(file = "www/images/formC_letsoundnam_1_img.png", alt = "S"),
                fill = FALSE, max_height = "100%"
                
              )
            ),
            card(
              full_screen = TRUE,
              
              card_body(
                card_image(file = "www/images/formA_letnam_3_img.png", alt = "A"),
                fill = FALSE, max_height = "100%"
                
                
              )
            ),
            card(
              full_screen = TRUE,
              
              card_body(
                card_image(file = "www/images/formC_letsoundnam_3_img.png", alt = "T"),
                fill = FALSE, max_height = "100%"
                
                
              )
            ),
            card(
              full_screen = TRUE,
              
              card_body(
                card_image(file = "www/images/formB_letrec_4_img.png", alt = "P"),
                fill = FALSE, max_height = "100%"
                
              )
            )
          ),
          layout_column_wrap(
            width = 1/6,
            height = "100%",
            radioButtons("formC_letsoundnam_1", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
            radioButtons("formC_letsoundnam_2", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
            radioButtons("formC_letsoundnam_3", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
            radioButtons("formC_letsoundnam_4", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
            
          ),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,

              card_body(
                card_image(file = "www/images/formC_letsoundnam_5_img.png", alt = "O"),
                fill = FALSE, max_height = "100%"

              )
            ),
            card(
              full_screen = TRUE,

              card_body(
                card_image(file = "www/images/formB_letrec_3_img.png", alt = "M"),
                fill = FALSE, max_height = "100%"

              )
            ),
            card(
              full_screen = TRUE,

              card_body(
                card_image(file = "www/images/formC_letsoundnam_7_img.png", alt = "G"),
                fill = FALSE, max_height = "100%"

              )
            )
          ),
          layout_column_wrap(
            width = 1/6,
            height = "100%",

            radioButtons("formC_letsoundnam_5", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
            radioButtons("formC_letsoundnam_6", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
            radioButtons("formC_letsoundnam_7", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
          ),
          # radioButtons("formC_letsoundnam_1", "S", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formC_letsoundnam_2", "A", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formC_letsoundnam_3", "T", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formC_letsoundnam_4", "P", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formC_letsoundnam_5", "O", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formC_letsoundnam_6", "M", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formC_letsoundnam_7", "G", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formC_sounds",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formC_sounds"){
      return(
        list(
          h1("Word Parts: Sounds"),
          p("I will say 3 words and I want you to tell me which 2 words start with the same sound. For example, if I say 'cat', 'luck', and 'coat', 'cat' and 'coat' start with the same sound. Do you understand? Now, I want you to tell me which 2 words start with the same sound:", style = "display: inline;"),
          actionLink("formC_sounds_title_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"), style = "display: inline;"),
          br(),br(),
          p("(Ok to repeat directions and items for Word Parts subtests)"),
          radioButtons("formC_sounds_1", HTML("bed, mouse, book", as.character(actionLink("formC_sounds_1_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formC_sounds_2", HTML("cape, pain, pen", as.character(actionLink("formC_sounds_2_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formC_phondel",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formC_phondel"){
      return(
        list(
          h1("Word Parts: Phoneme Deletion"),
          p("I am going to tell you a word and I want you to take part of the word away to make a new word. For example, if I say the word MOTEL and I take M out I am left with the word OTEL. Do you understand? Now you try."),
          actionLink("formC_phondel_title_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"), style = "display: inline;"),
          br(),br(),
          p("(Ok to repeat directions and items for Word Parts subtests)"),
          radioButtons("formC_phondel_1", HTML("Say the word feet.", as.character(actionLink("formC_phondel_1_1_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"))), " Now say feet but don't say /f/. (eat)", as.character(actionLink("formC_phondel_1_2_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formC_phondel_2", HTML("Say the word like.", as.character(actionLink("formC_phondel_2_1_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"))), " Now say like but don't say /k/. (lie)", as.character(actionLink("formC_phondel_2_2_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formC_phonseg",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formC_phonseg"){
      return(
        list(
          h1("Word Parts: Phoneme Segmentation"),
          p("I am going to say a word. Tell me each sound in the word. For example, if I say 'dog', you would say /d/ /o/ /g/. Do you understand? Now you try. Now say each sound in: "),
          actionLink("formC_phonseg_title_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"), style = "display: inline;"),
          br(),br(),
          p("(Ok to repeat directions and items for Word Parts subtests)"),
          radioButtons("formC_phonseg_1", HTML("to (/t/ /oo/)", as.character(actionLink("formC_phonseg_1_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formC_phonseg_2", HTML("pot (/p/ /o/ /t/)", as.character(actionLink("formC_phonseg_2_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formC_phonseg_3", HTML("fish (/f/ /i/ /sh/)", as.character(actionLink("formC_phonseg_3_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formC_nonword",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formC_nonword"){
      return(
        list(
          h1("Word Parts: Non-word Reading"),
          p("I want you to read these made-up words. How would they sound?"),
          actionLink("formC_nonword_title_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"), style = "display: inline;"),
          br(),br(),
          p("(Ok to repeat directions and items for Word Parts subtests)"),
          br(),br(),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,
              card_body(
              card_image(file = "www/images/formC_nonword_1_img.png", alt = "tep"),
              fill = FALSE, max_height = "100%"
              )
              
            ),
            radioButtons("formC_nonword_1", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
            
            ),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,
              card_body(
              card_image(file = "www/images/formC_nonword_2_img.png", alt = "lat"),
              fill = FALSE, max_height = "100%"
              )
              
            ),
            radioButtons("formC_nonword_2", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
            
            ),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,
              card_body(
              card_image(file = "www/images/formC_nonword_3_img.png", alt = "nog"),
              fill = FALSE, max_height = "100%"
              )
              
            ),
            radioButtons("formC_nonword_3", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
            
          ),
          # radioButtons("formC_nonword_1", "tep", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formC_nonword_2", "lat", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formC_nonword_3", "nog", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formC_msd",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formC_msd"){
      return(
        list(
          h1("Multistep Directions"),
          p("(Do not repeat any items)"),
          p("I want you to follow some instructions but wait until I have finished saying them before you start."),
          actionLink("formC_msd_title_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"), style = "display: inline;"),
          br(),br(),
          radioButtons("formC_msd_1", HTML("Point to your nose then touch the top of your head twice.",  as.character(actionLink("formC_msd_1_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formC_msd_2", HTML("Touch your ear two times, then point to your mouth.",  as.character(actionLink("formC_msd_2_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formC_clin",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formC_clin"){
      return(
        list(
          h1("Clinical Assessment"),
          radioButtons("formC_clin_1", "Hearing screening concern?", choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formC_clin_2", "Past/current clinician concern?", choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formC_submit",
                       label = "Submit!")
        )
      )
    }
    
    if(CurrentValues$page == "formD_hist"){
      return(
        list(
          h1("History"),
          actionLink("test", label = "(Play Audio)", icon = icon("play", lib = "font-awesome"), style = "display:none;"),
          radioButtons("formD_hist_1", HTML("Has anyone in your family had problems with reading, or learning to read or spell?", as.character(actionLink("formA_hist_1_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formD_hist_2", HTML("Has anyone in your family been diagnosed with dyslexia or a reading disability?", as.character(actionLink("formA_hist_2_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formD_hist_3a", HTML("At 2 years old, did your child speak less than 50 words in any language?", as.character(actionLink("formA_hist_3a_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formD_hist_3b", HTML('At 2 years old, was your child not able to speak using two-word phrases like "go home" or "want cookie"?', as.character(actionLink("formA_hist_3b_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formD_hist_4", HTML("Has your child been diagnosed with a language or articulation or speech delay?", as.character(actionLink("formA_hist_4_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formD_hist_5", HTML("Does your child speak another language at home?", as.character(actionLink("formA_hist_5_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formD_hist_6", HTML("Do you or others worry about the way your child reads or spells words and sentences?", as.character(actionLink("formD_hist_6_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formD_hist_7", HTML("Do you or others worry about how your child talks or uses language overall?", as.character(actionLink("formA_hist_6_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formD_hist_8", HTML("Does your child have problems describing events, telling stories, or explaining what's happening?", as.character(actionLink("formA_hist_7_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formD_hist_9", HTML("Does your child have trouble following directions, often ask you to repeat or explain things in a simpler way, or have problems answering questions correctly?", as.character(actionLink("formA_hist_8_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formD_readingwords",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formD_readingwords"){
      return(
        list(
          h1("Reading Single Words"),
          p("I want you to read these words, talk loud and clear."),
          actionLink("formD_readingwords_title_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"), style = "display: inline;"),
          br(),br(),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,
              card_body(
              card_image(file = "www/images/formD_readingwords_1_img.png", alt = "fine"),
              fill = FALSE, max_height = "100%"
              )
              
            ),
            radioButtons("formD_readingwords_1", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
            
            ),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,
              card_body(
              card_image(file = "www/images/formD_readingwords_2_img.png", alt = "turn"),
              fill = FALSE, max_height = "100%"
              )
              
            ),
            radioButtons("formD_readingwords_2", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
            
            ),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,
              card_body(
              card_image(file = "www/images/formD_readingwords_3_img.png", alt = "stove"),
              fill = FALSE, max_height = "100%"
              )
              
            ),
            radioButtons("formD_readingwords_3", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
            
            ),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,
              card_body(
              card_image(file = "www/images/formD_readingwords_4_img.png", alt = "bait"),
              fill = FALSE, max_height = "100%"
              )
              
            ),
            radioButtons("formD_readingwords_4", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
            
          ),
          # radioButtons("formD_readingwords_1", "fine", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formD_readingwords_2", "turn", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formD_readingwords_3", "stove", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formD_readingwords_4", "bait", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formD_readingnonwords",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formD_readingnonwords"){
      return(
        list(
          h1("Reading Nonsense Words"),
          p("I want you to read these made-up words, talk loud and clear."),
          actionLink("formD_readingnonwords_title_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"), style = "display: inline;"),
          br(),br(),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,
              card_body(
              card_image(file = "www/images/formD_readingnonwords_1_img.png", alt = "tope"),
              fill = FALSE, max_height = "100%"
              )
              
            ),
            radioButtons("formD_readingnonwords_1", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
            
            ),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,
              card_body(
              card_image(file = "www/images/formD_readingnonwords_2_img.png", alt = "glish"),
              fill = FALSE, max_height = "100%"
              )
              
            ),
            radioButtons("formD_readingnonwords_2", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
            
            ),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,
              card_body(
              card_image(file = "www/images/formD_readingnonwords_3_img.png", alt = "creb"),
              fill = FALSE, max_height = "100%"
              )
              
            ),
            radioButtons("formD_readingnonwords_3", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
            
            ),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,
              card_body(
              card_image(file = "www/images/formD_readingnonwords_4_img.png", alt = "sprad"),
              fill = FALSE, max_height = "100%"
              )
              
            ),
            radioButtons("formD_readingnonwords_4", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
            
          ),
          # radioButtons("formD_readingnonwords_1", "tope (3) /t//oe//p/", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formD_readingnonwords_2", "glish (4) /g//l//i//sh/", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formD_readingnonwords_3", "creb (4) /c//r//e//b/", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formD_readingnonwords_4", "sprad (5) /s//p//r//a//d/", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formD_msd",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formD_msd"){
      return(
        list(
          h1("Multistep Directions"),
          p("(Do not repeat any items)"),
          p("I want you to follow some instructions but wait until I have finished saying them before you start."),
          actionLink("formD_msd_title_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"), style = "display: inline;"),
          br(),br(),
          radioButtons("formD_msd_1", HTML("Point to your nose then touch the top of your head twice.", as.character(actionLink("formD_msd_1_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formD_msd_2", HTML("Touch your ear two times, then point to your mouth.", as.character(actionLink("formD_msd_2_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formD_clin",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formD_clin"){
      return(
        list(
          h1("Clinical Assessment"),
          radioButtons("formD_clin_1", "Hearing screening concern?", choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formD_clin_2", "Past/current clinician concern?", choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formD_submit",
                       label = "Submit!")
        )
      )
    }
    
    
    if(CurrentValues$page == "formE_hist"){
      return(
        list(
          h1("History"),
          actionLink("test", label = "(Play Audio)", icon = icon("play", lib = "font-awesome"), style = "display:none;"),
          radioButtons("formE_hist_1", HTML("Has anyone in your family had problems with reading, or learning to read or spell?", as.character(actionLink("formA_hist_1_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formE_hist_2", HTML("Has anyone in your family been diagnosed with dyslexia or a reading disability?", as.character(actionLink("formA_hist_2_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formE_hist_3a", HTML("At 2 years old, did your child speak less than 50 words in any language?", as.character(actionLink("formA_hist_3a_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formE_hist_3b", HTML('At 2 years old, was your child not able to speak using two-word phrases like "go home" or "want cookie"?', as.character(actionLink("formA_hist_3b_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formE_hist_4", HTML("Has your child been diagnosed with a language or articulation or speech delay?", as.character(actionLink("formA_hist_4_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formE_hist_5", HTML("Do you or others worry about the way your child reads or spells words and sentences?", as.character(actionLink("formD_hist_6_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formE_hist_6", HTML("Do you or others worry about how your child talks or uses language overall?", as.character(actionLink("formA_hist_6_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formE_hist_7", HTML("Does your child have problems describing events, telling stories, or explaining what's happening?", as.character(actionLink("formA_hist_7_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formE_hist_8", HTML("Does your child have trouble following directions, often ask you to repeat or explain things in a simpler way, or have problems answering questions correctly?", as.character(actionLink("formA_hist_8_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formE_readingwords",
                       label = "Next!")
        )
      )
    }
    if(CurrentValues$page=="formE_readingwords"){
      return(
        list(
          h1("Reading Single Words"),
          p("I want you to read these words, talk loud and clear."),
          actionLink("formD_readingwords_title_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"), style = "display: inline;"),
          br(),br(),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,
              
              card_body(
                card_image(file = "www/images/formE_readingwords_1_img.png", alt = "tunnel"),
                fill = FALSE,  max_height = "100%"
                
              )
            ),
            radioButtons("formE_readingwords_1", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
            ),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,
              
              card_body(
                card_image(file = "www/images/formE_readingwords_2_img.png", alt = "athlete"),
                fill = FALSE,  max_height = "100%"
                
              )
            ),
            radioButtons("formE_readingwords_2", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
            ),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,
              
              card_body(
                card_image(file = "www/images/formE_readingwords_3_img.png", alt = "pending"),
                fill = FALSE,  max_height = "100%"
              )
            ),
            radioButtons("formE_readingwords_3", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
            
            ),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,
              
              card_body(
                card_image(file = "www/images/formE_readingwords_4_img.png", alt = "thankful"),
                fill = FALSE,  max_height = "100%"
              )
            ),
            radioButtons("formE_readingwords_4", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
            
          ),
          # radioButtons("formE_readingwords_1", "tunnel", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formE_readingwords_2", "athlete", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formE_readingwords_3", "pending", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formE_readingwords_4", "thankful", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formE_readingnonwords",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formE_readingnonwords"){
      return(
        list(
          h1("Reading Nonsense Words"),
          p("I want you to read these made-up words, talk loud and clear."),
          actionLink("formD_readingnonwords_title_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"), style = "display: inline;"),
          br(),br(),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,
              card_body(
              card_image(file = "www/images/formE_readingnonwords_1_img.png", alt = "shobe"),
              fill = FALSE,  max_height = "100%"
              )
              
            ),
            radioButtons("formE_readingnonwords_1", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
            ),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,
              card_body(
              card_image(file = "www/images/formE_readingnonwords_2_img.png", alt = "twaze"),
              fill = FALSE,  max_height = "100%"
              )
              
            ),
            radioButtons("formE_readingnonwords_2", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
            ),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,
              card_body(
              card_image(file = "www/images/formE_readingnonwords_3_img.png", alt = "croast"),
              fill = FALSE,  max_height = "100%"
              )
              
            ),
            radioButtons("formE_readingnonwords_3", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
            ),
          layout_column_wrap(
            width = 1/6,
            height = 200,
            card(
              full_screen = TRUE,
              card_body(
              card_image(file = "www/images/formE_readingnonwords_4_img.png", alt = "flepping"),
              fill = FALSE,  max_height = "100%"
              )
              
            ),
            radioButtons("formE_readingnonwords_4", label = NULL, choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0))
          ),
          # radioButtons("formE_readingnonwords_1", "shobe (3) /sh//oe//b/", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formE_readingnonwords_2", "twaze (4) /t//w//ae//z/", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formE_readingnonwords_3", "croast (5) /c//r//oe//s//t/", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          # radioButtons("formE_readingnonwords_4", "flepping (6) /f//l//e//p//i//ng/", choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formE_msd",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formE_msd"){
      return(
        list(
          h1("Multistep Directions"),
          p("(Do not repeat any items)"),
          p("I want you to follow some instructions but wait until I have finished saying them before you start."),
          actionLink("formD_msd_title_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome"), style = "display: inline;"),
          br(),br(),
          radioButtons("formE_msd_1", HTML("Before you point to your nose tap the top of your head 3 times.", as.character(actionLink("formE_msd_1_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formE_msd_2", HTML("After you touch your ear two times, stick out your tongue and then point to your mouth.", as.character(actionLink("formE_msd_2_audio", label = NULL, icon = icon("volume-high", lib = "font-awesome")))), choiceNames = c("Correct", "Incorrect"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formE_clin",
                       label = "Next!")
        )
      )
    }
    
    if(CurrentValues$page=="formE_clin"){
      return(
        list(
          h1("Clinical Assessment"),
          radioButtons("formE_clin_1", "Hearing screening concern?", choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          radioButtons("formE_clin_2", "Past/current clinician concern?", choiceNames = c("yes", "no"), choiceValues = c(1,0), inline = TRUE, selected = character(0)),
          actionButton(inputId = "gt_formE_submit",
                       label = "Submit!")
        )
      )
    }
    
    if(CurrentValues$page=="thank_submission"){
      return(
        list(
          h1("Complete"),
          p("Thank you for your submission. If you would like to restart the application and go to the homepage please press reset the button below."),
          actionButton(inputId = "reset",
                       label = "Reset!")
        )
      )
    }
    
    
  })
  observeEvent(input$gt_begin, {
    CurrentValues$page = "begin"
  })
  observeEvent(input$gt_form, {
    if(input$form=="Form A: 4 Years Old: No Formal Schooling"){
      CurrentValues$page = "formA_hist"
    }
    if(input$form=="Form B: Mid-Late Pre-Kindergarten to Early Kindergarten"){
      CurrentValues$page = "formB_hist"
    }
    if(input$form=="Form C: Mid-Late Kindergarten to Early 1st Grade"){
      CurrentValues$page = "formC_hist"
    }
    if(input$form=="Form D: Mid-Late 1st Grade to Entering 2nd Grade"){
      CurrentValues$page = "formD_hist"
    }
    if(input$form=="Form E: Mid-Late 2nd Grade"){
      CurrentValues$page = "formE_hist"
    }
  })
  
  
  
  observeEvent(input$reset, {
    session$reload()
  })
  
  observeEvent(input$gt_formA_letnam, {
    if (all(!sapply(list(input$formA_hist_1, input$formA_hist_2, input$formA_hist_3a, input$formA_hist_3b, input$formA_hist_4, input$formA_hist_5, input$formA_hist_6, input$formA_hist_7, input$formA_hist_8), is.null)))
    {
    CurrentValues$page = "formA_letnam"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  observeEvent(input$gt_formA_rhyme, {
    if (all(!sapply(list(input$formA_letnam_1, input$formA_letnam_2, input$formA_letnam_3, input$formA_letnam_4), is.null)))
    {
    CurrentValues$page = "formA_rhyme"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  observeEvent(input$gt_formA_sylldel, {
    if (all(!sapply(list(input$formA_rhyme_1, input$formA_rhyme_2), is.null)))
    {
    CurrentValues$page = "formA_sylldel"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  observeEvent(input$gt_formA_clin, {
    if (all(!sapply(list(input$formA_sylldel_1, input$formA_sylldel_2), is.null)))
    {
    CurrentValues$page = "formA_clin"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
  }
  else
  {
    showNotification(
      "Please provide an answer for each question.",
      type = "error",
      duration = 4
    )
  }
  })
  
  observeEvent(input$gt_formB_letrec, {
    if (all(!sapply(list(input$formB_hist_1, input$formB_hist_2, input$formB_hist_3a, input$formB_hist_3b, input$formB_hist_4, input$formB_hist_5, input$formB_hist_6, input$formB_hist_7, input$formB_hist_8, input$formB_hist_9, input$formB_hist_10, input$formB_hist_11), is.null)))
    {
    CurrentValues$page = "formB_letrec"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  observeEvent(input$gt_formB_rhyme, {
    if (all(!sapply(list(input$formB_letrec_1, input$formB_letrec_2, input$formB_letrec_3, input$formB_letrec_4), is.null)))
    {
    CurrentValues$page = "formB_rhyme"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  observeEvent(input$gt_formB_sylldel, {
    if (all(!sapply(list(input$formB_rhyme_1, input$formB_rhyme_2), is.null)))
    {
    CurrentValues$page = "formB_sylldel"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  observeEvent(input$gt_formB_msd, {
    if (all(!sapply(list(input$formB_sylldel_1, input$formB_sylldel_2), is.null)))
    {
    CurrentValues$page = "formB_msd"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  observeEvent(input$gt_formB_clin, {
    if (all(!sapply(list(input$formB_msd_1, input$formB_msd_2), is.null)))
    {
    CurrentValues$page = "formB_clin"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  
  observeEvent(input$gt_formC_letsoundnam, {
    if (all(!sapply(list(input$formC_hist_1, input$formC_hist_2, input$formC_hist_3a, input$formC_hist_3b, input$formC_hist_4, input$formC_hist_5, input$formC_hist_6a, input$formC_hist_6b, input$formC_hist_6c, input$formC_hist_6d, input$formC_hist_7, input$formC_hist_8, input$formC_hist_9), is.null)))
    {
    CurrentValues$page = "formC_letsoundnam"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  observeEvent(input$gt_formC_sounds, {
    if (all(!sapply(list(input$formC_letsoundnam_1, input$formC_letsoundnam_2, input$formC_letsoundnam_3, input$formC_letsoundnam_4, input$formC_letsoundnam_5, input$formC_letsoundnam_6, input$formC_letsoundnam_7), is.null)))
    {
    CurrentValues$page = "formC_sounds"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  observeEvent(input$gt_formC_phondel, {
    if (all(!sapply(list(input$formC_sounds_1, input$formC_sounds_2), is.null)))
    {
    CurrentValues$page = "formC_phondel"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  observeEvent(input$gt_formC_phonseg, {
    if (all(!sapply(list(input$formC_phondel_1, input$formC_phondel_2), is.null)))
    {
    CurrentValues$page = "formC_phonseg"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  observeEvent(input$gt_formC_nonword, {
    if (all(!sapply(list(input$formC_phonseg_1, input$formC_phonseg_2, input$formC_phonseg_3), is.null)))
    {
    CurrentValues$page = "formC_nonword"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  observeEvent(input$gt_formC_msd, {
    if (all(!sapply(list(input$formC_nonword_1, input$formC_nonword_2, input$formC_nonword_3), is.null)))
    {
    CurrentValues$page = "formC_msd"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  observeEvent(input$gt_formC_clin, {
    if (all(!sapply(list(input$formC_msd_1, input$formC_msd_2), is.null)))
    {
    CurrentValues$page = "formC_clin"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  
  observeEvent(input$gt_formD_readingwords, {
    if (all(!sapply(list(input$formD_hist_1, input$formD_hist_2, input$formD_hist_3a, input$formD_hist_3b, input$formD_hist_4, input$formD_hist_5, input$formD_hist_6, input$formD_hist_7, input$formD_hist_8, input$formD_hist_9), is.null)))
    {
    CurrentValues$page = "formD_readingwords"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  observeEvent(input$gt_formD_readingnonwords, {
    if (all(!sapply(list(input$formD_readingwords_1, input$formD_readingwords_2, input$formD_readingwords_3, input$formD_readingwords_4), is.null)))
    {
    CurrentValues$page = "formD_readingnonwords"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  observeEvent(input$gt_formD_msd, {
    if (all(!sapply(list(input$formD_readingnonwords_1, input$formD_readingnonwords_2, input$formD_readingnonwords_3, input$formD_readingnonwords_4), is.null)))
    {
    CurrentValues$page = "formD_msd"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  observeEvent(input$gt_formD_clin, {
    if (all(!sapply(list(input$formD_msd_1, input$formD_msd_2), is.null)))
    {
    CurrentValues$page = "formD_clin"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  
  observeEvent(input$gt_formE_readingwords, {
    if (all(!sapply(list(input$formE_hist_1, input$formE_hist_2, input$formE_hist_3a, input$formE_hist_3b, input$formE_hist_4, input$formE_hist_5, input$formE_hist_6, input$formE_hist_7, input$formE_hist_8), is.null)))
    {
    CurrentValues$page = "formE_readingwords"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  observeEvent(input$gt_formE_readingnonwords, {
    if (all(!sapply(list(input$formE_readingwords_1, input$formE_readingwords_2, input$formE_readingwords_3, input$formE_readingwords_4), is.null)))
    {
    CurrentValues$page = "formE_readingnonwords"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  observeEvent(input$gt_formE_msd, {
    if (all(!sapply(list(input$formE_readingnonwords_1, input$formE_readingnonwords_2, input$formE_readingnonwords_3, input$formE_readingnonwords_4), is.null)))
    {
    CurrentValues$page = "formE_msd"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  observeEvent(input$gt_formE_clin, {
    if (all(!sapply(list(input$formE_msd_1, input$formE_msd_2), is.null)))
    {
    CurrentValues$page = "formE_clin"
    removeUI(
      selector = "div audio",
      multiple = TRUE
    )
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  
  #audo observeEvents
  addAudio <- function(name) {
    insertUI(
      selector = paste0("#", name, "_audio"),
      where = "afterEnd",
      ui = tags$audio(
        src = paste0("audio/", name, ".mp3"),
        
        type = "audio/mp3",
        autoplay = TRUE,
        controls = NA,
        style = "display:none;"
      ),
      immediate = TRUE
    )
  }
  
  
  
  observeEvent(input$formA_hist_1_audio, {
    addAudio("formA_hist_1")
  })
  observeEvent(input$formA_hist_2_audio, {
    addAudio("formA_hist_2")
  })
  observeEvent(input$formA_hist_3a_audio, {
    addAudio("formA_hist_3a")
  })
  observeEvent(input$formA_hist_3b_audio, {
    addAudio("formA_hist_3b")
  })
  observeEvent(input$formA_hist_4_audio, {
    addAudio("formA_hist_4")
  })
  observeEvent(input$formA_hist_5_audio, {
    addAudio("formA_hist_5")
  })
  observeEvent(input$formA_hist_6_audio, {
    addAudio("formA_hist_6")
  })
  observeEvent(input$formA_hist_7_audio, {
    addAudio("formA_hist_7")
  })
  observeEvent(input$formA_hist_8_audio, {
    addAudio("formA_hist_8")
  })
  observeEvent(input$formA_rhyme_title1_audio, {
    addAudio("formA_rhyme_title1")
  })
  observeEvent(input$formA_rhyme_title2_audio, {
    addAudio("formA_rhyme_title2")
  })
  observeEvent(input$formA_rhyme_1_audio, {
    addAudio("formA_rhyme_1")
  })
  observeEvent(input$formA_rhyme_2_audio, {
    addAudio("formA_rhyme_2")
  })
  observeEvent(input$formA_sylldel_title_audio, {
    addAudio("formA_sylldel_title")
  })
  observeEvent(input$formA_sylldel_1_1_audio, {
    addAudio("formA_sylldel_1_1")
  })
  observeEvent(input$formA_sylldel_1_2_audio, {
    addAudio("formA_sylldel_1_2")
  })
  observeEvent(input$formA_sylldel_2_1_audio, {
    addAudio("formA_sylldel_2_1")
  })
  observeEvent(input$formA_sylldel_2_2_audio, {
    addAudio("formA_sylldel_2_2")
  })
  observeEvent(input$formB_hist_6_audio, {
    addAudio("formB_hist_6")
  })
  observeEvent(input$formB_hist_7_audio, {
    addAudio("formB_hist_7")
  })
  observeEvent(input$formB_hist_8_audio, {
    addAudio("formB_hist_8")
  })
  observeEvent(input$formB_sylldel_1_1_audio, {
    addAudio("formB_sylldel_1_1")
  })
  observeEvent(input$formB_sylldel_1_2_audio, {
    addAudio("formB_sylldel_1_2")
  })
  observeEvent(input$formB_sylldel_2_1_audio, {
    addAudio("formB_sylldel_2_1")
  })
  observeEvent(input$formB_sylldel_2_2_audio, {
    addAudio("formB_sylldel_2_2")
  })
  observeEvent(input$formB_msd_1_audio, {
    addAudio("formB_msd_1")
  })
  observeEvent(input$formB_msd_2_audio, {
    addAudio("formB_msd_2")
  })
  observeEvent(input$formB_letrec_title_audio, {
    addAudio("formB_letrec_title")
  })
  observeEvent(input$formC_hist_6b_audio, {
    addAudio("formC_hist_6b")
  })
  observeEvent(input$formC_hist_6c_audio, {
    addAudio("formC_hist_6c")
  })
  observeEvent(input$formC_hist_6d_audio, {
    addAudio("formC_hist_6d")
  })
  observeEvent(input$formC_letsoundnam_title_audio, {
    addAudio("formC_letsoundnam_title")
  })
  observeEvent(input$formC_sounds_title_audio, {
    addAudio("formC_sounds_title")
  })
  observeEvent(input$formC_sounds_1_audio, {
    addAudio("formC_sounds_1")
  })
  observeEvent(input$formC_sounds_2_audio, {
    addAudio("formC_sounds_2")
  })
  observeEvent(input$formC_phondel_title_audio, {
    addAudio("formC_phondel_title")
  })
  observeEvent(input$formC_phondel_1_1_audio, {
    addAudio("formC_phondel_1_1")
  })
  observeEvent(input$formC_phondel_1_2_audio, {
    addAudio("formC_phondel_1_2")
  })
  observeEvent(input$formC_phondel_2_1_audio, {
    addAudio("formC_phondel_2_1")
  })
  observeEvent(input$formC_phondel_2_2_audio, {
    addAudio("formC_phondel_2_2")
  })
  observeEvent(input$formC_phonseg_title_audio, {
    addAudio("formC_phonseg_title")
  })
  observeEvent(input$formC_phonseg_1_audio, {
    addAudio("formC_phonseg_1")
  })
  observeEvent(input$formC_phonseg_2_audio, {
    addAudio("formC_phonseg_2")
  })
  observeEvent(input$formC_phonseg_3_audio, {
    addAudio("formC_phonseg_3")
  })
  observeEvent(input$formC_nonword_title_audio, {
    addAudio("formC_nonword_title")
  })
  observeEvent(input$formC_msd_title_audio, {
    addAudio("formC_msd_title")
  })
  observeEvent(input$formC_msd_1_audio, {
    addAudio("formC_msd_1")
  })
  observeEvent(input$formC_msd_2_audio, {
    addAudio("formC_msd_2")
  })
  observeEvent(input$formD_hist_6_audio, {
    addAudio("formD_hist_6")
  })
  observeEvent(input$formD_readingwords_title_audio, {
    addAudio("formD_readingwords_title")
  })
  observeEvent(input$formD_readingnonwords_title_audio, {
    addAudio("formD_readingnonwords_title")
  })
  observeEvent(input$formD_msd_title_audio, {
    addAudio("formD_msd_title")
  })
  observeEvent(input$formD_msd_1_audio, {
    addAudio("formD_msd_1")
  })
  observeEvent(input$formD_msd_2_audio, {
    addAudio("formD_msd_2")
  })
  observeEvent(input$formE_msd_1_audio, {
    addAudio("formE_msd_1")
  })
  observeEvent(input$formE_msd_2_audio, {
    addAudio("formE_msd_2")
  })
  
  
  
  # Function to submit data to REDCap
  submit_to_redcap <- function(df) {
    api_url <- "https://redcap.aws.rits.hms.harvard.edu/api/"
    api_token <- "901B0BBF709888123683301DA99E7522"
    
    # Open connection to REDCap API
    rcon <- redcapConnection(url = api_url, token = api_token)
    
    
    #Prepare data for import
    prepared_data <- castForImport(rcon,
                                   data = df)
    #Import Data
    importRecords(rcon,
                  data = prepared_data)
  }
  
  
  
 
  #---Submit Forms----
  observeEvent(input$gt_formA_submit, {
    if (all(!sapply(list(input$formA_clin_1, input$formA_clin_2), is.null)))
    {
    df <- data.frame("record_id" = interaction(input$pid, format(Sys.time(), "%s"), sep = "-"), "Patient ID" = input$pid, "Clinician ID" = input$cid, input$formA_hist_1, input$formA_hist_2, input$formA_hist_3a, input$formA_hist_3b, input$formA_hist_4, input$formA_hist_5, input$formA_hist_6, input$formA_hist_7, input$formA_hist_8, input$formA_letnam_1, input$formA_letnam_2, input$formA_letnam_3, input$formA_letnam_4,
                     input$formA_rhyme_1, input$formA_rhyme_2, input$formA_sylldel_1, input$formA_sylldel_2, input$formA_clin_1, input$formA_clin_2)
    df[, -c(1:3)] <- as.data.frame(lapply(df[, -c(1:3)], type.convert, as.is = TRUE))
    print(str(df[, -c(1:3)]))
    df2 <- data.frame("total_score" = rowSums(df[, -c(1:3)]), "form_complete" = 1)
    df3 <- cbind(df, df2)
    names(df3) = c("record_id", "forma_patient_id", "forma_clinician_id", "forma_hist_1", "forma_hist_2", "forma_hist_3a", "forma_hist_3b", "forma_hist_4", "forma_hist_5",  "forma_hist_6", "forma_hist_7", "forma_hist_8", "forma_letnam_1", "forma_letnam_2", "forma_letnam_3", "forma_letnam_4",
                   "forma_rhyme_1", "forma_rhyme_2", "forma_sylldel_1", "forma_sylldel_2", "forma_clin_1", "forma_clin_2", "forma_total_score", "form_a_complete")
    
    print(df3)
    submit_to_redcap(df3)
    CurrentValues$page = "thank_submission"
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
    
  })
  
  observeEvent(input$gt_formB_submit, {
    if (all(!sapply(list(input$formB_clin_1, input$formB_clin_2), is.null)))
    {
    df <- data.frame("record_id" = interaction(input$pid, format(Sys.time(), "%s"), sep = "-"), "Patient ID" = input$pid, "Clinician ID" = input$cid, input$formB_hist_1, input$formB_hist_2, input$formB_hist_3a, input$formB_hist_3b, input$formB_hist_4, input$formB_hist_5, input$formB_hist_6, input$formB_hist_7, input$formB_hist_8, input$formB_hist_9, input$formB_hist_10, input$formB_hist_11, input$formB_letrec_1, input$formB_letrec_2, input$formB_letrec_3, input$formB_letrec_4,
                     input$formB_rhyme_1, input$formB_rhyme_2, input$formB_sylldel_1, input$formB_sylldel_2, input$formB_msd_1, input$formB_msd_2, input$formB_clin_1, input$formB_clin_2)
    df[, -c(1:3)] <- as.data.frame(lapply(df[, -c(1:3)], type.convert, as.is = TRUE))
    print(str(df[, -c(1:3)]))
    df2 <- data.frame("total_score" = rowSums(df[, -c(1:3)]), "form_complete" = 1)
    df3 <- cbind(df, df2)
    names(df3) = c("record_id", "formb_patient_id", "formb_clinician_id", "formb_hist_1", "formb_hist_2", "formb_hist_3a", "formb_hist_3b", "formb_hist_4", "formb_hist_5", "formb_hist_6", "formb_hist_7", "formb_hist_8", "formb_hist_9", "formb_hist_10", "formb_hist_11", "formb_letrec_1", "formb_letrec_2", "formb_letrec_3", "formb_letrec_4",
                  "formb_rhyme_1", "formb_rhyme_2", "formb_sylldel_1", "formb_sylldel_2", "formb_msd_1", "formb_msd_2", "formb_clin_1", "formb_clin_2", "formb_total_score", "form_b_complete")
    print(df3)
    submit_to_redcap(df3)
    CurrentValues$page = "thank_submission"
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  
  observeEvent(input$gt_formC_submit, {
    if (all(!sapply(list(input$formC_clin_1, input$formC_clin_2), is.null)))
    {
    df <- data.frame("record_id" = interaction(input$pid, format(Sys.time(), "%s"), sep = "-"), "Patient ID" = input$pid, "Clinician ID" = input$cid, input$formC_hist_1, input$formC_hist_2, input$formC_hist_3a, input$formC_hist_3b, input$formC_hist_4, input$formC_hist_5, input$formC_hist_6a, input$formC_hist_6b, input$formC_hist_6c, input$formC_hist_6d, input$formC_hist_7, input$formC_hist_8, input$formC_hist_9,
                     input$formC_letsoundnam_1, input$formC_letsoundnam_2, input$formC_letsoundnam_3, input$formC_letsoundnam_4, input$formC_letsoundnam_5, input$formC_letsoundnam_6, input$formC_letsoundnam_7, input$formC_sounds_1, input$formC_sounds_2, input$formC_phondel_1, input$formC_phondel_2, input$formC_phonseg_1, input$formC_phonseg_2, input$formC_phonseg_3,
                     input$formC_nonword_1, input$formC_nonword_2, input$formC_nonword_3, input$formC_msd_1, input$formC_msd_2, input$formC_clin_1, input$formC_clin_2)
    df[, -c(1:3)] <- as.data.frame(lapply(df[, -c(1:3)], type.convert, as.is = TRUE))
    df2 <- data.frame("total_score" = rowSums(df[, -c(1:3)]), "form_complete" = 1)
    df3 <- cbind(df, df2)
    names(df3) = c("record_id", "formc_patient_id", "formc_clinician_id", "formc_hist_1", "formc_hist_2", "formc_hist_3a", "formc_hist_3b", "formc_hist_4", "formc_hist_5", "formc_hist_6a", "formc_hist_6b", "formc_hist_6c", "formc_hist_6d", "formc_hist_7", "formc_hist_8", "formc_hist_9",
                  "formc_letsoundnam_1", "formc_letsoundnam_2", "formc_letsoundnam_3", "formc_letsoundnam_4", "formc_letsoundnam_5", "formc_letsoundnam_6", "formc_letsoundnam_7", "formc_sounds_1", "formc_sounds_2", "formc_phondel_1", "formc_phondel_2", "formc_phonseg_1", "formc_phonseg_2", "formc_phonseg_3",
                  "formc_nonword_1", "formc_nonword_2", "formc_nonword_3", "formc_msd_1", "formc_msd_2", "formc_clin_1", "formc_clin_2", "formc_total_score", "form_c_complete")
    print(df3)
    submit_to_redcap(df3)
    CurrentValues$page = "thank_submission"
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  
  observeEvent(input$gt_formD_submit, {
    if (all(!sapply(list(input$formD_clin_1, input$formD_clin_2), is.null)))
    {
    df <- data.frame("record_id" = interaction(input$pid, format(Sys.time(), "%s"), sep = "-"), "Patient ID" = input$pid, "Clinician ID" = input$cid, input$formD_hist_1, input$formD_hist_2, input$formD_hist_3a, input$formD_hist_3b, input$formD_hist_4, input$formD_hist_5, input$formD_hist_6, input$formD_hist_7, input$formD_hist_8, input$formD_hist_9,
                     input$formD_readingwords_1, input$formD_readingwords_2, input$formD_readingwords_3, input$formD_readingwords_4, input$formD_readingnonwords_1, input$formD_readingnonwords_2, input$formD_readingnonwords_3, input$formD_readingnonwords_4,
                     input$formD_msd_1, input$formD_msd_2, input$formD_clin_1, input$formD_clin_2)
    df[, -c(1:3)] <- as.data.frame(lapply(df[, -c(1:3)], type.convert, as.is = TRUE))
    df2 <- data.frame("total_score" = rowSums(df[, -c(1:3)]), "form_complete" = 1)
    df3 <- cbind(df, df2)
    names(df3) = c("record_id", "formd_patient_id", "formd_clinician_id", "formd_hist_1", "formd_hist_2", "formd_hist_3a", "formd_hist_3b", "formd_hist_4", "formd_hist_5", "formd_hist_6", "formd_hist_7", "formd_hist_8", "formd_hist_9",
                  "formd_readingwords_1", "formd_readingwords_2", "formd_readingwords_3", "formd_readingwords_4", "formd_readingnonwords_1", "formd_readingnonwords_2", "formd_readingnonwords_3", "formd_readingnonwords_4",
                  "formd_msd_1", "formd_msd_2", "formd_clin_1", "formd_clin_2", "formd_total_score", "form_d_complete")
    print(df3)
    submit_to_redcap(df3)
    CurrentValues$page = "thank_submission"
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  
  observeEvent(input$gt_formE_submit, {
    if (all(!sapply(list(input$formE_clin_1, input$formE_clin_2), is.null)))
    {
    df <- data.frame("record_id" = interaction(input$pid, format(Sys.time(), "%s"), sep = "-"), "Patient ID" = input$pid, "Clinician ID" = input$cid, input$formE_hist_1, input$formE_hist_2, input$formE_hist_3a, input$formE_hist_3b, input$formE_hist_4, input$formE_hist_5, input$formE_hist_6, input$formE_hist_7, input$formE_hist_8,
                     input$formE_readingwords_1, input$formE_readingwords_2, input$formE_readingwords_3, input$formE_readingwords_4, input$formE_readingnonwords_1, input$formE_readingnonwords_2, input$formE_readingnonwords_3, input$formE_readingnonwords_4,
                     input$formE_msd_1, input$formE_msd_2, input$formE_clin_1, input$formE_clin_2)
    df[, -c(1:3)] <- as.data.frame(lapply(df[, -c(1:3)], type.convert, as.is = TRUE))
    df2 <- data.frame("total_score" = rowSums(df[, -c(1:3)]), "form_complete" = 1)
    df3 <- cbind(df, df2)
    names(df3) = c("record_id", "forme_patient_id", "forme_clinician_id", "forme_hist_1", "forme_hist_2", "forme_hist_3a", "forme_hist_3b", "forme_hist_4", "forme_hist_5", "forme_hist_6", "forme_hist_7", "forme_hist_8",
                  "forme_readingwords_1", "forme_readingwords_2", "forme_readingwords_3", "forme_readingwords_4", "forme_readingnonwords_1", "forme_readingnonwords_2", "forme_readingnonwords_3", "forme_readingnonwords_4",
                  "forme_msd_1", "forme_msd_2", "forme_clin_1", "forme_clin_2", "forme_total_score", "form_e_complete")
    print(df3)
    submit_to_redcap(df3)
    CurrentValues$page = "thank_submission"
    }
    else
    {
      showNotification(
        "Please provide an answer for each question.",
        type = "error",
        duration = 4
      )
    }
  })
  
}

shinyApp(ui, server)
