library(shiny)
library(shinyFiles)
library(shinyjs)
library(bslib)
library(bsicons)
library(stringr)
library(dplyr)
library(processx)
library(shinybusy)
library(digest)
library(hover)
library(reactable)

bin_on_path = function(bin) {
  exit_code = suppressWarnings(system2("command", args = c("-v", bin), stdout = FALSE))
  return(exit_code == 0)
}

sidebar <- sidebar(
  selectInput(
    'pipeline', 'Select workflow', 
    choices = c(
      'Plasmid assemly' = 'wf-clone-validation', 
      'Amplicon assembly' = 'wf-amplicon', 
      'Report only' = 'report-only'), 
    selected = 'wf-clone-validation'),
  # fastq_pass folder
  shinyDirButton('fastq_folder', 'Select fastq_pass folder', title ='Please select a fastq_pass folder from a run', multiple = F),
  fileInput('upload', 'Upload sample sheet', multiple = F, accept = c('.xlsx', '.csv'), placeholder = 'xlsx or csv file'),
  
  hover_action_button('start', 'Start pipeline', button_animation = 'overline-reveal', icon = icon('play')),
  hover_action_button('show_session', 'Show session', button_animation = 'overline-reveal', icon = icon('expand')),
  #hover_action_button('ctrlc', 'Send ctrl-c to session', button_animation = 'overline-reveal', icon = icon('stop')),
  hover_action_button('kill', 'Kill session', button_animation = 'overline-reveal', icon = icon('xmark')),
  
  checkboxInput('advanced', 'Advanced settings', value = FALSE),
  conditionalPanel(
    condition = 'input.advanced',
    selectInput('profile', 'Nextflow profile', choices = c('standard', 'singularity', 'test'), selected = 'standard', multiple = T),
    selectInput('nxf_ver', 'Use Nextflow version', choices = c('24.02.2', '25.04.6')),
    textInput('assembly_args', 'Assembly arguments')
  )
)

ui <- page_navbar(
  
  useShinyjs(),
  use_hover(),
  
  
  fillable = F,
  title = tags$span(
    tags$span(
      "NXF - TGS app",
      style = "font-size: 1.3rem; font-weight: normal;"
    ),
    tags$span(
      #icon('align-center'),
      "Nextflow pipeline for ONT plasmid/amplicon assembly at BCL", 
      #icon('align-center'),
      style = "font-size: 0.9rem; font-weight: normal; margin-left: 3em;"
    )
  ),
  theme = bs_theme(bootswatch = 'yeti', primary = '#196F3D'),
  sidebar = sidebar,
  
  card(
    card_header(
      id = 'header1', 
      class = 'bg-secondary', 
      tags$a('Sessions', tooltip(bsicons::bs_icon("question-circle"), 'Currently active tmux sessions'))
    ),
    max_height = '300px',
    card_body(
      reactableOutput('table')
    )
  ),
  
  card(
    card_header(
      id = 'header2', 
      class = 'bg-secondary', 
      tags$a('Output', tooltip(bsicons::bs_icon("question-circle"), 'Output from the selected nxf-tgs pipeline'))
    ),
    height = 450,
    card_body(
      verbatimTextOutput('stdout')
    )
  )
)

server <- function(input, output, session) {
  default_path <- Sys.getenv('DEFAULT_PATH')
  volumes <- c(ont_data = default_path, getVolumes()())
  
  shinyDirChoose(
    input, 
    "fastq_folder", 
    roots = volumes,
    session = session,
    restrictions = system.file(package = "base"))
  
  empty_df <- data.frame(
    session_id = NA,
    started = NA,
    runtime = NA
    #command = NA,
    #active = NA,
    #attached = NA
    #session_path = NA
  )
  
  # reactives
  tmux_sessions <- reactive({
    invalidateLater(2000, session)
    oldw <- getOption("warn")
    options(warn = -1)
    tmuxinfo <- system2("bin/helper.sh", stdout = TRUE, stderr = TRUE)
    options(warn = oldw)
    
    if (any(str_detect(tmuxinfo, 'no server|error'))) {
      empty_df
    } else {
      data.frame(
        session_id = str_split_i(tmuxinfo, " ", 2),
        started = str_split_i(tmuxinfo, " ", 1) %>% as.numeric() %>% as.POSIXct(),
        runtime = NA
        #command = str_split_i(tmuxinfo, " ", 5),
        #active = str_split_i(tmuxinfo, " ", 6),
        #attached = str_split_i(tmuxinfo, " ", 3)
        #session_path = str_split_i(tmuxinfo, " ", 4)
      ) %>%
        mutate(
          runtime = difftime(Sys.time(), started, units = 'hours')
          #attached = if_else(as.numeric(attached) == 1, 'yes', 'no')
        ) %>%
        arrange(started)
    }
  })
  
  samplesheet <- reactive({
    file <- input$upload
  })
  
  selected <- reactive({
    getReactableState('table', 'selected')
  })
  
  session_selected <- reactive({
    tmux_sessions()[selected(), ]$session_id
  })
  
  
  # outputs
  output$table <- renderReactable({
    reactable(
      empty_df,
      #tmux_sessions(), 
      pagination = FALSE, highlight = TRUE, height = 200, compact = T, 
      fullWidth = T, selection = 'single', onClick = 'select', defaultSelected = 1,
      theme = reactableTheme(
        rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #7B241C")
      ),
      style = list(fontSize = '90%'),
      columns = list(
        started = colDef(format = colFormat(datetime = T, locales = 'en-GB')),
        runtime = colDef(format = colFormat(suffix = ' h', digits = 2))
      )
    )
  })
  
  observe({
    updateReactable('table', data = tmux_sessions(), selected = selected())
  })
  
}

shinyApp(ui, server)

