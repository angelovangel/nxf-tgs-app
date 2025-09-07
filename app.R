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
  shinyDirButton('fastq_pass', 'Select fastq_pass folder', title ='Please select a fastq_pass folder from a run', multiple = F),
  fileInput('upload', 'Upload sample sheet', multiple = F, accept = c('.xlsx', '.csv'), placeholder = 'xlsx or csv file'),
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
    height = 150,
    card_body(
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
  
}

shinyApp(ui, server)

