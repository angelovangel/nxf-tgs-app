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
library(prettyunits)

source('bin/global.R')

sidebar <- sidebar(
  shiny::div(id = 'inputs',
  selectInput(
    'pipeline', 'Select workflow', 
    choices = c(
      'Plasmid assemly' = 'wf-clone-validation', 
      'Amplicon assembly' = 'wf-amplicon', 
      'Report only' = 'report-only'), 
    selected = 'wf-clone-validation'),
  # fastq_pass folder
  shinyDirButton('fastq_folder', 'Select fastq_pass folder', title ='Please select a fastq_pass folder from a run', multiple = F),
  fileInput('upload', 'Upload sample sheet', multiple = F, accept = c('.xlsx', '.csv'), placeholder = 'xlsx or csv file')
  ),
  
  hover_action_button('start', 'Start pipeline', button_animation = 'overline-reveal', icon = icon('play')),
  hover_action_button('show_session', 'Show session', button_animation = 'overline-reveal', icon = icon('expand')),
  hover_action_button('reset', 'Reset inputs', button_animation = 'overline-reveal', icon = icon('rotate-right')),
  #hover_action_button('ctrlc', 'Send ctrl-c to session', button_animation = 'overline-reveal', icon = icon('stop')),
  hover_action_button('kill', 'Kill session', button_animation = 'overline-reveal', icon = icon('xmark')),
  
  checkboxInput('advanced', 'Advanced settings', value = FALSE),
  conditionalPanel(
    condition = 'input.advanced',
    selectInput('profile', 'Nextflow profile', choices = c('standard', 'singularity', 'test'), selected = 'singularity', multiple = T),
    #selectInput('entry', 'Pipeline modules to execute', choices = c( 'Merge reads'='merge_reads', 'Merge reads + Report'='report', 'Full'='full'), selected = 'full'),
    selectInput('nxf_ver', 'Use Nextflow version', choices = c('24.04.2','25.04.6')),
    textInput('assembly_args', 'Assembly arguments')
  ),
  verbatimTextOutput('nxf_tgs_version')
)

ui <- page_navbar(
  
  useShinyjs(),
  use_hover(),
  
  
  fillable = F,
  title = tags$span(
    tags$span(
      "NXF - TGS app",
      style = "font-size: 1.3rem; font-weight: normal; margin-left: 0em; color: #701705;"
    ),
    tags$span(
      #icon('align-center'),
      "Nextflow pipeline for ONT merge/rename, report generation, and de novo plasmid/amplicon assembly at BCL",
      #icon('align-center'),
      style = "font-size: 0.9rem; font-weight: normal; margin-left: 7em; color: #701705;"
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
    max_height = 280,
    card_body(
      reactableOutput('table')
    )
  ),
  
  card(
    card_header(
      id = 'header2', 
      class = 'bg-secondary', 
      tags$a('Session output', tooltip(bsicons::bs_icon("question-circle"), 'Output from the selected nxf-tgs pipeline'))
    ),
    height = 450,
    card_body(
      verbatimTextOutput('stdout')
    )
  )
)

server <- function(input, output, session) {
  
  # start with start deactivated, activate only when minimum args selected
  shinyjs::disable('start')
  
  default_path <- Sys.getenv('DEFAULT_PATH')
  volumes <- c(ont_data = default_path, getVolumes()())
  get_nxftgs_ver <- paste0(
    "git ls-remote ", "https://github.com/angelovangel/nxf-tgs.git", " HEAD | awk '{print substr($1, 1, 8)}'"
  ) 
  
  # nxftgs_ver <- system(get_nxftgs_ver, intern = T)
  # output$nxf_tgs_version <- renderText({
  #   paste0("nxf-tgs commit: " , nxftgs_ver)
  #   })
  
  shinyDirChoose(
    input, 
    "fastq_folder", 
    roots = volumes,
    session = session,
    restrictions = system.file(package = "base"))
  
  empty_df <- data.frame(
    session_id = NA,
    pipeline = NA,
    started = NA,
    runtime = NA,
    pipeline_runtime = NA,
    status = NA,
    results = NA
    #command = NA,
    #active = NA,
    #attached = NA
    #session_path = NA
  )
  
  # reactives
  nxflog <- tempfile(fileext = ".csv")
  
  # write nxf log
  autoInvalidate <- reactiveTimer(3000) 
  observe({
    autoInvalidate()
    write_nxf_status(nxflog)
  })
  
  tmux_sessions <- reactive({
    invalidateLater(3000, session)
    oldw <- getOption("warn")
    options(warn = -1)
    tmuxinfo <- system2("bin/tmux-info.sh", stdout = TRUE, stderr = TRUE)
    nxf_info <- read.csv(nxflog, header = T)
    options(warn = oldw)
    
    if (any(str_detect(tmuxinfo, 'no server|error'))) {
      empty_df
    } else {
      df <- data.frame(
        session_id = str_split_i(tmuxinfo, " ", 2),
        pipeline = NA,
        started = str_split_i(tmuxinfo, " ", 1) %>% as.numeric() %>% as.POSIXct(),
        runtime = NA,
        pipeline_runtime = NA,
        status = NA,
        results = NA
      )
   
    # add status etc from nxflog
    df$status <- sapply(df$session_id, function(x) {
      status_vec <- nxf_info[str_detect(nxf_info$COMMAND, x), ]$STATUS
      if (length(status_vec) == 0) {
        "STARTING"
      } else {
        str_trim(status_vec)
      }
    })
    df$status <- as.character(df$status) # avoid a warning if status is not atomic
    df$pipeline_runtime = sapply(df$session_id, function(x) { nxf_info[str_detect(nxf_info$COMMAND, x), ]$DURATION })
    df$pipeline = sapply(df$session_id, function(x) {
      command <- nxf_info[str_detect(nxf_info$COMMAND, x), ]$COMMAND
      str_extract(command, "(?<=--pipeline\\s)\\S+") 
    }) 
    
    df <- df %>% mutate(
        status = ifelse(str_detect(status, "-"), "RUNNING", status),
        runtime = prettyunits::pretty_dt(difftime(Sys.time(), started), compact = T)
      ) %>%
      arrange(started)
    
    # Add direct download link if tarball exists
    df$results <- vapply(df$session_id, function(id) {
      tar_name <- paste0(id, ".tar.gz")
      tar_path <- file.path("www", tar_name)
      if (!is.na(id) && file.exists(tar_path)) {
        paste0('<a href="', tar_name, '" download>Download ', id, '</a>')
      } else {
        ""
      }
    }, character(1))
    df
    }
  })

  
  samplesheet <- reactive({
    file <- input$upload
  })
  
  row_selected <- reactive({
    getReactableState('table', 'selected')
  })
  
  session_selected <- reactive({
    tmux_sessions()[row_selected(), ]$session_id
  })
  
  # entry for the nextflow pipeline
  # entry <- reactive({
  #   if(input$entry == 'full') {
  #     ''
  #   } else {
  #     paste('-entry', input$entry, sep = ' Space ')
  #   }
  # })
  
  # outputs
  # show selection
  output$stdout <- renderText({
    #req(samplesheet())
    path <- parseDirPath(volumes, input$fastq_folder)
    #ext <- tools::file_ext(samplesheet()$datapath)
    #shiny::validate(need(ext == 'csv' | ext == 'xlsx', 'Please upload a csv or excel (xlsx) file'))
    paste0(
      "Selected parameters: \n",
      "-----------------------\n",
      "pipeline: ", input$pipeline, "\n",
      "fastq path: ", path, "\n",
      "samplesheet: ", samplesheet()$name, "\n",
      "profile: ", str_flatten(input$profile, collapse = ","), "\n"
    )
  })
  
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
        results = colDef(html = TRUE)
      )
    )
  })
  
  # tar whnen ready and place in www
  # decide if ready by looking at the nxflog (OK for finished)
  observe({
  df <- tmux_sessions()
  for (id in df$session_id) {
    if (!is.na(id) && pipeline_finished(df = df, id = id)) {
    #if ( !is.na(id) && (df[df$session_id == id, ]$status == 'OK') ) {
      tar_path <- file.path("www", paste0(id, ".tar.gz"))
      outdir <- file.path("output", id)
      if (!file.exists(tar_path) && dir.exists(outdir)) {
        # Create tarball in www folder
        system2("tar", args = c("-czf", tar_path, "-C", "output", id))
      }
    }
  }
})

  observe({
    updateReactable('table', data = tmux_sessions(), selected = row_selected())
  })
  
  observe({
    if(!is.integer(input$fastq_folder)) {
      shinyjs::enable('start')
    }
  })
  
  observeEvent(input$profile, {
    if(str_detect(string = str_flatten(input$profile, collapse = ","), pattern = 'test')) {
      shinyjs::hide('inputs')
      shinyjs::enable('start')
    } else {
      shinyjs::show('inputs')
    }
  })
  
  # reset inputs
  observeEvent(input$reset, {
    session$reload()
  })
  
  # main call
  observeEvent(input$start, {
    session_id <- digest(runif(1), algo = 'crc32')
    new_session_name <- session_id #paste0(session_id, "-", input$pipeline)
    selectedFolder <- parseDirPath(volumes, input$fastq_folder)
  
    # launch new session
    args1 <- c('new', '-d', '-s', new_session_name, '-x', '120', '-y', '30')
    system2('tmux', args = args1)
    
    # execute command in the new session - this is application-specific, everything else is common
    tmux_command <- paste(
      paste0('NXF_VER=', input$nxf_ver),
      'nextflow', 'run', 'angelovangel/nxf-tgs', 
      '--pipeline', input$pipeline,
      '--fastq', selectedFolder,
      '--samplesheet', samplesheet()$datapath,
      # allows per session cleanup
      '--outdir', file.path('output', session_id),
      '-profile', str_flatten(input$profile, collapse = ','),
      # allows per session cleanup
      '-w', file.path('work', session_id),
      #'-name', paste0(session_id, '_', session_id), # use for nextflow log to get status etc
      sep = ' Space '
      )
    if(str_detect(string = str_flatten(input$profile, collapse = ","), pattern = 'test')) {
      tmux_command <- paste(
        paste0('NXF_VER=', input$nxf_ver),
        'nextflow', 'run', 'angelovangel/nxf-tgs', 
        '--pipeline', input$pipeline,
        '--outdir', file.path('output', session_id),
        '-profile', str_flatten(input$profile, collapse = ','),
        '-w', file.path('work', session_id),
        #'-name', paste0(session_id, '_', session_id), # use for nextflow log to get status etc
        sep = ' Space '
      )
    }
    
    args2 <- c('send-keys', '-t', new_session_name, tmux_command, 'C-m')
    system2('tmux', args = args2)
      # '-c', 'Space', samplesheet()$datapath, 'Space', '-w', 'Space', input$pipeline, 'Space', '-n', 'Space', 
      # new_session_name, 'Space', htmlreport, 'Space', mapping, 'Space', singularity, 'Space', transfer, 'Space', 
      # largeconstruct, 'Space', noassembly, 'Space', assembly_tool, sep = ' '
    
    notify_success(text = paste0('Started session: ', session_id), position = 'center-bottom')
  })
  
  observeEvent(input$show_session, {
    withCallingHandlers({
      shinyjs::html(id = "stdout", "")
      #args <- paste0(' a', ' -t ', session_selected)
      args <- c('capture-pane', '-S', '-', '-E', '-', '-pt', session_selected())
      
      p <- processx::run(
        'tmux', args = args,
        stdout_callback = function(line, proc) {message(line)},
        #stdout_line_callback = function(line, proc) {message(line)},
        stderr_to_stdout = TRUE,
        error_on_status = FALSE
      )
    },
    message = function(m) {
      shinyjs::html(id = "stdout", html = m$message, add = T);
      #runjs("document.getElementById('stdout').parentElement.scrollTo(0,1e9);")
      runjs("document.getElementById('stdout').parentElement.scrollTo({ top: 1e9, behavior: 'smooth' });")
    }
    )
  })
  
  # kill session (and delete data)
  observeEvent(input$kill, {
   
    args <- paste0('kill-session -t ', session_selected())
    if (!is.null(row_selected())) {
      # kill session
      system2('tmux', args = args)
      
      # Remove tarball from www
      tar_path <- file.path("www", paste0(session_selected(), ".tar.gz"))
      if (file.exists(tar_path)) {
        file.remove(tar_path)
      }
      
      # Remove output directory
      outdir <- file.path("output", session_selected())
      if (dir.exists(outdir)) {
        unlink(outdir, recursive = TRUE)
      }
      
      # Remove work directory
      outdir <- file.path("work", session_selected())
      if (dir.exists(outdir)) {
        unlink(outdir, recursive = TRUE)
      }
      notify_success(text = paste0('Session ', session_selected(), ' killed!'), position = 'center-bottom')
    } else {
      notify_failure('Select session first!', timeout = 2000, position = 'center-bottom')
    }  
  })
  
}

shinyApp(ui, server)

