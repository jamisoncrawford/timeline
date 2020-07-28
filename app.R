library(DT)
library(readr)
library(dplyr)
library(shiny)
library(stringr)
library(shinyjs)
library(timevis)
library(htmltools)
library(lubridate)
library(colourpicker)
library(shinyWidgets)
library(shinydashboard)

guides <- read_csv("2020-05-20_timeline-events-guide.csv")

groups <- read_csv("2020-05-20_timeline-events-groups.csv")

ui <- dashboardPage(
    
    dashboardHeader(title = "Roadmap App",
                    tags$li(class = "dropdown",
                            helpText(""),
                            actionBttn(inputId = "screenshot", 
                                       label = " Toggle Screenshot Mode", 
                                       icon = icon("camera"), 
                                       style = "minimal", 
                                       color = "default", 
                                       size = "sm"))),
    
    dashboardSidebar(
        useShinyjs(),
        div(id = "sidebar", sidebarMenu(
            
            # SIDEBAR TITLE: BASICS
            
            menuItem(text = "Basic Details", 
                     tabName = "main", 
                     icon = icon("clipboard-list"), 
                     badgeLabel = "New Timeline", 
                     badgeColor = "blue"),
            
            # STUDENT NAME INPUT
            
            textInput(inputId = "name", 
                      label = "Your Name", 
                      value = "Aspiring Student"),
            
            # GRAD SCHOOL START DATE INPUT
            
            dateInput(inputId = "entry", 
                      label = "Grad School Start Date", 
                      value = ymd(paste0(year(Sys.Date()) + 2, "08-01")),
                      min = Sys.Date() - years(5),
                      max = Sys.Date() + years(5)),
            
            # SIDEBAR TITLE: FILTERS
            
            menuItem("Add Recommendations", 
                     tabName = "main", 
                     icon = icon("graduation-cap"),
                     
                     helpText(HTML('Add or remove select <i>groups</i> of
                                   <br>recommended prep activities.
                                   <br>
                                   <p style="color:lightgray">
                                   <b>Add before custom events</b>.</p>')),
                     
                     checkboxGroupInput(inputId = "groups", 
                                        label = "Event Groups",
                                        choiceNames = c("Final Preparations",
                                                        "Recommendations",
                                                        "Entrance Exam",
                                                        "Exercises",
                                                        "Research"), 
                                        choiceValues = c("prep",
                                                         "recs",
                                                         "exam",
                                                         "exer",
                                                         "rsch")),
                     
                     
                     actionBttn(inputId = "addgroups", 
                                label = "Add Groups", 
                                style = "jelly", 
                                icon = icon("plus-circle"),
                                size = "sm", 
                                color = "primary"),
                     
                     helpText(HTML('<br>To change selected <i>groups</i>,
                                   <br>select "Remove Groups" below.')),
                     
                     actionBttn(inputId = "rmgroups", 
                                label = "Remove Groups",
                                icon = icon("minus-circle"),
                                style = "jelly", 
                                size = "sm"),
                     
                     helpText(HTML("<br>"))),
            
            # SIDEBAR TITLE: ADDITIONS
            
            menuItem("Add Custom Events", 
                     tabName = "main", 
                     icon = icon("calendar-plus"),
                     
                     helpText(HTML("Customize your timeline with <i>points</i>
                                   <br>and <i>ranges</i> in the below menus.")),
                     
                     # ADDITION: POINTS
                     
                     menuItem(text = "Points", 
                              icon = icon("bullseye"), 
                              
                              helpText(HTML("<i>Points</i> require one date and
                                            <br>indicate a single point in time.")),
                              
                              textInput(inputId = "event1", 
                                        label = "Label", 
                                        placeholder = "Attend GSU open house"), 
                              
                              dateInput(inputId = "range1",
                                        label = "Date",
                                        value = as.Date(NA)),
                              
                              actionBttn("action1", "Add Point",
                                         style = "jelly", size = "sm", color = "primary"),
                              
                              helpText(HTML("<br>"))),
                     
                     # ADDITION: RANGES
                     
                     menuItem(text = "Ranges",
                              icon = icon("stopwatch"), 
                              
                              helpText(HTML("<i>Ranges</i> need two dates and
                                   <br>indicate an interval over time.")),
                              
                              textInput(inputId = "event2", 
                                        label = "Label", 
                                        placeholder = "Find mentor at CASA"),
                              
                              dateRangeInput(inputId = "range2",
                                             label = "Start & End Dates", 
                                             start = as.Date(NA),
                                             end = as.Date(NA)),
                              
                              actionBttn(inputId = "action2", 
                                         label = "Add Range", style = "jelly", size = "sm", color = "primary"),
                              
                              helpText(HTML("<br>"))))
            
        )),
        
        collapsed = FALSE),
    
    dashboardBody(tabItems(
        tabItem(tabName = "main",
                
                # TIMELINE OUTPUT
                
                fluidRow(tags$head(tags$style(HTML(".vis-item .vis-item-overflow { overflow: visible; }
                                                    .vis-range { border-color: transparent;
                                                                 background-color: transparent;
                                                                 border-bottom: 5px solid #9AABEA }"))),
                         box(title = textOutput("name"),
                             timevisOutput(outputId = "timeline"), 
                             width = "100%", 
                             height = "100%")),
                
                fluidRow(box(DTOutput(outputId = "timeline_data")),
                         
                         box(width = 6,
                             uiOutput(outputId = "about_ui")))
                
        ))))



server <- function(input, output, session) {
    
    # STUDENT NAME OUTPUT
    
    output$name <- renderText({paste0(input$name, "'s Grad School Roadmap")})
    
    # TIMELINE OUTPUT PER GRAD SCHOOL ENTRY INPUT
    
    output$timeline <- renderTimevis({
        
        today <- Sys.Date()
        
        floor <- floor_date(Sys.Date(), unit = "year")
        
        setup <- guides %>%
            mutate(month = as.integer(month - 23),
                   entry = floor_date(ymd(input$entry) %m+% months(1), unit = "month"),
                   end = entry + months(month),
                   start = end - months(as.integer(duration)),
                   content = event) %>%
            select(id, content, start, end, group, type)
        
        recs <- setup %>% filter(group == "recs")
        exer <- setup %>% filter(group == "exer")
        exam <- setup %>% filter(group == "exam")
        rsch <- setup %>% filter(group == "rsch")
        apps <- setup %>% filter(group == "apps")
        prep <- setup %>% filter(group == "prep")
        
        filtered <- apps %>%
            mutate(style = "word-wrap: normal;",
                   content = gsub("<.*>", "", content))
        
        timevis(data = filtered,
                options = list(editable = list(add = FALSE,
                                               remove = TRUE,
                                               updateTime = TRUE,
                                               updateGroup = TRUE,
                                               overrideItems = TRUE),
                               max = input$entry + months(12),
                               min = input$entry - months(30),
                               multiselect = TRUE,
                               verticalScroll = TRUE))})
    
    observeEvent(input$action1, {addItem("timeline", list(content = input$event1, start = input$range1[1],
                                                          type = "point", 
                                                          style = ""))})
    
    observeEvent(input$action2, {addItem("timeline", list(content = input$event2, start = input$range2[1], 
                                                          end = input$range2[2], type = "range", 
                                                          style = ""))})
    
    
    
    # ADD GROUPS
    
    observeEvent(input$addgroups, {addItems(id = "timeline", guides %>%
                                                mutate(month = as.integer(month - 23),
                                                       entry = floor_date(ymd(input$entry) %m+% months(1), unit = "month"),
                                                       end = entry + months(month),
                                                       start = end - months(as.integer(duration)),
                                                       content = event) %>%
                                                select(id, content, start, end, group, type) %>%
                                                filter(group %in% input$groups))})
    
    
    
    # REMOVE ALL ADDED GROUPS
    
    observeEvent(input$rmgroups, {updateCheckboxGroupInput(session = session, 
                                                           inputId = "addgroups", 
                                                           choiceNames = c("Final Preparations",
                                                                           "Recommendations",
                                                                           "Entrance Exam",
                                                                           "Exercises",
                                                                           "Research"), 
                                                           choiceValues = (c("prep",
                                                                             "recs",
                                                                             "exam",
                                                                             "exer",
                                                                             "rsch")),
                                                           selected = NULL)})
    
    observeEvent(input$rmgroups, {removeItem(id = "timeline", itemId = list(1, 3, 19, 22, 2))})
    observeEvent(input$rmgroups, {removeItem(id = "timeline", itemId = list(30, 31, 32, 33))})
    observeEvent(input$rmgroups, {removeItem(id = "timeline", itemId = list(4, 6, 8, 9, 11, 16, 20, 23))})
    observeEvent(input$rmgroups, {removeItem(id = "timeline", itemId = list(2))})
    observeEvent(input$rmgroups, {removeItem(id = "timeline", itemId = list(5, 7, 10, 14, 15, 18))})

    # OUTPUT TIMEVIS TABLE
    
    output$timeline_data <- renderDT({datatable(input$timeline_data %>%
                                                    rename(Event = content,
                                                           Begin = start,
                                                           End = end) %>%
                                                    arrange(Begin) %>%
                                                    mutate(Event = gsub('<[biu]{1}>|</[biu]{1}>|</font>|<font size = "[0-9]{1,}[\\.]{1}[0-9]{1,}" color = "#[A-Z0-9]{6}">', "", Event),
                                                           Order = 1:n(),
                                                           Begin = format.Date(gsub("T.*$", "", Begin), "(%Y) %B"),
                                                           End = format.Date(gsub("T.*$", "", End), "(%Y) %B")) %>%
                                                    select(Order, Event, Begin, End),
                                                extensions = "Buttons",
                                                options = list(dom = "Blfrtip",
                                                               buttons = list("copy", 
                                                                              "print",
                                                                              list(extend = "collection",
                                                                                   buttons = c("csv",
                                                                                               "excel",
                                                                                               "pdf"),
                                                                                   text = "Download")),
                                                               pageLength = 20,
                                                               lengthMenu = c(5, 10, 20, 30, 40, 50, 100)),
                                                rownames = FALSE, 
                                                editable = FALSE)})
    
    # RENDER ABOUT
    
    output$about_ui <- renderUI({tabBox(width = 12, 
                                         id = "about_tab1",
                                         tabPanel(title = "About", icon = icon("info-circle"), value = "about_tab2",
                                                  HTML("<h4>Your Journey to Graduate School</h4>
                                              The <b>Graduate Prep Roadmap App</b> is an interactive platform
                                              for setting goals and planning ahead on your road to entering 
                                              graduate school. This tool is brought to you by the 
                                              <i>Center for the Advancement of Students & Alumni's (CASA)</i>
                                              at <i>The Graduate School</i> at <i>Georgia State University</i>.
                                              <br><br>
                                              The <b>Roadmap App</b> has a series of recommended activities and 
                                              events to prepare for graduate schoole, including:
                                              <br><br>
                                              <ul>
                                              <li>Preparing essential graduate school skills</li>
                                              <li>Securing academic and professional references</li>
                                              <li>Developing and submitting your applications</li>
                                              <li>Researching potential schools and programs</li>
                                              <li>Preparing for entrance exams like the GRE</li>
                                              </ul>
                                              <br>
                                              <i>Click to navigate the above tabs to learn more about using this app.</i>"), 
                                                  verbatimTextOutput("tab1")),
                                         tabPanel(title = "Use", icon = icon("list-alt"),
                                                  HTML("<h4>Flexible Preparation to Meet Your Needs</h4>
                                              The <b>Roadmap App</b> is a flexible, dynamic tool that allows you to
                                              customize recommended activities and add personalized goals based on <i>your</i>
                                              needs. Since needs differ by program, progress, and each aspiring student,
                                              the app is designed to meet you where you are. You can take it from there.
                                              <br><br>
                                              <h4>Sidebar Navigation</h4>
                                              In the <b>Sidebar</b>, the timeline will automatically adjust when you enter 
                                              your anticipated start date.
                                              <b>Add Recommendations</b> to insert activities based on your preparation 
                                              needs. Remove and re-add event groups using the <b>Remove Groups</b> button.
                                              <b>Add Custom Events</b> allows you to add points and ranges with custom labels and dates. 
                                              <br><br>
                                              <h4>Interactive Timeline & Dynamic Events Table</h4>
                                              The <b>Timeline</b> is fully interactive and editable. Zoom in and out, click-and-drag
                                              items to change their date and duration, or remove them with <b>X</b>. Click-and-hold an 
                                              item to select more than one. Click-and-drag to move forwards or backwards.
                                              The <b>Roadmap Events Table</b> updates dynamically to <b>Timeline</b> changes. 
                                              Sort events using the column arrows, navigate with pagination, 
                                              and choose how many events to see. Use the top
                                              buttons for different ways to export the table."), 
                                                  verbatimTextOutput("tab2")),
                                         tabPanel(title = "CASA", icon = icon("home"), 
                                                  HTML("<h4>The Center for the Advancement of Students & Alumni</h4>
                                              The <b>Georgia State University (GSU) Center for the Advancement of Students and Alumni 
                                              (CASA)</b> serves as an institutional hub that supports the progression of students from 
                                              all backgrounds into PhD programs, medical school, law school, or related pathways.
                                              <b><a href='https://casa.gsu.edu/'>Click here to learn more about CASA.</a></b>"), 
                                                  verbatimTextOutput("tab3")),
                                         tabPanel(title = "Documentation", icon = icon("address-card"),
                                                  HTML("<h4>App Licensing, Distribution, Documentation, & Internals</h4>
                                              The <b>Roadmap App</b> was developed using R, Shiny, HTML, and JavaScript, as 
                                              well as various R packages and wrapper libraries for other languages. This 
                                              product is an open source application licensed under the 
                                              <b><a href='https://www.r-project.org/Licenses/LGPL-3'>GNU Lesser 
                                              General Public License 3 (LGPL-3)</a></b>
                                              and the source code and additional documentation may be found in
                                              <b><a href='https://github.com/jamisoncrawford/timeline'>this GitHub repository</a></b>.
                                              <br><br>
                                              <h4>About the Developer</h4>
                                              <b>Jamison Crawford, MPA</b> is an institutional researcher for <i>The Graduate School</i>
                                              and <i>Center for the Advancement of Students & Alumni (CASA)</i> as well as a professor of 
                                              data science at <i>Georgia State University</i> and <i>Arizone State University</i>.
                                              <b><a href='mailto:jcrawford52@gsu.edu'>Contact</a></b>.
                                              <br><br>
                                              <h4>Contributors</h4>
                                              Special thanks to <b>Kyle Frantz, Ph.D.</b> and <b>Sarah Clark, Ph.D.</b> of 
                                              <i>Georgia State University</i> for originally conceptualizing this application and 
                                              developing recommended graduate school preparation activities, respectively."), 
                                                  verbatimTextOutput("tab4")),
                                         htmlOutput(outputId = "about_tab"))})
    
    # PRINT MODE
    
    observeEvent(input$screenshot, {toggle(id = "timeline_data", anim = TRUE)})
    observeEvent(input$screenshot, {toggle(id = "about_ui", anim = TRUE)})}

shinyApp(ui = ui, 
         server = server)
