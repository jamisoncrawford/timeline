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
    
    dashboardHeader(title = "Roadmap App"),
    
    dashboardSidebar(
        useShinyjs(),
        sidebarMenu(
            
            # SIDEBAR TITLE: BASICS
            
            menuItem(text = "Basic Details", 
                     tabName = "dashboard", 
                     icon = icon("clipboard-list"), 
                     badgeLabel = "Required", 
                     badgeColor = "green"),
            
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
            
            menuItem("Add Event Groups", 
                     tabName = "groups", 
                     icon = icon("object-group"),
                     
                     menuItem(text = "Recommended Events", 
                              icon = icon("graduation-cap"),
                     
                     helpText(HTML("Add one or more groups of events 
                                   <br>based on your preparation needs.")),
                     
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
                                label = "Add Groups", style = "jelly", size = "sm", color = "primary"),
                     
                     helpText(HTML("<br>"))),
                     
                     menuItem(text = "Remove Groups",
                              icon = icon("calendar-minus"),
                              
                              helpText(HTML("Remove all added groups. You
                                            <br> may reinsert them above.")),
                     
                     actionBttn(inputId = "rmgroups", 
                                label = "Remove All", style = "jelly", size = "sm"),
                     
                     helpText(HTML("<br>")))),
            
            # SIDEBAR TITLE: ADDITIONS
            
            menuItem("Add Elements", 
                     tabName = "timeline", 
                     icon = icon("calendar-plus"),
                     
                     helpText(HTML("Customize your timeline with point,
                                   <br>range, and backdrop menus below.")),
                     
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
                              
                              helpText(HTML("<br>"))),
                     
                     
                     # ADDITION: BACKDROPS
                     
                     menuItem(text = "Backdrops",
                              icon = icon("layer-group"), 
                              
                              menuItem(text = "Add Backdrops",
                                       tabName = "addbackdrop",
                                       icon = icon("plus"),
                              
                              helpText(HTML("<i>Backdrops</i> need two dates to
                                   <br>create a shaded background.")),
                              
                              textInput(inputId = "event3", 
                                        label = "Label", 
                                        placeholder = "Crunch time"),
                              
                              dateRangeInput(inputId = "range3",
                                             label = "Start & End Dates", 
                                             start = as.Date(NA),
                                             end = as.Date(NA)),
                              
                              actionBttn("action3", "Add Backdrop", 
                                         style = "jelly", size = "sm", color = "primary"),
                              
                              helpText(HTML("<br>"))),
                              
                              menuItem(text = "Remove Backdrops", 
                                       tabName = "rmbackdrop",
                                       icon = icon("trash-alt"),
                              
                              helpText(HTML("Backdrops <i>cannot</i> be removed
                                            <br> by interacting with timeline.")),
                              
                              actionBttn("rm_backdrops", "Remove Backdrops",
                                         style = "jelly", size = "sm"),
                              
                              helpText(HTML("<br>"))))),
            
            # SIDEBAR TITLE: CALLOUTS
            
            menuItem("Insert Callouts", 
                     tabName = "timeline", 
                     icon = icon("exclamation-triangle"),
                     
                     helpText(HTML("Double-click on the timeline to add
                                   <br>one or more custom <i>callouts</i>. Only
                                   <br>one text label may be used. ")),
                     
                     # CUSTOMIZE CALLOUT MESSAGE
                     
                     textInput(inputId = "callout", 
                               label = "Callout Text",
                               value = "Heads up!"),
                     
                     checkboxGroupInput(inputId = "coformat", 
                                        label = "Callout Formatting", 
                                        choices = c("Bold",
                                                    "Italics",
                                                    "Underline"), 
                                        selected = "Bold"),
                     
                     sliderInput(inputId = "cosize", 
                                 label = "Callout Font Size", 
                                 min = 1, 
                                 max = 10, 
                                 value = 3, 
                                 step = 1),
                     
                     colourInput(inputId = "cocolor", 
                                 label = "Callout Color", 
                                 value = "#000000"),
                     
                     helpText(HTML("<br>")))
            
        ),
        
        collapsed = FALSE),
    
    dashboardBody(tabItems(
        tabItem(tabName = "dashboard",
                
                # TIMELINE OUTPUT
                
                fluidRow(tags$head(tags$style(HTML(".vis-item .vis-item-overflow { overflow: visible; }
                                                    .vis-range { border-color: transparent;
                                                                 background-color: transparent;
                                                                 border-bottom: 5px solid #9AABEA }"))),
                         box(title = textOutput("name"),
                             timevisOutput(outputId = "timeline"), 
                             width = "100%", 
                             height = "100%")),
                
                fluidRow(box(title = "Roadmap Events",
                             DTOutput(outputId = "timeline_data")),
                         
                         tabBox(title = "About",
                                id = "tab",
                                tabPanel(title = "About the App", icon = icon("info-circle"),
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
                                tabPanel(title = "How to Use It", icon = icon("list-alt"),
                                         HTML("<h4>Flexible Preparation to Meet Your Needs</h4>
                                              The <b>Roadmap App</b> is a flexible, dynamic tool that allows you to
                                              customize recommended activities and add personalized goals based on <i>your</i>
                                              needs.  Since needs differ by program, progress, and each aspiring student,
                                              the app is designed to meet you where you are now. You can take it from there.
                                              <br><br>
                                              <h4>Sidebar Navigation</h4>
                                              In the <b>Sidebar Menu</b>, start by providing your name and target date
                                              to enter into your graduate program. The timeline will adjust automatically.
                                              <b>Event Groups</b> allow you to add groups of recommended activities to your 
                                              timeline based on your preparation needs, like exam prep and program research.
                                              You can remove and re-add event groups using the <b>Remove Groups</b> button.
                                              <br><br>
                                              <b>Add Elements</b> allows you to add points, ranges, and shaded backdrops 
                                              with cutom labels and dates. Shaded backdrops can only be removed here. Lastly,
                                              <b>Callouts</b> allow you to create a single, custom label to attract attention
                                              to specific points in your timeline. Double-click on the timeline to insert a callout.
                                              Collapse the entire menu by clicking the three bars near the app title.
                                              <br><br>
                                              <h4>Interactive Timeline</h4>
                                              The <b>Timeline</b> is fully interactive and editable. Use the zoom buttons or 
                                              a mousewheel to home in or expand the length of the timeline, or click and drag 
                                              to navigate backward or forward in time (i.e. left or right). Click on an item to
                                              delete it, or select multiple with 'ctrl', 'shift', or by holding down the mouse. Move
                                              one or more event to a different time by clicking-and-dragging them.  Adjust the 
                                              duration of time by selecting and dragging the left or right edges of each item. Delete
                                              an item by clicking the red 'X'.
                                              <br><br>
                                              <h4>Dynamic Events Table</h4>
                                              The <b>Roadmap Events Table</b> adjusts dynamically to changes in the <b>Timeline</b>, 
                                              including <b>Callouts</b>, new <b>Elements</b>, and <b>Event Groups</b>, as well as 
                                              modifications to existing events. Double-click on a specific value to change the 
                                              text. Rearrange events using the column arrows, navigate to other events using the 
                                              lower-right pagination, and show more entries using the 'Show Entries' dropdown. Use the
                                              buttons to copy the table to your clipboard, print the table, or download the table 
                                              as a CSV file, Excel spreadsheet, or PDF. 
                                              <br><br>
                                              <i>Changes to the timeline will update the table,
                                              but changes to the table will not update the timeline.</i>"), 
                                         verbatimTextOutput("tab2")),
                                tabPanel(title = "About the CASA", icon = icon("home"),
                                         HTML("<h4>The Center for the Advancement of Students & Alumni</h4>
                                              The <i>Georgia State University (GSU) Center for the Advancement of Students and Alumni 
                                              (CASA)</i> serves as an institutional hub that supports the progression of students from 
                                              all backgrounds into PhD programs, medical school, law school, or related pathways.
                                              <br><br>
                                              <b><a href='https://casa.gsu.edu/'>Click here to learn more.</a></b>"), 
                                         verbatimTextOutput("tab3")),
                                tabPanel(title = "Documentation", icon = icon("address-card"),
                                         HTML("<h4>App Licensing, Distribution, Documentation, & Internals</h4>
                                              The <b>Roadmap App</b> was developed using R, Shiny, HTML, and JavaScript, as 
                                              well as various R packages and wrapper libraries for other languages. This 
                                              product is an open source application licensed under the 
                                              <b><a href='https://www.r-project.org/Licenses/LGPL-3'>GNU Lesser 
                                              General Public License v. 3 (LGPL-3)</a></b>
                                              and the source code and additional documentation may be found in
                                              <b><a href='https://github.com/jamisoncrawford/timeline'>this GitHub repository</a></b>.
                                              <br><br>
                                              Sharing or repurposing the code is both permitted and encouraged in accordance with
                                              LGPL-3.
                                              <br><br>
                                              <h4>Contributors</h4>
                                              Special thanks is owed to <b>Sarah Clark, Ph.D.</b> of <i>Georgia State University</i>
                                              for originally conceptualizing this product and developing the recommended graduate 
                                              school prepration activities on which much of our timeline is based. 
                                              <br><br>
                                              <h4>About the Developer</h4>
                                              <b>Jamison Crawford, MPA</b> is an institutional researcher for <i>The Graduate School</i>
                                              and <i>Center for the Advancement of Students & Alumni (CASA)</i> at <i>Georgia State
                                              University </i>, where he also teaches the graduate course 'Coding in R' at 
                                              the <i>Andrew Young School School</i>. Jamison is also an Associate Faculty at the
                                              <i>Watts College</i> at <i>Arizone State University</i> where he teaches 'Foundations of 
                                              Data Science' for the Master of Science in Program Evaluation & Data Analytics (MS-PEDA).
                                              <br><br>
                                              He works as an independent consult for social sector data analytics, most notably for 
                                              the <i>Central New York Community Foundation</i>, and has authored numerious instructional
                                              works in applied data science for social impact, including co-authoring
                                              <b><a href='https://ds4ps.org/'>Data Science for Public Service (DS4PS)</a></b>.
                                              Please feel free to reach out via 
                                              <b><a href='https://www.linkedin.com/in/jamisoncrawford/'>LinkedIn</a></b>, 
                                              <b><a href='https://github.com/jamisoncrawford'>GitHub</a></b>, 
                                              <b><a href='https://rpubs.com/JamisonCrawford'>RPubs</a></b>, or 
                                              <b><a href='mailto:jcrawford52@gsu.edu'>Email</a></b>.
                                              
                                              "), 
                                         verbatimTextOutput("tab4")),
                                htmlOutput(outputId = "about")))
                
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
        
        # CALLOUT FORMATTING 
        
        bold_start <- ifelse("Bold" %in% input$coformat, "<b>", "")
        bold_end <- ifelse("Bold" %in% input$coformat, "</b>", "")
        
        italics_start <- ifelse("Italics" %in% input$coformat, "<i>", "")
        italics_end <- ifelse("Italics" %in% input$coformat, "</i>", "")
        
        underline_start <- ifelse("Underline" %in% input$coformat, "<u>", "")
        underline_end <- ifelse("Underline" %in% input$coformat, "</u>", "")
        
        formatting_start <- paste0(bold_start, italics_start, underline_start)
        formatting_end <- paste0(bold_end, italics_end, underline_end)
        
        size <- input$cosize * 0.75
        color <- input$cocolor
        
        timevis(data = filtered,
                options = list(editable = TRUE,
                               max = input$entry + months(3),
                               min = input$entry - years(3),
                               multiselect = TRUE,
                               verticalScroll = TRUE,
                               onAdd = htmlwidgets::JS(paste0("function(item, callback){
                                                              item.content = '",
                                                              '<font size = "',
                                                              size,
                                                              '"',
                                                              ' color = "',
                                                              color,
                                                              '"',
                                                              '>',
                                                              formatting_start,
                                                              input$callout,
                                                              formatting_end,
                                                              "</font>",
                                                              "';
                                                              callback(item);}"))))})
    
    observeEvent(input$action1, {addItem("timeline", list(content = input$event1, start = input$range1[1],
                                                          type = "point", 
                                                          style = ""))})
    
    observeEvent(input$action2, {addItem("timeline", list(content = input$event2, start = input$range2[1], 
                                                          end = input$range2[2], type = "range", 
                                                          style = ""))})
    
    observeEvent(input$action3, {addItem("timeline", list(content = input$event3, start = input$range3[1], 
                                                          end = input$range3[2], type = "background", 
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
    
    observeEvent(input$rm_backdrops, {removeItem(id = "timeline", 
                                                 itemId = input$timeline_data[input$timeline_data$type == "background", "id"])})
    
    
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
                                                               pageLength = 5,
                                                               lengthMenu = c(5, 10, 20, 30, 40, 50, 100)),
                                                rownames = FALSE, 
                                                editable = TRUE)})}

shinyApp(ui = ui, 
         server = server)
