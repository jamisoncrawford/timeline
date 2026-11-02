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

guides <- read_csv("2020-05-20_timeline-events-guide.csv", show_col_types = FALSE)

groups <- read_csv("2020-05-20_timeline-events-groups.csv", show_col_types = FALSE)

default_groups <- c("prep", "rsch")
default_entry_date <- function() ymd(paste0(year(Sys.Date()) + 2, "-08-01"))

blank_date_widget <- function(widget) {
    htmltools::tagQuery(widget)$find("input")$addAttrs(`data-initial-date` = "")$allTags()
}

ui <- dashboardPage(
    dashboardHeader(disable = TRUE),

    dashboardSidebar(
        width = 280,
        useShinyjs(),
        div(id = "sidebar",
            div(class = "casa-sidebar-brand",
                tags$img(src = "casa-logo-gsu.png",
                         alt = "CASA - Center for the Advancement of Students and Alumni")),
            sidebarMenu(
            tags$li(class = "sidebar-name-control",
                    div(class = "sidebar-name-heading",
                        tags$label(`for` = "name", class = "control-label", "Your Name"),
                        actionButton(inputId = "newtimeline", label = "New Timeline",
                                     class = "new-timeline-button btn-xs")),
                    textInput(inputId = "name",
                              label = NULL,
                              value = "Aspiring Student")),

            dateInput(inputId = "entry",
                      label = "Grad School Start Date",
                      value = default_entry_date(),
                      min = Sys.Date() - years(5),
                      max = Sys.Date() + years(5)),

            # SIDEBAR TITLE: FILTERS
            menuItem("Add Recommendations",
                     icon = icon("graduation-cap"),

                     tags$li(class = "sidebar-control sidebar-control--intro",
                             helpText(HTML('Add or remove select <i>groups</i> of
                                            recommended prep activities.')),
                             div(class = "sidebar-callout", "Add before custom events.")),

                     tags$li(class = "sidebar-control sidebar-control--checklist",
                             checkboxGroupInput(
                                 inputId = "groups",
                                 label = "Event Groups",
                                 choiceNames = c("Final Preparations",
                                                 "Recommendations",
                                                 "Entrance Exam",
                                                 "Exercises",
                                                 "Research"),
                                 choiceValues = c("prep", "recs", "exam", "exer", "rsch"),
                                 selected = default_groups)),

                     tags$li(class = "sidebar-control sidebar-control--button",
                             actionBttn(inputId = "addgroups",
                                        label = "Add Groups",
                                        style = "jelly",
                                        icon = icon("plus-circle"),
                                        size = "xs",
                                        color = "primary")),

                     tags$li(class = "sidebar-control sidebar-control--helper",
                             helpText(HTML('To change selected <i>groups</i>,
                                            select "Remove Groups" below.'))),

                     tags$li(class = "sidebar-control sidebar-control--button sidebar-control--last",
                             actionBttn(inputId = "rmgroups",
                                        label = "Remove Groups",
                                        icon = icon("minus-circle"),
                                        style = "jelly",
                                        size = "xs"))),

            # SIDEBAR TITLE: ADDITIONS
            menuItem("Add Custom Events",
                     icon = icon("calendar-plus"),

                     tags$li(class = "sidebar-control sidebar-control--intro sidebar-control--section-intro",
                             helpText(HTML('Customize your timeline with <i>points</i>
                                            and <i>ranges</i> using the menus below.'))),

                     # ADDITION: POINTS
                     menuItem(text = "Points",
                              icon = icon("bullseye"),

                              tags$li(class = "sidebar-control sidebar-control--intro",
                                      helpText(HTML('<i>Points</i> require one date and
                                                     indicate a single point in time.'))),

                              tags$li(class = "sidebar-control sidebar-control--field",
                                      textInput(inputId = "event1",
                                                label = "Label",
                                                placeholder = "Attend GSU open house")),

                              tags$li(class = "sidebar-control sidebar-control--field",
                                      blank_date_widget(dateInput(inputId = "range1",
                                                                  label = "Date",
                                                                  value = NULL))),

                              tags$li(class = "sidebar-control sidebar-control--button sidebar-control--last",
                                      actionBttn("action1", "Add Point",
                                                 style = "jelly", size = "xs", color = "primary"))),

                     # ADDITION: RANGES
                     menuItem(text = "Ranges",
                              icon = icon("stopwatch"),

                              tags$li(class = "sidebar-control sidebar-control--intro",
                                      helpText(HTML('<i>Ranges</i> need two dates and
                                                     indicate an interval over time.'))),

                              tags$li(class = "sidebar-control sidebar-control--field",
                                      textInput(inputId = "event2",
                                                label = "Label",
                                                placeholder = "Find mentor at CASA")),

                              tags$li(class = "sidebar-control sidebar-control--field",
                                      blank_date_widget(dateRangeInput(inputId = "range2",
                                                                       label = "Start & End Dates",
                                                                       start = NULL,
                                                                       end = NULL))),

                              tags$li(class = "sidebar-control sidebar-control--button sidebar-control--last",
                                      actionBttn(inputId = "action2",
                                                 label = "Add Range",
                                                 style = "jelly", size = "xs", color = "primary"))))

        )),
        div(class = "sidebar-footer", style = "padding-bottom: 20px;",
            actionBttn(inputId = "screenshot",
                       label = "Toggle Screenshot Mode",
                       icon = icon("camera"),
                       style = "jelly",
                       color = "primary",
                       size = "xs"),
            span(class = "sidebar-version", "Version 1.0")),
        collapsed = FALSE),

    dashboardBody(
        tags$head(
            tags$title("CASA | Graduate School Roadmap"),
            tags$meta(name = "description",
                      content = "An interactive graduate-school preparation roadmap developed for CASA at Georgia State University."),
            tags$meta(property = "og:type", content = "website"),
            tags$meta(property = "og:title", content = "CASA | Graduate School Roadmap"),
            tags$meta(property = "og:description",
                      content = "Explore an interactive timeline for planning graduate-school applications and preparation activities."),
            tags$meta(property = "og:url",
                      content = "https://uruguayguy.shinyapps.io/timeline/"),
            tags$meta(property = "og:image",
                      content = "https://uruguayguy.shinyapps.io/timeline/linkedin-thumbnail.png"),
            tags$meta(property = "og:image:alt",
                      content = "CASA Graduate School Roadmap application preview"),
            tags$meta(name = "twitter:card", content = "summary_large_image"),
            tags$link(rel = "canonical", href = "https://uruguayguy.shinyapps.io/timeline/"),
            tags$style(HTML('
            :root {
                --gsu-blue: #0039A6;
                --gsu-blue-steel: #374057;
                --gsu-cool-blue: #0071CE;
                --gsu-vibrant-blue: #00AEEF;
                --gsu-red-accent: #CC0000;
                --gsu-light-blue: #97CAEB;
                --gsu-medium-gray: #CCCCCC;
                --gsu-light-gray: #EEEEEE;
                --panel-inset: 16px;
            }
            .main-header { display: none !important; }
            .skin-blue .main-sidebar,
            .skin-blue .left-side {
                top: 0 !important;
                padding-top: 0 !important;
                height: 100vh;
                height: 100dvh;
                overflow: hidden;
                background-color: var(--gsu-blue) !important;
            }
            .main-sidebar > .sidebar {
                box-sizing: border-box;
                height: 100vh;
                height: 100dvh;
                padding: 0 !important;
                display: flex;
                flex-direction: column;
                min-height: 0;
                overflow: hidden;
            }
            #sidebar {
                flex: 1 1 auto;
                min-height: 0;
                overflow-y: auto;
                overflow-x: hidden;
                background-color: var(--gsu-blue) !important;
            }
            #sidebar {
                scrollbar-width: thin; /* Firefox and modern Chromium */
                scrollbar-color: var(--gsu-medium-gray) transparent;
            }
            #sidebar::-webkit-scrollbar {
                width: 6px;
                background-color: transparent;
            }
            #sidebar::-webkit-scrollbar-track,
            #sidebar::-webkit-scrollbar-track-piece,
            #sidebar::-webkit-scrollbar-corner {
                background-color: transparent;
                border: 0;
            }
            #sidebar::-webkit-scrollbar-button {
                display: none;
                width: 0;
                height: 0;
            }
            #sidebar::-webkit-scrollbar-thumb,
            #sidebar::-webkit-scrollbar-thumb:hover {
                background-color: var(--gsu-medium-gray);
                border-radius: 999px;
                border: 0;
            }
            #sidebar .sidebar-menu,
            #sidebar .sidebar-menu > li > a {
                background-color: var(--gsu-blue) !important;
            }
            #sidebar .sidebar-menu .treeview-menu,
            #sidebar .sidebar-menu .treeview-menu > li > a {
                background-color: var(--gsu-blue-steel) !important;
            }
            .skin-blue .sidebar-menu > li:hover > a,
            .skin-blue .sidebar-menu > li.active > a,
            .skin-blue .sidebar-menu > li.menu-open > a,
            #sidebar .sidebar-menu .treeview-menu > li > a:hover {
                background-color: rgba(255,255,255,.10) !important;
            }
            #sidebar .sidebar-menu > li > a,
            #sidebar .sidebar-menu .treeview-menu > li > a,
            #sidebar .sidebar-menu li.sidebar-control {
                color: #fff !important;
            }
            #sidebar .casa-sidebar-brand {
                box-sizing: border-box;
                background: var(--gsu-blue);
                display: flex;
                align-items: center;
                justify-content: center;
                width: 100%;
                padding: 15px;
            }
            #sidebar .casa-sidebar-brand img {
                display: block;
                width: 250px;
                max-width: 100%;
                height: auto;
                object-fit: contain;
            }
            .skin-blue .main-sidebar .sidebar-footer {
                position: relative;
                box-sizing: border-box;
                flex: 0 0 66px;
                height: 66px;
                min-height: 66px;
                padding: 14px 20px 20px;
                background-color: var(--gsu-blue);
            }
            .main-sidebar .sidebar-footer #screenshot,
            .main-sidebar .sidebar-footer .sidebar-version {
                position: absolute;
                top: calc(50% + 8px);
                transform: translateY(-50%);
                margin: 0 !important;
                white-space: nowrap;
            }
            .main-sidebar .sidebar-footer #screenshot {
                left: 20px;
                width: auto;
            }
            .main-sidebar .sidebar-footer .sidebar-version {
                right: 20px;
                color: #fff;
                font-size: 12px;
                font-weight: 400;
                line-height: 1.2;
            }
            .main-sidebar .sidebar-footer .bttn-jelly.bttn-primary {
                background-color: var(--gsu-vibrant-blue) !important;
                border-color: var(--gsu-vibrant-blue) !important;
                color: #fff !important;
            }
            .main-sidebar .sidebar-footer .bttn-jelly.bttn-primary:hover {
                background-color: #008CC0 !important;
                border-color: #008CC0 !important;
            }

            #sidebar {
                --sidebar-inset: 20px;
                --sidebar-gap: 10px;
                --sidebar-half-gap: 5px;
                --sidebar-submenu-gap: 15px;
                --sidebar-label-field-gap: 7px; /* Same as Basic Details. */
            }
            #sidebar .sidebar-menu,
            #sidebar .sidebar-menu li,
            #sidebar .sidebar-menu li > a,
            #sidebar .sidebar-menu .treeview-menu {
                white-space: normal !important;
            }
            #sidebar .sidebar-menu li > a > span:not(.pull-right-container),
            #sidebar .sidebar-control .help-block,
            #sidebar .sidebar-callout {
                white-space: normal !important;
                overflow-wrap: break-word;
                word-break: normal;
            }
            #sidebar .sidebar-menu li > a > i.fa:not(.pull-right),
            #sidebar .sidebar-menu li > a > i.fas:not(.pull-right),
            #sidebar .sidebar-menu li > a > i.far:not(.pull-right) {
                display: inline-block;
                flex: 0 0 20px;
                width: 20px;
                margin-right: 9px;
                text-align: center;
                vertical-align: middle;
            }
            #sidebar .sidebar-menu > li > a,
            #sidebar .sidebar-menu .treeview-menu > li.treeview > a {
                display: flex;
                align-items: center;
                box-sizing: border-box;
                min-width: 0;
            }
            #sidebar .sidebar-menu > li > a > span:not(.pull-right-container),
            #sidebar .sidebar-menu .treeview-menu > li.treeview > a > span:not(.pull-right-container) {
                flex: 1 1 auto;
                min-width: 0;
            }
            #sidebar .sidebar-menu li > a > i.pull-right {
                flex: 0 0 auto;
                margin-left: auto;
            }
            #sidebar .sidebar-menu > li.sidebar-name-control {
                list-style: none;
                box-sizing: border-box;
                width: 100%;
                padding: 0 var(--sidebar-inset);
                margin: -3px 0 var(--sidebar-gap);
            }
            #sidebar .sidebar-name-heading {
                display: flex;
                align-items: center;
                justify-content: space-between;
                min-height: 24px;
                gap: 8px;
                margin: 0 0 var(--sidebar-label-field-gap);
            }
            #sidebar .sidebar-name-heading .control-label {
                flex: 1 1 auto;
                min-width: 0;
                margin: 0 !important;
                line-height: 20px;
                color: #fff;
            }
            #sidebar .sidebar-name-control .shiny-input-container {
                box-sizing: border-box;
                width: 100% !important;
                min-width: 0;
                max-width: 100%;
                padding: 0 !important;
                margin: 0 !important;
            }
            #sidebar .sidebar-name-control .form-control {
                box-sizing: border-box;
                width: 100%;
            }
            #sidebar .new-timeline-button {
                position: static;
                transform: none;
                flex: 0 0 auto;
                display: inline-flex;
                align-items: center;
                justify-content: center;
                margin: 0 0 0 auto;
                padding: 3px 9px;
                border: 0;
                border-radius: 12px;
                font-size: 10px;
                font-weight: 700;
                line-height: 1.2;
                white-space: nowrap;
                color: #fff !important;
                background-color: var(--gsu-vibrant-blue) !important;
            }
            #sidebar .new-timeline-button:hover,
            #sidebar .new-timeline-button:focus {
                background-color: #008CC0 !important;
                color: #fff !important;
            }
            #sidebar .sidebar-menu > .shiny-input-container,
            #sidebar .sidebar-menu > .form-group.shiny-input-container {
                box-sizing: border-box !important;
                width: calc(100% - 2 * var(--sidebar-inset)) !important;
                max-width: none !important;
                min-width: 0;
                padding: 0 !important;
                margin: 0 var(--sidebar-inset) var(--sidebar-gap) !important;
            }
            #sidebar .sidebar-menu > .shiny-input-container .control-label {
                display: block;
                margin: 0 0 var(--sidebar-label-field-gap) !important;
            }
            #sidebar .sidebar-menu > .shiny-input-container .form-control,
            #sidebar .sidebar-menu > .shiny-input-container .input-group {
                box-sizing: border-box;
                width: 100%;
                max-width: 100%;
            }
            #sidebar .sidebar-menu .treeview-menu,
            #sidebar .sidebar-menu .treeview-menu .treeview-menu {
                padding-left: 0 !important;
            }
            #sidebar .sidebar-menu .treeview-menu > li.treeview > a {
                padding: 4px var(--sidebar-inset);
            }
            #sidebar .sidebar-menu li.sidebar-control {
                display: block;
                box-sizing: border-box;
                width: 100%;
                padding: 0 var(--sidebar-inset);
                margin: 0;
                list-style: none;
                text-align: left;
                white-space: normal !important;
                overflow-wrap: break-word;
            }
            #sidebar .sidebar-menu li.sidebar-control--intro {
                padding-top: var(--sidebar-submenu-gap);
            }
            #sidebar .sidebar-menu li.sidebar-control--section-intro {
                padding-bottom: var(--sidebar-submenu-gap);
            }
            #sidebar .sidebar-menu .treeview-menu > li.treeview > .treeview-menu > li.sidebar-control--intro {
                padding-top: var(--sidebar-submenu-gap);
            }
            #sidebar .sidebar-menu li.sidebar-control--checklist,
            #sidebar .sidebar-menu li.sidebar-control--field,
            #sidebar .sidebar-menu li.sidebar-control--button,
            #sidebar .sidebar-menu li.sidebar-control--helper {
                margin-top: var(--sidebar-submenu-gap);
            }
            #sidebar .sidebar-menu li.sidebar-control--last {
                padding-bottom: var(--sidebar-submenu-gap);
            }
            #sidebar .sidebar-control .help-block {
                display: block;
                margin: 0;
                line-height: 1.5;
                color: #E3EEFF !important;
            }
            #sidebar .sidebar-callout {
                color: #fff;
                font-weight: 600;
                margin: var(--sidebar-submenu-gap) 0 0;
                line-height: 1.5;
            }
            #sidebar .sidebar-control > .shiny-input-container,
            #sidebar .sidebar-control > .form-group,
            #sidebar .sidebar-control .shiny-input-container {
                box-sizing: border-box;
                width: 100% !important;
                max-width: 100%;
                min-width: 0;
                padding: 0 !important;
                margin: 0 !important;
            }
            #sidebar .sidebar-control .form-control,
            #sidebar .sidebar-control .input-group {
                box-sizing: border-box;
                width: 100%;
                max-width: 100%;
            }
            #sidebar .sidebar-control .control-label {
                display: block;
                margin: 0 0 var(--sidebar-submenu-gap) !important;
            }
            #sidebar .sidebar-control--field .control-label {
                margin-bottom: var(--sidebar-label-field-gap) !important;
            }
            #sidebar .sidebar-control--checklist .shiny-options-group {
                display: flex;
                flex-direction: column;
                gap: 7px;
            }
            #sidebar .sidebar-control .checkbox { margin: 0 !important; }
            #sidebar .sidebar-control .checkbox label { color: #fff; }
            #sidebar .sidebar-control--button .action-button {
                display: inline-block;
                width: auto !important;
                max-width: 100%;
                margin: 0;
                vertical-align: top;
            }
            #sidebar .bttn-jelly.bttn-primary,
            #sidebar .bttn-primary.bttn-jelly {
                background-color: var(--gsu-vibrant-blue) !important;
                border-color: var(--gsu-vibrant-blue) !important;
                color: #fff !important;
            }
            #sidebar .bttn-jelly.bttn-primary:hover,
            #sidebar .bttn-jelly.bttn-primary:focus {
                background-color: #008CC0 !important;
                border-color: #008CC0 !important;
                color: #fff !important;
            }

            .content-wrapper, .right-side { min-height: 100vh !important; min-height: 100dvh !important; }
            .content-wrapper > .content {
                box-sizing: border-box;
                height: 100vh;
                height: 100dvh;
                min-height: 520px;
                padding: 15px;
                overflow: hidden;
            }
            .content-wrapper > .content > .tab-content,
            .content-wrapper > .content > .tab-content > .tab-pane.active {
                height: 100%;
                min-height: 0;
            }
            #roadmap-layout {
                display: grid;
                grid-template-rows: minmax(0, 1fr) minmax(0, 1fr);
                gap: 15px;
                height: 100%;
                min-height: 0;
                min-width: 0;
            }
            #roadmap-layout .roadmap-bottom {
                display: grid;
                grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
                gap: 15px;
                min-height: 0;
                min-width: 0;
            }
            #roadmap-layout .roadmap-panel,
            #roadmap-layout .roadmap-panel > div {
                height: 100%;
                min-width: 0;
                min-height: 0;
            }
            #roadmap-layout .roadmap-panel .box {
                display: flex;
                flex-direction: column;
                height: 100% !important;
                margin: 0;
                min-width: 0;
                min-height: 0;
            }
            #roadmap-layout .roadmap-panel .box-header { flex: 0 0 auto; }
            #roadmap-layout .roadmap-timeline .box-header {
                padding: var(--panel-inset) var(--panel-inset) 0;
            }
            #roadmap-layout .roadmap-timeline .box-header .box-title {
                margin: 0;
                font-size: 27px !important; /* 50% larger than AdminLTE 18px */
                font-weight: 700 !important;
                line-height: 1.3;
                white-space: normal;
            }
            #roadmap-layout .roadmap-timeline .box-header .box-title #name {
                font-size: inherit;
                font-weight: inherit;
            }
            #roadmap-layout .roadmap-panel .box-body {
                flex: 1 1 auto;
                min-height: 0;
                min-width: 0;
            }
            #roadmap-layout .roadmap-timeline .box-body {
                display: flex;
                flex-direction: column;
                padding: var(--panel-inset);
            }
            #roadmap-layout #timeline {
                flex: 1 1 auto;
                height: 100% !important;
                width: 100% !important;
                min-height: 0;
            }
            #roadmap-layout .roadmap-data .box-body {
                display: flex;
                flex-direction: column;
                overflow: hidden;
                padding: var(--panel-inset);
            }
            #roadmap-layout .roadmap-data #timeline_data {
                flex: 1 1 0;
                width: 100%;
                min-height: 0;
                min-width: 0;
            }
            #roadmap-layout .roadmap-data .dataTables_wrapper {
                display: flex;
                flex-direction: column;
                height: 100%;
                min-height: 0;
                min-width: 0;
                overflow: hidden;
            }
            #roadmap-layout .roadmap-data .dataTables_scroll {
                flex: 1 1 0;
                display: flex;
                flex-direction: column;
                min-height: 0;
                min-width: 0;
                overflow: hidden;
            }
            #roadmap-layout .roadmap-data .dataTables_scrollHead {
                flex: 0 0 auto;
                min-height: 0;
            }
            #roadmap-layout .roadmap-data .dataTables_scrollBody {
                flex: 1 1 0;
                min-height: 0;
                height: 0 !important;
                max-height: none !important;
                overflow-y: auto !important;
                overflow-x: hidden !important;
            }
            #roadmap-layout .roadmap-dt-toolbar {
                display: flex;
                align-items: center;
                flex-wrap: wrap;
                gap: 10px;
                flex: 0 0 auto;
                margin-bottom: 10px;
            }
            #roadmap-layout .roadmap-dt-toolbar .dt-buttons,
            #roadmap-layout .roadmap-dt-toolbar .dataTables_filter {
                float: none !important;
                margin: 0 !important;
            }
            #roadmap-layout .roadmap-dt-toolbar .dataTables_filter {
                margin-left: auto !important;
            }
            #roadmap-layout .roadmap-dt-toolbar label { margin-bottom: 0; }
            #roadmap-layout .roadmap-dt-footer {
                display: flex;
                align-items: center;
                justify-content: flex-start;
                flex-wrap: wrap;
                flex: 0 0 auto;
                gap: 10px;
                margin-top: auto;
                padding-top: 10px;
            }
            #roadmap-layout .roadmap-dt-footer .dataTables_info {
                float: none !important;
                padding-top: 0;
            }
            #roadmap-layout .roadmap-about #about_ui { height: 100%; }
            #roadmap-layout .roadmap-about #about_ui > .col-sm-12 {
                float: none;
                width: 100%;
                height: 100%;
                padding: 0;
            }
            #roadmap-layout .roadmap-about .nav-tabs-custom {
                box-sizing: border-box;
                padding: var(--panel-inset);
                display: flex;
                flex-direction: column;
                height: 100%;
                min-height: 0;
                margin: 0;
            }
            #roadmap-layout .roadmap-about .nav-tabs-custom > .nav-tabs {
                flex: 0 0 auto;
                display: flex;
                flex-wrap: wrap;
            }
            #roadmap-layout .roadmap-about .nav-tabs-custom > .nav-tabs > li {
                float: none;
            }
            #roadmap-layout .roadmap-about .nav-tabs-custom > .tab-content {
                flex: 1 1 auto;
                min-height: 0;
                margin-top: var(--panel-inset);
                padding: 0;
                overflow-y: auto;
            }
            #roadmap-layout .roadmap-about .nav-tabs-custom > .tab-content h4:first-child {
                margin-top: 0;
            }

            @media (max-width: 767px) {
                .skin-blue .main-sidebar,
                .skin-blue .left-side {
                    position: relative !important;
                    width: 100% !important;
                    height: auto !important;
                    transform: none !important;
                }
                .main-sidebar > .sidebar {
                    display: block;
                    height: auto !important;
                    overflow: visible;
                }
                #sidebar { overflow: visible; }
                .skin-blue .main-sidebar .sidebar-footer {
                    padding-bottom: 24px;
                }
                .content-wrapper, .right-side {
                    margin-left: 0 !important;
                }
            }
            @media (max-width: 900px) {
                .content-wrapper > .content {
                    height: auto;
                    min-height: 0;
                    overflow: visible;
                }
                #roadmap-layout {
                    display: flex;
                    flex-direction: column;
                    height: auto;
                }
                #roadmap-layout .roadmap-timeline { height: 46vh; min-height: 320px; }
                #roadmap-layout .roadmap-bottom { grid-template-columns: minmax(0, 1fr); }
                #roadmap-layout .roadmap-data,
                #roadmap-layout .roadmap-about { height: 420px; }
            }



            #roadmap-layout.screenshot-mode {
                grid-template-rows: minmax(0, 1fr);
                gap: 0;
            }
            #roadmap-layout.screenshot-mode .roadmap-bottom {
                display: none !important;
            }
            @media (max-width: 900px) {
                #roadmap-layout.screenshot-mode {
                    display: grid;
                    height: calc(100dvh - 30px);
                    min-height: 300px;
                    grid-template-rows: minmax(0, 1fr);
                }
                #roadmap-layout.screenshot-mode .roadmap-timeline {
                    height: 100% !important;
                    min-height: 0 !important;
                }
            }

            #roadmap-layout #timeline .vis-item .vis-item-overflow,
            #roadmap-layout #timeline .vis-item .vis-item-content {
                overflow: visible !important;
            }
            #roadmap-layout #timeline .vis-item .vis-item-content {
                white-space: nowrap;
            }

            #timeline .vis-current-time,
            #timeline .vis-custom-time {
                background-color: var(--gsu-red-accent) !important;
            }
            #timeline .vis-item.vis-background {
                background-color: var(--gsu-medium-gray) !important;
                border-color: transparent !important;
            }
            #timeline .vis-item.vis-range:not(.vis-selected) {
                background-color: transparent !important;
                border-color: transparent !important;
                border-bottom: 5px solid var(--gsu-light-blue) !important;
            }
            #timeline .vis-item.vis-line,
            #timeline .vis-item.vis-dot {
                border-color: var(--gsu-red-accent) !important;
            }
            #timeline .vis-item.vis-box:not(.vis-selected),
            #timeline .vis-item.vis-point:not(.vis-selected) {
                background-color: transparent !important;
                border-color: var(--gsu-red-accent) !important;
            }
            #timeline .vis-item.vis-range.vis-selected,
            #timeline .vis-item.vis-box.vis-selected,
            #timeline .vis-item.vis-point.vis-selected {
                background-color: var(--gsu-light-gray) !important;
                border: 1px solid var(--gsu-red-accent) !important;
                border-bottom: 5px solid var(--gsu-red-accent) !important;
            }
            #timeline .vis-item.vis-selected .vis-delete,
            #timeline .vis-item.vis-selected .vis-delete-rtl {
                background-image: none !important;
                color: var(--gsu-red-accent) !important;
                font-size: 0;
            }
            #timeline .vis-item.vis-selected .vis-delete::after,
            #timeline .vis-item.vis-selected .vis-delete-rtl::after {
                content: "X";
                display: block;
                color: var(--gsu-red-accent);
                font-family: Arial, sans-serif;
                font-weight: 700;
                font-size: 17px;
                line-height: 18px;
                text-align: center;
                pointer-events: none;
            }
            #roadmap-layout .roadmap-data table.dataTable tbody tr.selected > td,
            #roadmap-layout .roadmap-data table.dataTable tbody tr.selected > th,
            #roadmap-layout .roadmap-data table.dataTable tbody tr.selected > * {
                background-color: var(--gsu-vibrant-blue) !important;
                color: #fff !important;
                box-shadow: none !important;
            }
            #roadmap-layout .roadmap-data table.dataTable tbody tr:hover:not(.selected) > td {
                background-color: rgba(0,174,239,.12) !important;
            }
            #roadmap-layout .roadmap-about .nav-tabs-custom > .nav-tabs > li.active {
                border-top-color: var(--gsu-blue) !important;
            }
            #roadmap-layout .roadmap-about .nav-tabs-custom > .nav-tabs > li.active > a {
                color: var(--gsu-blue) !important;
            }
        '))),
        div(id = "roadmap-layout",
                        div(class = "roadmap-panel roadmap-timeline",
                            box(title = textOutput("name"),
                                width = NULL,
                                height = "100%",
                                timevisOutput(outputId = "timeline", height = "100%"))),
                        div(class = "roadmap-bottom",
                            div(class = "roadmap-panel roadmap-data",
                                box(width = NULL,
                                    height = "100%",
                                    DTOutput(outputId = "timeline_data"))),
                            div(class = "roadmap-panel roadmap-about",
                                uiOutput(outputId = "about_ui"))))))


server <- function(input, output, session) {
    
    output$name <- renderText({paste0(input$name, "'s Grad School Roadmap")})
    
    make_items <- function(entry_date) {
        guides %>%
            mutate(month = as.integer(month - 23),
                   entry = floor_date(ymd(entry_date) %m+% months(1), unit = "month"),
                   end = entry + months(month),
                   start = end - months(as.integer(duration)),
                   content = event) %>%
            select(id, content, start, end, group, type)
    }

    timeline_items <- reactive({
        req(input$entry)
        make_items(input$entry)
    })

    initial_items <- function(entry_date) {
        make_items(entry_date) %>%
            filter(group %in% c("apps", default_groups)) %>%
            mutate(style = "word-wrap: normal;",
                   content = gsub("<.*>", "", content))
    }

    output$timeline <- renderTimevis({
        req(input$entry)
        timevis(data = initial_items(input$entry),
                options = list(editable = list(add = FALSE,
                                               remove = TRUE,
                                               updateTime = TRUE,
                                               updateGroup = TRUE,
                                               overrideItems = TRUE),
                               max = input$entry + months(12),
                               min = input$entry - months(30),
                               multiselect = TRUE,
                               verticalScroll = TRUE,
                               height = "100%",
                               autoResize = TRUE))
    })

    observeEvent(input$action1, {
        addItem("timeline", list(content = input$event1, start = input$range1[1],
                                 type = "point", style = ""))
    })

    observeEvent(input$action2, {
        addItem("timeline", list(content = input$event2, start = input$range2[1],
                                 end = input$range2[2], type = "range", style = ""))
    })

    observeEvent(input$addgroups, {
        if (is.null(input$timeline_ids)) return(invisible(NULL))
        selected <- input$groups
        if (is.null(selected) || length(selected) == 0) return(invisible(NULL))

        existing_ids <- as.character(input$timeline_ids)
        additions <- timeline_items() %>%
            filter(group %in% selected,
                   !(as.character(id) %in% existing_ids))
        if (nrow(additions) > 0) addItems(id = "timeline", data = additions)
    })

    observeEvent(input$rmgroups, {
        items <- input$timeline_data
        if (is.data.frame(items) && nrow(items) > 0 &&
            all(c("id", "group") %in% names(items))) {
            removable <- items$id[!is.na(items$group) &
                                      items$group %in% c("prep", "recs", "exam", "exer", "rsch")]
            if (length(removable) > 0) {
                removeItem(id = "timeline", itemId = as.list(removable))
            }
        }
        updateCheckboxGroupInput(session, inputId = "groups", selected = character(0))
    })

    observeEvent(input$newtimeline, {
        entry_date <- default_entry_date()
        updateTextInput(session, "name", value = "Aspiring Student")
        updateDateInput(session, "entry", value = entry_date)
        updateCheckboxGroupInput(session, "groups", selected = default_groups)
        updateTextInput(session, "event1", value = "")
        updateTextInput(session, "event2", value = "")
        suppressWarnings({
            updateDateInput(session, "range1", value = as.Date(NA))
            updateDateRangeInput(session, "range2",
                                 start = as.Date(NA), end = as.Date(NA))
        })

        setItems(id = "timeline", data = initial_items(entry_date))
        setSelection(id = "timeline", itemId = character(0),
                     options = list(animation = FALSE))
        fitWindow(id = "timeline", options = list(animation = FALSE))
        shinyjs::removeCssClass(id = "roadmap-layout", class = "screenshot-mode")
    })

    # OUTPUT TIMEVIS TABLE
    
    output$timeline_data <- renderDT({
        req(input$timeline_data)
        datatable(input$timeline_data %>%
                                                    rename(Event = content,
                                                           Begin = start,
                                                           End = end) %>%
                                                    arrange(Begin) %>%
                                                    mutate(Event = gsub('<[biu]{1}>|</[biu]{1}>|</font>|<font size = "[0-9]{1,}[\\.]{1}[0-9]{1,}" color = "#[A-Z0-9]{6}">', "", Event),
                                                           Order = seq_len(n()),
                                                           Begin = format.Date(gsub("T.*$", "", Begin), "(%Y) %B"),
                                                           End = format.Date(gsub("T.*$", "", End), "(%Y) %B")) %>%
                                                    select(Order, Event, Begin, End),
                                                extensions = "Buttons",
                                                options = list(dom = '<"roadmap-dt-toolbar"Bf>t<"roadmap-dt-footer"i>',
                                                               buttons = list("copy",
                                                                              "print",
                                                                              list(extend = "collection",
                                                                                   buttons = c("csv", "excel", "pdf"),
                                                                                   text = "Download")),
                                                               paging = FALSE,
                                                               scrollY = "200px",
                                                               scrollCollapse = FALSE,
                                                               initComplete = JS(
                                                                   "function(settings) {",
                                                                   "  var api = this.api();",
                                                                   "  var wrapper = api.table().container();",
                                                                   "  var panel = wrapper.closest('.roadmap-data');",
                                                                   "  if (!panel || !window.ResizeObserver) return;",
                                                                   "  var queued = false;",
                                                                   "  var observer = new ResizeObserver(function() {",
                                                                   "    if (!document.documentElement.contains(wrapper)) { observer.disconnect(); return; }",
                                                                   "    if (queued) return;",
                                                                   "    queued = true;",
                                                                   "    requestAnimationFrame(function() {",
                                                                   "      queued = false;",
                                                                   "      api.columns.adjust();",
                                                                   "    });",
                                                                   "  });",
                                                                   "  observer.observe(panel);",
                                                                   "  $(api.table().node()).one('destroy.dt', function() { observer.disconnect(); });",
                                                                   "}")),
                                                rownames = FALSE,
                                                editable = FALSE)}, server = FALSE)
    
    output$about_ui <- renderUI({tabBox(width = 12, 
                                         id = "about_tab1",
                                         tabPanel(title = "About", icon = icon("info-circle"), value = "about_tab2",
                                                  HTML("<h4>Your Journey to Graduate School</h4>
                                              The <b>Graduate Prep Roadmap App</b> is an interactive platform
                                              for setting goals and planning ahead on your road to entering 
                                              graduate school. This tool is brought to you by the 
                                              <i>Center for the Advancement of Students and Alumni (CASA)</i>
                                              at <i>The Graduate School</i> at <i>Georgia State University</i>.
                                              <br><br>
                                              The <b>Roadmap App</b> has a series of recommended activities and 
                                              events to prepare for graduate school, including:
                                              <br><br>
                                              <ul>
                                              <li>Developing essential skills for graduate school</li>
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
                                              needs. Because preparation needs vary by program and each student's stage of progress,
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
                                              The <b>Roadmap Events Table</b> updates dynamically as the <b>Timeline</b> changes. 
                                              Sort events using the column arrows, and scroll through the
                                              full event list. Use the top buttons for different ways to export the table."), 
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
                                              product is an open-source application licensed under the 
                                              <b><a href='https://www.r-project.org/Licenses/LGPL-3'>GNU Lesser 
                                              General Public License 3 (LGPL-3)</a></b>
                                              and the source code and additional documentation may be found in
                                              <b><a href='https://github.com/jamisoncrawford/timeline'>this GitHub repository</a></b>.
                                              <br><br>
                                              <h4>About the Developer</h4>
                                              <b>Jamison Crawford, MPA</b> is an institutional researcher for <i>The Graduate School</i>
                                              and <i>Center for the Advancement of Students & Alumni (CASA)</i> as well as a professor of 
                                              data science at <i>Georgia State University</i> and <i>Arizona State University</i>.
                                              <b><a href='mailto:jcrawford52@gsu.edu'>Contact</a></b>.
                                              <br><br>
                                              <h4>Contributors</h4>
                                              Special thanks to <b>Kyle Frantz, Ph.D.</b> and <b>Sarah Clark, Ph.D.</b> of 
                                              <i>Georgia State University</i> for originally conceptualizing this application and 
                                              developing recommended graduate school preparation activities, respectively."), 
                                                  verbatimTextOutput("tab4")))})
    
    observeEvent(input$screenshot, {
        shinyjs::toggleCssClass(id = "roadmap-layout", class = "screenshot-mode")
        shinyjs::runjs("window.dispatchEvent(new Event('resize'));")
    })
}

shinyApp(ui = ui, 
         server = server)
