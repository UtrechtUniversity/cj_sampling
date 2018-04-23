# Copyright statement:
# This shiny apllication is developed by Duco Veen to be used for educational purposes.
# Is is part of a program sponsered by the Education Incentive Funds of Utrecht University. 
# The lay-out for the shiny applications for this program is developed by Kimberley Lek. 
# The application is licensed under the ?? GNU General Public License V3.0 - decision on this? ?? 

# Author Comment:
# I have tried to code this according to the Google R Style Guide to improve readibility:
# https://google.github.io/styleguide/Rguide.xml
# For any quenstions or comments you can contact me at d.veen@uu.nl.

# File description:
# This file contains the user interface (UI) for the application related to correlation.

# Loading library's 
library(shiny)
library(shinydashboard)

# user interface design
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Correlational statistics",titleWidth = 350), 
                    dashboardSidebar(width = 350,
                                     sidebarMenu(#menuItem("", tabName = "home", icon = icon("home")),
                                       menuItem("Select sample", tabName = "tab1"),
                                       menuItem("Disclaimer", tabName = "Disclaimer"), 
                                       HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br>"), 
                                       img(src= "logo.png", align = "left")
                                     ) #end sidebarMenu
                    ), #end dashboardsidebar
                    dashboardBody(
                      # CSS styles lay-out settings from Kimberley
                      tags$style(HTML(".irs-bar {background: #EAC626}")),
                      tags$style(HTML(".irs-bar {border-top: 1px solid black}")),
                      tags$style(HTML(".irs-bar-edge {background: #EAC626}")),
                      tags$style(HTML(".irs-bar-edge {border: 1px solid black}")),
                      tags$style(HTML(".irs-single {background: #EAC626}")),
                      tags$style(HTML(".selectize-input {border-color: #EAC626}")),
                      tags$style(HTML(".selectize-dropdown {border-color: #EAC626}")),
                      tags$head(tags$style(HTML('.skin-black .main-header .logo {
                                                background-color: #EAC626;
                                                } .skin-black .main-header .logo:hover {
                                                background-color: #EAC626;
                                                }
                                                /* active selected tab in the sidebarmenu */
                                                .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                                                background-color: #EAC626;
                                                }
                                                /* navbar (rest of the header) */
                                                .skin-black .main-header .navbar {
                                                background-color: #EAC626;
                                                }
                                                /* toggle button when hovered  */                    
                                                .skin-black .main-header .navbar .sidebar-toggle:hover{
                                                background-color: #EAC626;
                                                }
                                                /* other links in the sidebarmenu when hovered */
                                                .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
                                                background-color: #EAC626;
                                                }
                                                /* other links in the sidebarmenu */
                                                .skin-black .main-sidebar .sidebar .sidebar-menu a{
                                                background-color: #EAC626;
                                                color: #000000;
                                                }
                                                /* active selected tab in the sidebarmenu */
                                                .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                                                background-color: #000000;
                                                color: #FFFFFF;
                                                }
                                                .skin-black .main-sidebar {color: #000000; background-color: #EAC626;}
                                                ') # end html
                      ) # end tags$style
                      ), # end tags$head
                      tabItems(
                        # now for the content of the different tabs.
                        # Disclaimer tab, university disclaimer.
                        tabItem(tabName = "Disclaimer", 
                                box(width = 12, 
                                    h5("Terms of Usage Utrecht Unversity Shiny Server", 
                                       br(), 
                                       br(),
                                       tags$ul(
                                         tags$li("Purpose of the service “utrecht-university.shinyapps.io” 
                                                 is to provide a digital place for trying out, evaluating 
                                                 and/or comparing methods developed by researchers of Utrecht University 
                                                 for the scientific community worldwide. 
                                                 The app and its contents may not be preserved in such a way that it can 
                                                 be cited or can be referenced to. "), 
                                         tags$li("The web application is provided ‘as is’ and ‘as available’ and is without any warranty. 
                                                 Your use of this web application is solely at your own risk."), 
                                         tags$li("	You must ensure that you are lawfully entitled and have full authority to upload 
                                                 data in the web application. The file data must not contain any data which can raise 
                                                 issues relating to abuse, confidentiality, privacy,  data protection, licensing, and/or 
                                                 intellectual property. You shall not upload data with any confidential or proprietary information 
                                                 that you desire or are required to keep secret. "),
                                         tags$li("By using this app you agree to be bound by the above terms.")
                                         ) # end tags$ul
                                         ), # end h5
                                    h5("Development and questions",
                                       br(),
                                       br(),
                                       tags$ul(
                                         tags$li("Utrecht University lay-out developed by Kimberley Lek (k.m.lek@uu.nl)."),
                                         tags$li("This application is developed by Duco Veen (d.veen@uu.nl).")
                                       ) # end tags$ul
                                    ) # end h5
                                       ) # end box
                                       ), # end tab Item (disclaimer tab.)
                        
                        tabItem(tabName = "tab1",
                                fluidRow(
                                  box(
                                    width = 12,
                                    align = "left",
                                    h4("Sampling from a population"),
                                    h5(
                                      "The colorful objects in the plot represent cases in the population. The researcher is located somewhere in the population. The closer cases are to the researcher, the easier they are to reach. Select one of the different sampling methods to see which cases would be included."
                                    ),
                                    h5(
                                      "To see how well the selected sample captures the population as a whole, click on the tab 'Descriptives'"
                                    ),
                                    # explanation what students should do
                                    plotOutput(
                                      "plot1",
                                      width = 600,
                                      height = 400#,
                                      #click = "plot_click"
                                    )
                                  ),
                                  box(
                                    numericInput("n", "Sample size:", 20, min = 5, max = 100, step = 5, width = 200),
                                    selectInput("sample_type", "Type of sample:", c("Convenience", "Snowball", "Purposive"), selectize = TRUE, width = 200),
                                    actionButton("do_sample", "Draw sample"),
                                    actionButton("reset", "Reset population"),
                                    width = 12,
                                    align = "left"#,
                                    # box for undo and reset buttons
                                    #actionButton("Undo", "Undo"),
                                    # action button that allows the previous point to be deleted
                                    #actionButton("reset", "Reset"), # action button to restart with clean plot,
                                    #tabsetPanel(type = "tabs",
                                    #            tabPanel("Plot", plotOutput("plot")),
                                    #            tabPanel("Summary", verbatimTextOutput("summary")),
                                    #            tabPanel("Table", tableOutput("table"))
                                    #             )
                                  ) # end box
                                ) # end fluidrow)
                        )
                        
                                ) # end tabItems
                      ) # end dashboardbody
                      ) # end dashboardpage


  

# BLABLABLA
