library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)

# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Nome de usuário", label = tagList(icon("user"), "Nome de usuário")),
                   passwordInput("passwd", placeholder="Senha", label = tagList(icon("unlock-alt"), "Senha")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "ENTRAR", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Nome de usuário ou senha incorretos!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Username: admin, prof, gest, equi"),
                     br(),
                     tags$code("Password: admin, prof, gest, equi")
                   ))
)

credentials = data.frame(
  #NOMES DE USUÁRIO (LINKAR COM O RSQLITE)
  username_id = c("admin", "prof", "gest", "equi"),
  
  #SENHAS (LINKAR COM O RSQLITE)
  passod   = sapply(c("admin", "prof", "gest", "equi"),password_store),
  
  #SENHAS (CRIAR UM CAMPO EXTRA PRA LINKAR NO SQL)
  permission  = c("advanced", "prof", "gest", "equi"), 
  stringsAsFactors = F
)

header <- dashboardHeader( title = "Simple Dashboard", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    
    if (USER$login == TRUE ){ 
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
        sidebarMenu(
          menuItem("Pagina principal", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("About Page", tabName = "About", icon = icon("th"))
        )
      }
      else{
        if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="prof") {
          sidebarMenu(
            menuItem("Profissional", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("About Page", tabName = "About", icon = icon("th"))
          )
        }
        else{
          if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="gest") {
            sidebarMenu(
              menuItem("Gestante", tabName = "dashboard", icon = icon("dashboard")),
              menuItem("About Page", tabName = "About", icon = icon("th"))
            )
          }
          else{
            if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="equi") {
              sidebarMenu(
                menuItem("Equipe", tabName = "dashboard", icon = icon("dashboard")),
                menuItem("About Page", tabName = "About", icon = icon("th"))
              )
            }
          }
        }
      }
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
        tabItems(
          tabItem(
            tabName ="dashboard", class = "active",
            fluidRow(
              box(width = 12, dataTableOutput('results'))
            )
          )
          ,
          tabItem(
            tabName ="About",
            h2("Essa é a pagina do Administrador")
          )
        )
      } 
      else {
        if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="prof") {
          tabItems(
            tabItem(
              tabName ="dashboard", class = "active",
              fluidRow(
                box(width = 12, dataTableOutput('results'))
              ))
            ,
            tabItem(
              tabName ="About",
              h2("Essa é a pagina do Profissional")
            )
          )
        }
        else{
          if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="gest") {
            tabItems(
              tabItem(
                tabName ="dashboard", class = "active",
                fluidRow(
                  box(width = 12, dataTableOutput('results'))
                ))
              ,
              tabItem(
                tabName ="About",
                h2("Essa é a pagina da Gestante")
              )
            )
          }
          else{
            if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="equi") {
              tabItems(
                tabItem(
                  tabName ="dashboard", class = "active",
                  fluidRow(
                    box(width = 12, dataTableOutput('results'))
                  )
                  )
                ,
                tabItem(
                  tabName ="About",
                  h2("Essa é a pagina da Equipe")
                )
              )
            }
            else{
              loginpage
            }
          }
        }
      }
    }
    else{
      loginpage
    }
  })
  
  output$results <-  DT::renderDataTable({
    datatable(iris, options = list(autoWidth = TRUE,
                                   searching = FALSE))
  })
  
  output$results2 <-  DT::renderDataTable({
    datatable(mtcars, options = list(autoWidth = TRUE,
                                     searching = FALSE))
  })
  
  
}

runApp(list(ui = ui, server = server), launch.browser = TRUE)