library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(billboarder)
library(RSQLite)
library(DBI)

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
                     tags$code("Username: admin, prof, gest, equi, gestor"),
                     br(),
                     tags$code("Password: admin, prof, gest, equi, gestor"), 
                     br(),
                     tags$code("match them")
                   ))
)

#Cria uma conexão com o banco de dados
#Essa conexão vai ser usada para todas as consultas no banco de dados
#Alterar o caminho para o servidor
dados_bd <- dbConnect(RSQLite::SQLite(), "D:/app-1/app/teste-funcoes/teste.db")
#Seleciona os dados de login da tabela usuario
credentials <- dbGetQuery(dados_bd, "SELECT telefone AS username_id, senha AS passod, email, `tipo-usuario` AS permission FROM usuario", stringsAsFactors = F)
#Aplica um hash na coluna das senhas
credentials$passod <- sapply(credentials$passod, password_store)



header <- dashboardHeader( title = "Painel Lateral", uiOutput("logoutbtn"))

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
    dbDisconnect(dados_login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  
  #Renderiza a barra lateral e mostra as abas permitidas do usuário

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
            else{
              if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="gestor") {
                sidebarMenu(
                  menuItem("Gestante", tabName = "Gestante", icon = icon("person-pregnant")),
                  menuItem("Profissional", tabName = "Profissional", icon =icon("user-doctor"))
                )
              }
            }
          }
        }
      }
    }
  })
  
  #dados
  #Adicionar externamente para não quebrar o Shiny
  data("gestantes")
  gestantes_idade <- dbGetQuery(dados_bd, "SELECT `respostas-questionario-inicial-triagem-gestante`.`a9-idade-em-anos-completos` AS `a9-idade-em-anos-completos`, count(*) AS `count`
                                           FROM `respostas-questionario-inicial-triagem-gestante`
                                           WHERE ((`respostas-questionario-inicial-triagem-gestante`.`a9-idade-em-anos-completos` <> 15
                                           OR `respostas-questionario-inicial-triagem-gestante`.`a9-idade-em-anos-completos` IS NULL)
                                           AND (`respostas-questionario-inicial-triagem-gestante`.`a9-idade-em-anos-completos` <> 16 OR `respostas-questionario-inicial-triagem-gestante`.`a9-idade-em-anos-completos` IS NULL) AND (`respostas-questionario-inicial-triagem-gestante`.`a9-idade-em-anos-completos` <> 17 OR `respostas-questionario-inicial-triagem-gestante`.`a9-idade-em-anos-completos` IS NULL) AND `respostas-questionario-inicial-triagem-gestante`.`a14a-tem-restricao-medica` = 'Não' AND `respostas-questionario-inicial-triagem-gestante`.`a17-concordou-em-participar-e-assinou-tcle` = 'Sim')
                                           GROUP BY `respostas-questionario-inicial-triagem-gestante`.`a9-idade-em-anos-completos`
                                          ORDER BY `respostas-questionario-inicial-triagem-gestante`.`a9-idade-em-anos-completos` ASC")
  
  gestantes_semanas <-dbGetQuery(dados_bd, "SELECT `respostas-questionario-inicial-triagem-gestante`.`a10-semana-gestacional` AS `a10-semana-gestacional`, count(*) AS `count`
                                            FROM `respostas-questionario-inicial-triagem-gestante`
                                            GROUP BY `respostas-questionario-inicial-triagem-gestante`.`a10-semana-gestacional`
                                            ORDER BY `respostas-questionario-inicial-triagem-gestante`.`a10-semana-gestacional` ASC")
  
  gestantes_consultas_prenatal <- dbGetQuery(dados_bd, "SELECT `respostas-questionario-inicial-triagem-gestante`.`a11-consulta-pre-natal-numero` AS `a11-consulta-pre-natal-numero`, count(*) AS `count`
                                                        FROM `respostas-questionario-inicial-triagem-gestante`
                                                        GROUP BY `respostas-questionario-inicial-triagem-gestante`.`a11-consulta-pre-natal-numero`
                                                        ORDER BY `respostas-questionario-inicial-triagem-gestante`.`a11-consulta-pre-natal-numero` ASC")
  
  gestantes_entrevistadora <-dbGetQuery(dados_bd, "SELECT `respostas-questionario-inicial-triagem-gestante`.`a1-entrevistador` AS `a1-entrevistador`, count(*) AS `count`
                                                   FROM `respostas-questionario-inicial-triagem-gestante`
                                                   GROUP BY `respostas-questionario-inicial-triagem-gestante`.`a1-entrevistador`
                                                   ORDER BY `count` DESC, `respostas-questionario-inicial-triagem-gestante`.`a1-entrevistador` ASC")
  #fim dos dados
  
  
  #Renderiza as abas permitidas do usuário
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      #Credencial de administrador
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
        #tabItens renderiza os itens (tabItem) das abas
        tabItems(
          tabItem(
            tabName ="dashboard", class = "active",
            fluidRow(
              box(width = 12, dataTableOutput('results'))
            )
          ),
          tabItem(
            tabName ="About",
            h2("Essa é a pagina do Administrador")
          )
        )
      } 
      else {
        #Credencial de Profissional
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
          #Credencial de Gestante
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
            #Credencial de equipe
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
              #Credencial de gestor
              if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="gestor") {
                tabItems(
                  
                  tabItem(
                    tabName ="Gestante", class ="active",
                    fluidRow(
                      # pie chart Gestantes por idade
                      billboarder() %>% 
                        bb_piechart(data = gestantes_idade) %>% 
                        bb_labs(title = "Gestantes por idade",
                                caption = "")
                      ),
                    fluidRow(
                      # pie chart Gestantes por semana de gestação
                      billboarder() %>% 
                        bb_piechart(data = gestantes_semanas) %>% 
                        bb_labs(title = "Gestantes por semanas de gestação",
                                caption = "")
                    ),
                    fluidRow(
                      # pie chart Gestantes por número de consultas pré natal
                      billboarder() %>% 
                        bb_piechart(data = gestantes_consultas_prenatal) %>% 
                        bb_labs(title = "Gestantes por número de consultas pré natal",
                                caption = "")
                    ),
                    fluidRow(
                      # pie chart Percentual de gestantes por entrevistadora
                      billboarder() %>% 
                        bb_piechart(data = gestantes_entrevistadora) %>% 
                        bb_labs(title = "Percentual de gestantes por entrevistadora",
                                caption = "")
                    )
                    ),
                  
                    tabItem(
                      tabName = "Profissional",
                      fluidRow(
                        billboarder() %>% 
                          bb_piechart(data = gestantes) %>% 
                          bb_labs(title ="Dados de profissional",
                                  caption ="Data source: RTE (https://opendata.rte-france.com)")
                        )
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
    }
    else{
      loginpage
    }
  })

  #  
  output$results <-  DT::renderDataTable({
    datatable(iris, options = list(autoWidth = TRUE,
                                   searching = FALSE))
  })
  
  output$results2 <-  DT::renderDataTable({
    datatable(mtcars, options = list(autoWidth = TRUE,
                                     searching = FALSE))
  })
  
  
}

runApp(list(ui = ui, server = server))