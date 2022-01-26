source("code_finalee.R")
library(shiny)
library(shinydashboard)
library(rmarkdown)
library(knitr)

# Define UI for application that draws a histogram

ui <- dashboardPage(
  
  skin="purple",
  
  dashboardHeader(title = "Big Data Analytics"),
  
  dashboardSidebar(
    
    width=230,
    
    sidebarMenu(
      
      menuItem("About",
               tabName="metodo"),
      
      menuItem("Estimation",
               tabName="building"),
      
      menuItem("Performance",
               tabName="perf"),
      
      menuItem("Simulations",
               tabName="demo")
      
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName="metodo",
              includeMarkdown("Introduction.Rmd"),
              includeCSS("style.css")
      ),
      tabItem(tabName="building",
              tabsetPanel(
                
                tabPanel("Méthodes Régressionnellles",
                         includeMarkdown("Regressions.Rmd"),
                         includeCSS("style.css")
                ),
                tabPanel("Méthodes d'aggrégation",
                         includeMarkdown("Aggregation.Rmd"),
                         includeCSS("style.css")
                )
              )
      ),
      
      tabItem(tabName="perf",
              
              box(
                title="Comparaison des modèles",
                tableOutput("table"),
              ),
              
              box(
                title="Critère d'Evaluation",
                selectInput("metric","Choisissez une métrique de performance",
                            c("AUC","PCC","KS","F_Score","Log_Loss")),
                htmlOutput("eval"),
              )
              
      ),
      tabItem(tabName="demo",
              tabsetPanel(
               
                tabPanel("Random Forest",
                         
                         fluidRow(
                           
                           box(title="Hyperparamètres",
                               sliderInput("ntree","Nombre d'arbres",min=50,max=100,value=50),
                               sliderInput("mtry","Nombre de predicteurs",min=1,max=12,value=2,step=1),
                               sliderInput("nodesize","Nombre minimal d'exemples dans chaque feuille",min=50,max=100,value=5),
                               actionButton("go","Veuillez cliquer pour
                                      obtenir les résultats")
                           )
                         ),
                         fluidRow(
                           box(
                             title="Résultats d'estimation",
                             plotOutput("rf_plot"),
                             plotOutput("imp_plot"),
                           ),
                           box(title="Performance prédictive globale",
                               plotOutput("Auc_rf"),
                               plotOutput("PR_rf")
                           )
                         )
                ),
                tabPanel("Gradient Boosting",
                         
                         fluidRow(
                           
                           box(title="Hyperparamètres",
                               sliderInput("n.trees","Nombre d'arbres",min=50,max=150,value=50),
                               sliderInput("interaction.depth","Profondeur maximal de chaque arbre",min=1,max=4,value=3,step=1),
                               sliderInput("shrinkage","Taux d'apprentissage",min=0.001,max=1,value=0.1),
                               sliderInput("n.minobsinnode","Nombre minimal d'exemples dans chaque feuille",
                                           min=50,max=100,value=70),
                               actionButton("allez","Veuillez cliquer pour
                                      obtenir les résultats")
                           )
                         ),
                         
                         fluidRow(
                           
                           box(
                             title="Importance relative des variables",
                             plotOutput("imp_gbm")
                           ),
                           
                           box(title="Performance prÃ©dictive globale",
                               plotOutput("Auc_gbm"),
                               plotOutput("PR_gbm")
                           )
                           
                         )
                         
                         
                         
                         
                ),
                tabPanel("Adaptive Ridge",
                         
                         fluidRow(
                           
                           sliderInput("ridge","Paramètre de régularisation de la régression Ridge avec les poids",
                                       min=0,max=5,value=0.01,step=0.01),
                           actionButton("pame_ridge","Veuillez cliquer pour
                                      obtenir les résultats")
                         ),
                         fluidRow(
                           
                           
                           
                           box(title="Performance prédictive globale",
                               plotOutput("Auc_ridge"),
                               plotOutput("PR_ridge")
                               
                           )
                           
                         )
                ),
                tabPanel("Adaptive Lasso",
                         
                         fluidRow(
                           
                           sliderInput("lasso","Paramètre de régularisation de la régression lasso avec les poids",
                                       min=0,max=0.07,value=0.01,step=0.01),
                           actionButton("pame","Veuillez cliquer pour
                                      obtenir les résultats")
                         ),
                         fluidRow(
                           
                           
                           
                           box(title="Performance prédictive globale",
                               plotOutput("Auc_lasso"),
                               plotOutput("PR_lasso")
                               
                           )
                           
                         )
                ),
                tabPanel("Elastic Net",
                         
                         fluidRow(
                           
                           sliderInput("elastic_reg","Paramètre de régularisation de la régression Elastic Net avec les poids",
                                       min=0,max=0.07,value=0.01,step=0.01),
                           sliderInput("elastic_mel","Paramètre de mélange de l'elastic net (elasticnet mixing parameter)",
                                       min=0.01,max=0.99,value=0.5,step=0.01),
                           actionButton("pame_elastic","Veuillez cliquer pour
                                      obtenir les résultats")
                         ),
                         fluidRow(
                           
                           
                           
                           box(title="Performance prédictive globale",
                               plotOutput("Auc_elastic"),
                               plotOutput("PR_elastic")
                               
                           )
                           
                         )
                )
                
                
                
                
              )
              
      )
      
      
      
      
      
      
    )
  )
  
)


# Define server logic required to draw a histogram
rsconnect::configureApp("KAKOGIANNAKI_SHAO_HAMID", size="xxxlarge")
#options(rsconnect.max.bundle.size=3145728000000000000)
server <- function(input, output) {
  
  output$table=renderTable(tableau[,c("Methodes",input$metric)],digits=3)
  
  Compare=reactive({
    
    if(input$metric=="AUC"){
      
      HTML(paste(" L'AUC permet d'évaluer le pouvoir de discrimination globale du modèle.","Plus elle est grande, meilleur
            est le modèle.","L'AUC la plus élevée est obtenue avec la Random Forest (0.958).",
                 sep="<br/>"))
      
    }else if(input$metric=="PCC"){
      HTML(paste(" La PCC est la proportion d'individus correctement classifiés.","Plus elle est grande, meilleur
            est le modèle.","La PCC la plus élevée est obtenue avec la Random Forest (0.957).","La méthode de
            gradient boosting les autres méthodes utilisées.", sep="<br/>"))
      
    }else if(input$metric=="KS"){
      HTML(paste(" La statistique KS est la distance maximale entre les distributions des scores cumulés des
          individus qui dépassent 90 jours ou plus d'une part, et non d'autre part.","Plus elle est grande, meilleur
            est le modéle.","La statistique KS la plus élevée est obtenue avec la Random Forest (0.803).","La méthode
            de Random Forest surperforme les autres méthodes prédictives.", sep="<br/>"))
      
    }else if(input$metric=="F_Score"){
      HTML(paste(" Le F-Score mesure la performance du modèle dans la classification des individus qui dépassent 90 jours ou plus.",
                 "Plus il est grand, meilleur est le modèle.","Le F-Score le plus élevé est obtenu avec la Random Forest (0.977).",
                 sep="<br/>"))
      
    }else if(input$metric=="Log_Loss"){
      HTML(paste(" La Log Loss est la perte en entropie croisée, généralement utilisée pour évaluer la qualité des probabilités prédites.",
                 "Plus les valeurs obtenues sont faibles, meilleur est la classification.","La log Loss est la plus faible pour la Random Forest (0.135).",
                 "La méthode de Random Forest surperforme les autre méthodes de prédiction",
                 sep="<br/>"))
    }
  })
  
  output$eval=renderUI(Compare())
  
  
  
  
  est_rf=eventReactive(input$go,{
    set.seed(7)
    randomForest(cible~.,data=train,
                 ntree=input$ntree,mtry=input$mtry,nodesize=input$nodesize)
  })
  
  pred_rf=reactive({
    set.seed(7)
    predict(est_rf(),test,type="prob")
  })
  
  output$rf_plot=renderPlot({
    plot(est_rf()$err.rate[, 1], type = "l",
         xlab = "nombre d'arbres", ylab = "erreur OOB",
         main="Evolution du taux d'erreur OOB avec le nombre d'arbres")
  })
  
  output$imp_plot=renderPlot({
    varImpPlot(est_rf(),main="Importance des variables en termes de réduction d'impureté")
  })
  
  output$Auc_rf=renderPlot({
    set.seed(7)
    plot(roc.curve(scores.class0=pred_rf()[,"Yes"],weights.class0=test$cible=="Yes",curve=TRUE))
  })
  
  output$PR_rf=renderPlot({
    set.seed(7)
    plot(pr.curve(scores.class0=pred_rf()[,"Yes"],weights.class0=test$cible=="Yes",curve=TRUE))
  })
  
  est_gbm=eventReactive(input$allez,{
    set.seed(7)
    gbm(as.character(ifelse(cible=="Yes",1,0))~.,
        n.trees=input$n.trees,interaction.depth=input$interaction.depth,shrinkage=input$shrinkage,
        n.minobsinnode=input$n.minobsinnode,data=train,distribution="bernoulli")
  })
  
  pred_gbm=reactive({
    set.seed(7)
    predict(est_gbm(),test,n.trees=input$n.trees,type="response")
  })
  
  output$imp_gbm=renderPlot({
    summary(est_gbm(),cBars = 10,
            method = relative.influence,las=2)
  })
  
  output$Auc_gbm=renderPlot({
    set.seed(7)
    plot(roc.curve(scores.class0=pred_gbm(),weights.class0=test$cible=="Yes",curve=TRUE))
  })
  
  output$PR_gbm=renderPlot({
    set.seed(7)
    plot(pr.curve(scores.class0=pred_gbm(),weights.class0=test$cible=="Yes",curve=TRUE))
  })
  
  ridge=eventReactive(input$pame_ridge,{
    set.seed(7)
    glmnet(x=train_X, y=train_Y,family="binomial",type.measure = "auc", alpha=0)
  })
  pred_ridge=reactive({
    set.seed(7)
    predict(ridge(),newx=test_X,s=input$ridge, type="response")
  })
  output$Auc_ridge=renderPlot({
    set.seed(7)
    plot(roc.curve(scores.class0=pred_ridge(),weights.class0=test_Y=="1",curve=TRUE))
  })
  
  output$PR_ridge=renderPlot({
    set.seed(7)
    plot(pr.curve(scores.class0=pred_ridge(),weights.class0=test_Y=="1",curve=TRUE))
  })
  
  
  lasso=eventReactive(input$pame,{
    set.seed(7)
    glmnet(x=train_X, y=train_Y,family="binomial",type.measure = "auc", alpha=1)
  })
  pred_lasso=reactive({
    set.seed(7)
    predict(lasso(),newx=test_X,s=input$lasso, type="response")
  })
  output$Auc_lasso=renderPlot({
    set.seed(7)
    plot(roc.curve(scores.class0=pred_lasso(),weights.class0=test_Y=="1",curve=TRUE))
  })
  
  output$PR_lasso=renderPlot({
    set.seed(7)
    plot(pr.curve(scores.class0=pred_lasso(),weights.class0=test_Y=="1",curve=TRUE))
  })
  
  elastic=eventReactive(input$pame_elastic,{
    set.seed(7)
    glmnet(x=train_X, y=train_Y,family="binomial",type.measure = "auc", alpha=input$elastic_mel)
  })
  pred_elastic=reactive({
    set.seed(7)
    predict(elastic(),newx=test_X,s=input$elastic_reg, type="response")
  })
  output$Auc_elastic=renderPlot({
    set.seed(7)
    plot(roc.curve(scores.class0=pred_elastic(),weights.class0=test_Y=="1",curve=TRUE))
  })
  
  output$PR_elastic=renderPlot({
    set.seed(7)
    plot(pr.curve(scores.class0=pred_elastic(),weights.class0=test_Y=="1",curve=TRUE))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)