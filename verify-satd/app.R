#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(gt)
library(reactable)
library(DBI)
library(tidyverse)
library(dbplyr)


con <- dbConnect(RSQLite::SQLite(), "db/db.db")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title

    tabsetPanel(
        
        tabPanel(
            title = "Evaluate",
            
            sidebarLayout(
                
                sidebarPanel( 
                    radioButtons(
                        inputId = "user",
                        label = "User",
                        choices = c("Bruno", "Prof. Earl", "Prof. Marcio"),
                        inline = TRUE
                    ),
                    
                    radioButtons(
                        inputId = "satd",
                        label = "SATD?",
                        choices = c("YES!", "Nooooooooo!"),
                        inline = TRUE
                    ),
                    
                    textAreaInput(
                        inputId = "justification",
                        label = "Justify",
                        cols = 80,
                        rows = 20
                    ),
                    
                    actionButton(
                        inputId = "save",
                        label = "Save and sample another one!"
                    )
                ),
                mainPanel(
                    tags$hr(),
                    "Comment info:",
                    gt_output("info"),
                    tags$hr(),
                    "Comment:",    
                    verbatimTextOutput("comment"),
                    tags$hr(),
                    "Bag found",
                    gt_output("bags"),
                    tags$hr(),
                    htmlOutput("frame")
                )
            )
            
        ),
        
        tabPanel(
            
            title = "Results",
            
            actionButton(
                inputId = "refresh",
                label = "Refresh"
            ),

            actionButton(
                inputId = "delete",
                label = "Delete selected!"
            ),

            
            reactableOutput("table")
            
            
        )
        
        
        
        
        
    )
    
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    values <- reactiveValues(
        message = NULL, 
        address = NULL, 
        comment = NULL,
        bags = NULL,
        comment_register = NULL
    )
    
    evaluations_table <- reactiveValues(
        table = NULL
    )
    

    observeEvent(input$save, {


        if(!is.null(values$comment_register$id_comment_pk)){

            chosen_pk = values$comment_register$id_comment_pk
            
            insert_data <- tibble(
                user = input$user,
                id_comment_pk = chosen_pk,
                satd = input$satd,
                justification = input$justification
            )
            
            dbWriteTable(con, "evaluations", insert_data, append = TRUE)
        }        
        
        rows <- dbGetQuery(con, "SELECT COUNT() as c FROM cOMMENTS where satd = 1") %>% pull(c)
        lucky <- sample(1:rows, size = 1)
        chosen_comment <- tbl(con, "comments" ) %>% 
            filter(
                satd == 1
            ) %>% 
            filter(
                row_number() == lucky
            ) %>% 
            collect() %>% 
            mutate(
                file = str_remove(file, "c:/doutorado")
            )
        
        values$comment <- chosen_comment$comment
        id_bag_find <- chosen_comment$id_bag
        chosen_bag <- tbl(con, "bags" ) %>% 
            filter(
                id_bag == id_bag_find
            ) %>% 
            collect()
        
        project_to_find <- chosen_comment$project
        chosen_project <- tbl(con, "map_github" ) %>% 
            filter(
                project == project_to_find
            ) %>% 
            collect()


        values$address <- str_replace(chosen_comment$file, chosen_project$root_dir, chosen_project$root_github)
        values$bags <- chosen_bag
        values$comment_register <- chosen_comment
        

    })    

    output$frame <- renderUI({
        tags$a(href = values$address, "Click to see the comment in context!", target = "_blank" )
    })

    output$bags <- render_gt({
        validate(need(!is.null(values$bags), "Sample a comment"))
        gt(values$bags)
    })
    
    output$info <- render_gt({


        validate(need(!is.null(values$comment_register), "Sample a comment"))
        
        values$comment_register %>%
            select(
                project,
                file,
                beginline,
                endline
            ) %>% 
            mutate(
                across(
                    .cols = everything(),
                    .fns = as.character
                )
            ) %>% 
            pivot_longer(
                cols = everything(),
                names_to = "Attribute",
                values_to = "Value"
            ) %>% 
            gt()
    })
    
    output$comment <- renderText(
        values$comment
    )
    
    output$table <- renderReactable({
        
        validate(need(!is.null(evaluations_table$table), "Click refresh"))
        
        evaluations_table$table %>% 
            reactable(
                selection = "multiple", onClick = "select"
            )
    })
    
    
    observeEvent(input$refresh,{
        
        evaluations_table$table <- tbl(con, "evaluations") %>% 
            collect()

    })
    
    

    observeEvent(input$delete,{
    
        
        id_evaluation_to_delete <- 
            evaluations_table$table %>% 
            filter(
                row_number() %in% getReactableState("table")$selected
            ) %>% 
            pull(
                id_evaluation
            )
        
        id_juntos = str_flatten(id_evaluation_to_delete, collapse = ",")            
        
        query <- "DELETE FROM EVALUATIONS WHERE ID_EVALUATION IN ({id_juntos})" %>%  str_glue()
        
        dbExecute(con, query)
        
        evaluations_table$table <- tbl(con, "evaluations") %>% 
            collect()

        
    })
    
                 

}

# Run the application 
shinyApp(ui = ui, server = server)
