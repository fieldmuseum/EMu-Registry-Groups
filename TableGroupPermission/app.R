# App to summarize an EMu clients user-group settings
# More about building Shiny apps here: http://shiny.rstudio.com/
# 2022-Oct-10 - (c) FMNH - MIT License

library(shiny)
library(tidyr)
library(readr)
library(stringr)

library(heatmaply)
library(RColorBrewer)
library(plotly)

# urlfile="https://raw.githubusercontent.com/fieldmuseum/EMu-Documentation/master/Schemas/all_schemas.csv"
# catFields <- read_csv(url(urlfile))

# input_filepath <- "../sample_data/table_security/"
input_filepath <- "../real_data_in/table_security/"
mm_sec_bu <- read_csv(file=paste0(input_filepath,"eregistr.csv"))  # read_csv(url(urlfile))  # 

modules <- unique(mm_sec_bu$Key4)


# remove 'Display' Security settings
# mm_sec_bu <- read_csv(file=paste0(input_filepath,"eregistr.csv"))

mm_sec <- mm_sec_bu[mm_sec_bu$Key6!="Insert",]

# split out the non-SecDept settings
# mm_sec$IntranetYesOnly <- NA
# mm_sec$IntranetYesOnly[grepl("AdmPublishWebPassword\\=Yes", mm_sec$Value)] <- "Intranet_Yes"
# 
# mm_sec$RecordStatusActiveOnly <- NA
# mm_sec$RecordStatusActiveOnly[grepl("SecRecordStatus\\=(.*)(;)*(.*)", mm_sec$Value)] <-
#   paste0("RecordStatus_",
#          # gsub("Active")
#          gsub("SecRecordStatus\\=(.+)(;)+.*","\\1", mm_sec$Value))
# 
# mm_sec$SecDept <- mm_sec$Value %>%
#   gsub("\\s*SecDepartment_tab\\=|\\s*AdmPublishWebPassword\\=Yes(;)*|\\s*SecRecordStatus\\=Active(;)*",
#        "", .) %>%
#   gsub("\\s*(\\=|\\-|\\$)\\s*", "_", .)
# #   gsub("(\\s|\\=|\\$|\\-)+", "_", .)


mm_sec$SecDept <- mm_sec$Value %>%
  # gsub("\\s*SecDepartment_tab\\=|\\s*AdmPublishWebPassword\\=Yes(;)*|\\s*SecRecordStatus\\=Active(;)*",
  #      "", .) %>%
  gsub("\\s*(\\=|\\-|\\$)+\\s*", "_", .) %>%
  gsub('(\")+', "NULL", .) %>%
  gsub("SecDepartment_tab", "SecDept", .)

max_depts <- 2

for (i in 1:NROW(mm_sec)) {
  
  depts_count <- str_count(mm_sec$SecDept[i], ";") + 1
  if (depts_count > max_depts) {
    max_depts <- depts_count
  }
  
}

mm_sec <- separate(mm_sec, SecDept, 
                   into=c(paste0("Dept_",1:max_depts)),
                   sep = ";")

drop_cols <- c("eregistry_key", "irn", "Key1", "Key3", "Key5",
               "Key7","Key8","Key9","Key10","Value",
               "AdmDateInserted", "AdmDateModified")

# gather columns to re-order/re-shape
mm_sec2 <- mm_sec[,!colnames(mm_sec) %in% drop_cols]

mm_sec3 <- pivot_longer(mm_sec2, cols = Dept_1:colnames(mm_sec2)[NCOL(mm_sec2)], # IntranetYesOnly:...
                        names_to = "Perm_order",
                        values_to = "Perm", values_drop_na = TRUE)

# spread display/edit/delete permission
mm_sec3 <- mm_sec3[order(mm_sec3$Key4, mm_sec3$Key2, mm_sec3$Key6),  # ]
                   !colnames(mm_sec3) %in% c("Perm_order")]

mm_sec3 <- mm_sec3[order(mm_sec3$Key4, mm_sec3$Key2, mm_sec3$Perm),]

mm_sec4 <- pivot_wider(unique(mm_sec3), id_cols = c(Key4,Key2,Perm),
                       names_from = Key6, values_from = Key6)

mm_sec4$Permission <- NA
mm_sec4$Permission[is.na(mm_sec4$Display)==F] <- 1
mm_sec4$Permission[is.na(mm_sec4$Edit)==F] <- 2
mm_sec4$Permission[is.na(mm_sec4$Delete)==F] <- 3

drop_cols2 <- c("Display", "Edit", "Delete")

mm_sec4 <- mm_sec4[nchar(mm_sec4$Perm) > 0,]

mm_sec5 <- pivot_wider(mm_sec4[,!colnames(mm_sec4) %in% drop_cols2], id_cols=Key4:Key2,
                       names_from=Perm, values_from=Permission)

mm_sec5[is.na(mm_sec5)] <- 0
mm_sec5 <- unite(mm_sec5, col = "table_group", Key4:Key2)

colnames(mm_sec5) <- colnames(mm_sec5) %>%
  gsub("^\\s+|\\s+$", "", .) # %>%
#   gsub("(\\s|\\=|\\$|\\-)+", "_", .)

mm_sec5 <- as.data.frame(mm_sec5)
rownames(mm_sec5) <- mm_sec5$table_group

mm_sec6 <- as.matrix(mm_sec5[,2:NCOL(mm_sec5)])
mm_sec6 <- mm_sec6[,order(colnames(mm_sec6))]


# Define UI
ui <- fluidPage(
  
  title = "EMu Group Permissions",
  
  # App title
  titlePanel("Permissions by table"),
  # tags$p("Charts visualize",
         # tags$a(href = "https://raw.githubusercontent.com/fieldmuseum/EMu-Documentation/master/Schemas/all_schemas.csv", 
         #        "all_schemas.csv"), 
         # " in ",
         # tags$a(href = "https://github.com/fieldmuseum/EMu-Documentation/tree/master/Schemas",
         #        "this repo."),
         # tags$em(" May take a moment to load.")),


  # Allow user to select a module to summarize
  sidebarLayout(
    sidebarPanel(

      selectInput("ModuleChosen",
                  label = "Choose a Module:",
                  choices = modules,
                  selected = "emultimedia"),
      
      tags$br(),
      h5("In the main chart:"),
      tags$p("- ", tags$strong("Each column"), " = an institution's EMu schema"),
      tags$p("- ", tags$strong("Each row"), " = a column-name (field) in the EMu schema"),
      tags$p("- ", tags$strong("0"), " (white) = no setting"),
      tags$p("- ", tags$strong("1"), " (pale blue) = View-permission"),
      tags$p("- ", tags$strong("2"), " (mid blue) = Edit-permission"),
      tags$p("- ", tags$strong("3"), " (dark blue) = Delete-permission"),
      tags$br(),

      width = 3
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      h3("Group permissions", textOutput("modulePick")),
      plotlyOutput("heatPlot", height = "800px"),
      tags$br(),
      # tags$br(),
      # # 
      # # h4("Alternate view:"),
      # # tags$p(tags$em("- 'Missing' here simply means a field is not in a schema -- not necessarily a bad thing.")),
      # # plotOutput("visdatChosenMiss"),
      # 
      # plotOutput("testPlot"),
      # tags$br,
      
      width = 8,
      fluid = FALSE  # 10
      
    )

  )
)


# Define server logic
server <- function(input, output) {
  
  # reactive(
    
    # mm_sec5 <- mm_sec5[mm_sec5$Key4==input$ModuleChosen,]
    
    # mm_sec5 <- unite(mm_sec5, col = "table_group", Key4:Key2)
    # mm_sec5 <- mm_sec5[,!colnames(mm_sec5 %in% "Key4")]

  # )
# 
  output$heatPlot <- renderPlotly({
    # vis_dat(catFields)

    heatmaply(mm_sec6[grepl(input$ModuleChosen, rownames(mm_sec6)) > 0,],
              # as.matrix(mm_sec5[grepl(input$ModuleChosen, rownames(mm_sec5)) > 0,
              #                   2:NCOL(mm_sec5)]),
              labRow = gsub(paste0(input$ModuleChosen, "_"), "",
                            rownames(mm_sec6)[grepl(input$ModuleChosen, rownames(mm_sec6)) > 0]),
              # labCol = colnames(mm_sec5)[2:NCOL(mm_sec5)],
              colors = brewer.pal (3, "Blues" ), # heat.colors(3),
              k_row = 1, k_col = 1,
              dendrogram = c("none"),
              show_dendrogram = c(FALSE, FALSE),
              # width = ,
              # height = 500,
              show_grid = FALSE,
              plot_method = "plotly",
              # scale = "column",
              colorbar_yanchor = "top")

  })

#   output$testPlot <- renderPlot({
# 
# 
#     # ggplotly(
#       ggplot(mm_sec4[mm_sec4$Key4==input$ModuleChosen,],  # as.data.frame(mm_sec6[grepl("Default", rownames(mm_sec6)) > 0,]),
#              aes(x = Perm, # colnames(mm_sec6[grepl("Default", rownames(mm_sec6)) > 0,]),
#                  y = Key2, # rownames(mm_sec6[grepl("Default", rownames(mm_sec6)) > 0,]),
#                  fill = Permission)) + # mm_sec6[grepl("Default", rownames(mm_sec6)) > 0,])) +
#       geom_tile()
# 
#   })
#   
  # output$visdatCat <- renderPlot({
  #   vis_dat(catFields[catFields$Table=="ecatalogue",])
  # })
  # 
  # output$visdatPar <- renderPlot({
  #   vis_dat(catFields[catFields$Table=="eparties",])
  # })

  # output$visdatChosen <- renderPlot({
  #   vis_dat(catFields[catFields$Table==input$ModuleChosen,])
  # })
  # 
  # output$visdatChosenMiss <- renderPlot({
  #   vis_miss(catFields[catFields$Table==input$ModuleChosen,],
  #            sort_miss = TRUE)
  # })

  output$modulePick <- renderText({
    paste(input$ModuleChosen, "fields")
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
