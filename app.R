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


# # Input Registry CSV
# # This should be an EMu eregistry report including 
# #   Key1 through Key10 and the Value columns
# #   for all records where Key5 == 'Security' and Key1 == 'Group'
# urlfile="https://raw.githubusercontent.com/fieldmuseum/EMu-Registry-Groups/master/sample_data_in/table_security/eregistr.csv"
# catFields <- read_csv(url(urlfile))

# input_filepath <- "../sample_data/table_security/"
if (Sys.getenv("LIVE_OR_TEST")=="LIVE") {
  input_filepath <- paste0(Sys.getenv("LIVE_REGISTRY_IN"), "table_security/")
} else {
  input_filepath <- paste0(Sys.getenv("TEST_REGISTRY_IN"), "table_security/")
}

mm_sec_bu <- read_csv(file=paste0(input_filepath,"eregistr.csv"))  # read_csv(url(urlfile))  # 

modules <- unique(mm_sec_bu$Key4[order(mm_sec_bu$Key4)])
mm_sec <- mm_sec_bu[mm_sec_bu$Key6!="Insert",]

mm_sec$SecDept <- mm_sec$Value %>%
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

# Split out Permission based on SecDept values only vs Other Permissions
mm_not_sec <-mm_sec3[grepl("SecDep", mm_sec3$Perm) < 1,]
  
mm_sec3 <- mm_sec3[grepl("SecDep", mm_sec3$Perm) > 0,]
mm_sec3$Perm <- gsub("SecDept_", "", mm_sec3$Perm)

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
  gsub("^\\s+|\\s+$", "", .)

mm_sec5 <- as.data.frame(mm_sec5)
rownames(mm_sec5) <- mm_sec5$table_group
mm_sec6 <- as.matrix(mm_sec5[,2:NCOL(mm_sec5)])
mm_sec6 <- mm_sec6[,order(colnames(mm_sec6))]


# Prep not-sec / Other Permissions
# spread display/edit/delete permission
mm_not_sec3 <- mm_not_sec[order(mm_not_sec$Key4, mm_not_sec$Key2, mm_not_sec$Key6),
                   !colnames(mm_not_sec) %in% c("Perm_order")]

mm_not_sec3 <- mm_not_sec3[order(mm_not_sec3$Key4, mm_not_sec3$Key2, mm_not_sec3$Perm),]

mm_not_sec4 <- pivot_wider(unique(mm_not_sec3), id_cols = c(Key4,Key2,Perm),
                       names_from = Key6, values_from = Key6)

mm_not_sec4$Permission <- NA
mm_not_sec4$Permission[is.na(mm_not_sec4$Display)==F] <- 1
mm_not_sec4$Permission[is.na(mm_not_sec4$Edit)==F] <- 2
mm_not_sec4$Permission[is.na(mm_not_sec4$Delete)==F] <- 3

drop_cols2 <- c("Display", "Edit", "Delete")

mm_not_sec4 <- mm_not_sec4[nchar(mm_not_sec4$Perm) > 0,]

mm_not_sec5 <- pivot_wider(mm_not_sec4[,!colnames(mm_not_sec4) %in% drop_cols2], id_cols=Key4:Key2,
                       names_from=Perm, values_from=Permission)

mm_not_sec5[is.na(mm_not_sec5)] <- 0
mm_not_sec5 <- unite(mm_not_sec5, col = "table_group", Key4:Key2)

colnames(mm_not_sec5) <- colnames(mm_not_sec5) %>%
  gsub("^\\s+|\\s+$", "", .)
mm_not_sec5 <- as.data.frame(mm_not_sec5)
rownames(mm_not_sec5) <- mm_not_sec5$table_group

mm_not_sec6 <- as.matrix(mm_not_sec5[,2:NCOL(mm_not_sec5)])
mm_not_sec6 <- mm_not_sec6[,order(colnames(mm_not_sec6))]


# Define UI
ui <- fluidPage(
  
  title = "EMu Group Permissions",
  
  # App title
  titlePanel("User-group Permissions"),
  tags$p("What values affect whether a user-group can view, edit, and/or delete records?"),
  tags$p(tags$strong("Last Mod: 2024-Aug-9"), " -- NOTE: This chart does not currently include query-defaults which may further restrict access."),
  tags$br(),
  tags$em(" May take a moment to load."),


  # Allow user to select a module to summarize
  sidebarLayout(
    sidebarPanel(

      selectInput("ModuleChosen",
                  label = "Choose a Module:",
                  choices = modules,
                  selected = "ecatalogue"),
      
      tags$br(),
      tags$p(tags$strong("In the main chart:")),
      tags$p(tags$strong("Each row"), " = an EMu user-group"),
      tags$p(tags$strong("Each column"), " = a value in an EMu field that can affect a group's permission"),
      tags$p(tags$strong("Color"), " represents permission levels:"),
      tags$ul(
        tags$li(tags$strong("3"), " (pink) = Full Delete, Edit & View", style = "color:#9E0076"), #  "color:#c4569f"),
        tags$li(tags$strong("2"), " (orange) = Edit & View", style = "color:#AD5F00"),  # "color:#ca9c33"),
        tags$li(tags$strong("1"), " (green) = View only", style = "color:#208320"), # "color:#64ab47"),
        tags$li(tags$strong("0"), " (gray) = no setting", style = "color:#707070")
      ),

      tags$br(),
      
      tags$line(),
      tags$p("Input-data is a CSV structured ",
             tags$a(href = "https://raw.githubusercontent.com/fieldmuseum/EMu-Registry-Groups/master/sample_data_in/table_security/eregistr.csv",
                    "like this.")),
      tags$p("Code for this app and further info in ",
             tags$a(href = "https://github.com/fieldmuseum/EMu-Registry-Groups",
                    "this repo.")),
      tags$br(),

      width = 3
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      h3("User-group permissions for", textOutput("modulePick")),
      tags$p("(If no chart displays, no group permissions are set.)"),
      plotlyOutput("heatPlot", height = "800px"),
      
      tags$br(),

      h4("Other fields/values that may affect permission settings:"),
      tags$p("(If no chart displays, no other permissions are set.)"),
      plotlyOutput("heatPlot2", height = "600px"),
      
      width = 8,
      fluid = FALSE
      
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
    
    req(NROW(mm_sec6[grepl(input$ModuleChosen, rownames(mm_sec6)) > 0,]) > 0)

    heatmaply(mm_sec6[grepl(input$ModuleChosen, rownames(mm_sec6)) > 0,],
              labRow = gsub(paste0(input$ModuleChosen, "_"), "",
                            rownames(mm_sec6)[grepl(input$ModuleChosen, rownames(mm_sec6)) > 0]),
              colors = c("#f7f7f7", "#bfe480", "#f4b850", "#c4569f"),
              k_row = 1, k_col = 1,
              dendrogram = c("none"),
              show_dendrogram = c(FALSE, FALSE),
              show_grid = FALSE,
              plot_method = "plotly",
              xlab = "Security Department", 
              ylab = "EMu User Group",
              limits = c(0,3),
              colorbar_yanchor = "top")

  })

  output$heatPlot2 <- renderPlotly({
    
    req(NROW(mm_not_sec6[grepl(input$ModuleChosen, rownames(mm_not_sec6)) > 0,]) > 0)
    
    heatmaply(mm_not_sec6[grepl(input$ModuleChosen, rownames(mm_not_sec6)) > 0,],
              labRow = gsub(paste0(input$ModuleChosen, "_"), "",
                            rownames(mm_not_sec6)[grepl(input$ModuleChosen, rownames(mm_not_sec6)) > 0]),
              colors = c("#f7f7f7", "#bfe480", "#f4b850", "#c4569f"),
              k_row = 1, k_col = 1,
              dendrogram = c("none"),
              show_dendrogram = c(FALSE, FALSE),
              show_grid = FALSE,
              plot_method = "plotly",
              xlab = "Permission Setting", 
              ylab = "EMu User Group",
              limits = c(0,3),
              colorbar_yanchor = "top")
    
  })

  output$modulePick <- renderText({
    paste(input$ModuleChosen, "SecDepartment values")
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
