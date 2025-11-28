# my library exploring app using shiny

library(shiny)
library(dplyr)
library(DT)
library(stringr)
library(bslib)

# ---- Load and lightly clean data ----
goodreads_raw <- read.csv(
  "goodreads_library_export.csv",
  stringsAsFactors = FALSE
)

goodreads <- goodreads_raw %>%
  mutate(
    ISBN   = str_replace_all(ISBN,   '^=\"|\"$', ""),
    ISBN13 = str_replace_all(ISBN13, '^=\"|\"$', ""),
    Goodreads_Link = paste0(
      "https://www.goodreads.com/search?q=",
      utils::URLencode(paste(Title, Author))
    )
  )

min_year <- suppressWarnings(min(goodreads$Year.Published, na.rm = TRUE))
max_year <- suppressWarnings(max(goodreads$Year.Published, na.rm = TRUE))

bindings <- sort(unique(goodreads$Binding))
bindings <- bindings[bindings != ""]
bindings <- c("All formats" = "all", bindings)

# ---- UI ----
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),

  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f7fa;
      }
      .app-header {
        margin-bottom: 20px;
        padding: 15px 20px;
        background: #ffffff;
        border-radius: 12px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.06);
      }
      .app-header h2 {
        margin-bottom: 5px;
        font-weight: 600;
      }
      .app-header p {
        margin-bottom: 0;
        color: #6c757d;
      }
      .sidebar-card, .main-card {
        background: #ffffff;
        border-radius: 12px;
        padding: 15px 18px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.05);
      }
      .book-details, .surprise-details {
        margin-top: 15px;
      }
      .book-details h3, .surprise-details h3 {
        margin-top: 0;
      }
    "))
  ),

  div(
    class = "app-header",
    h2("Megan’s Book Picker 📚"),
    p("Browse through my library and pick something to read.")
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(
        class = "sidebar-card",
        h4("Filters"),

        selectInput(
          "shelf_filter",
          "Shelf",
          choices = c(
            "Books I've read"   = "read",
            "My 'to-read' list" = "to-read",
            "All shelves"       = "all"
          ),
          selected = "read"   # default to  READ shelf
        ),

        sliderInput(
          "my_rating_filter",
          "My rating (stars)",
          min = 0, max = 5,
          value = c(0, 5),
          step = 1
        ),

        sliderInput(
          "avg_rating_filter",
          "Average Goodreads rating",
          min = 0, max = 5,
          value = c(0, 5),     # wide open so nothing disappears by accident
          step = 0.1
        ),

        if (is.finite(min_year) && is.finite(max_year)) {
          sliderInput(
            "year_filter",
            "Year published",
            min = min_year,
            max = max_year,
            value = c(min_year, max_year),
            step = 1,
            sep = ""
          )
        } else {
          sliderInput(
            "year_filter",
            "Year published",
            min = 1950,
            max = as.integer(format(Sys.Date(), "%Y")),
            value = c(1950, as.integer(format(Sys.Date(), "%Y"))),
            step = 1,
            sep = ""
          )
        },

        selectInput(
          "binding_filter",
          "Format",
          choices = bindings,
          selected = "all"
        ),

        textInput(
          "search_text",
          "Search title or author",
          placeholder = "e.g., 'Armstrong' or 'fantasy'"
        ),

        helpText("Tip: click a row in the table to see full details.")
      )
    ),

    mainPanel(
      width = 9,
      div(
        class = "main-card",
        h4("Books matching your filters"),

        # surprise buttons row
        div(
          class = "d-flex justify-content-end mb-2",
          actionButton("surprise_read", "Surprise me (read shelf)"),
          actionButton("surprise_toread", "Surprise me (to-read)", class = "ms-2")
        ),

        DTOutput("book_table"),

        hr(),
        h4("Selected book"),
        uiOutput("book_details"),

        hr(),
        h4("Random recommendation"),
        uiOutput("surprise_details")
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {

  # main filtered dataset
  filtered_books <- reactive({
    req(input$my_rating_filter, input$avg_rating_filter, input$year_filter)

    df <- goodreads

    # Shelf
    if (input$shelf_filter != "all") {
      df <- df %>%
        filter(Exclusive.Shelf == input$shelf_filter)
    }

    # My rating
    df <- df %>%
      filter(
        My.Rating >= input$my_rating_filter[1],
        My.Rating <= input$my_rating_filter[2]
      )

    # Average rating
    df <- df %>%
      filter(
        Average.Rating >= input$avg_rating_filter[1],
        Average.Rating <= input$avg_rating_filter[2]
      )

    # Year
    df <- df %>%
      filter(
        is.na(Year.Published) |
          (Year.Published >= input$year_filter[1] &
             Year.Published <= input$year_filter[2])
      )

    # Format
    if (input$binding_filter != "all") {
      df <- df %>%
        filter(Binding == input$binding_filter)
    }

    # Search
    if (nzchar(input$search_text)) {
      search <- tolower(input$search_text)
      df <- df %>%
        filter(
          grepl(search, tolower(Title)) |
            grepl(search, tolower(Author))
        )
    }

    df
  })

  # ---- table ----
  output$book_table <- renderDT({
    df <- filtered_books()

    if (nrow(df) == 0) {
      return(datatable(
        data.frame(
          Message = "No books match your filters. Try widening them!"
        ),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }

    df_table <- df %>%
      transmute(
        Title,
        Author,
        `My rating`  = My.Rating,
        `Avg rating` = round(Average.Rating, 2),
        Year         = Year.Published,
        Format       = Binding,
        Shelf        = Exclusive.Shelf,
        Goodreads    = paste0(
          "<a href='", Goodreads_Link,
          "' target='_blank'>View</a>"
        )
      )

    datatable(
      df_table,
      selection = "single",
      escape = FALSE,
      rownames = FALSE,
      class = "cell-border stripe hover compact",
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25, 50),
        dom = "tip",
        columnDefs = list(
          list(className = "dt-center", targets = c(2, 3, 4, 6, 7))
        )
      )
    )
  })

  # ---- details for clicked row ----
  output$book_details <- renderUI({
    df  <- filtered_books()
    sel <- input$book_table_rows_selected

    if (is.null(sel) || length(sel) == 0 || nrow(df) == 0) {
      return(
        div(
          em("Click a book in the table to see more details here.")
        )
      )
    }

    book <- df[sel, ]

    year_str      <- ifelse(is.na(book$Year.Published), "Unknown", book$Year.Published)
    binding_str   <- ifelse(book$Binding == "" | is.na(book$Binding), "Unknown", book$Binding)
    pages_str     <- ifelse(is.na(book$Number.of.Pages), "Unknown", book$Number.of.Pages)
    date_read_str <- ifelse(book$Date.Read == "" | is.na(book$Date.Read),
                            "Not recorded",
                            book$Date.Read)

    tagList(
      h3(book$Title),
      h4(paste("by", book$Author)),
      p(
        strong("My rating: "), book$My.Rating,
        "   |   ",
        strong("Average rating: "), round(book$Average.Rating, 2)
      ),
      p(strong("Year published: "), year_str),
      p(strong("Format: "), binding_str),
      p(strong("Shelf: "), book$Exclusive.Shelf),
      p(strong("Pages: "), pages_str),
      p(strong("Date read: "), date_read_str),
      if (!is.na(book$My.Review) && nzchar(book$My.Review)) {
        tagList(
          br(),
          h4("My notes / review"),
          p(book$My.Review)
        )
      },
      br(),
      tags$a(
        href = book$Goodreads_Link,
        target = "_blank",
        class = "btn btn-primary btn-sm",
        "Open on Goodreads"
      )
    )
  })

  # ---- Surprise-me logic ----
  surprise_book <- reactiveVal(NULL)

  observeEvent(input$surprise_read, {
    pool <- goodreads %>% filter(Exclusive.Shelf == "read")
    if (nrow(pool) > 0) {
      surprise_book(pool[sample(nrow(pool), 1), ])
    }
  })

  observeEvent(input$surprise_toread, {
    pool <- goodreads %>% filter(Exclusive.Shelf == "to-read")
    if (nrow(pool) > 0) {
      surprise_book(pool[sample(nrow(pool), 1), ])
    }
  })

  output$surprise_details <- renderUI({
    book <- surprise_book()

    if (is.null(book)) {
      return(
        div(
          em("Click one of the buttons above for a random pick from your shelves.")
        )
      )
    }

    year_str    <- ifelse(is.na(book$Year.Published), "Unknown", book$Year.Published)
    binding_str <- ifelse(book$Binding == "" | is.na(book$Binding), "Unknown", book$Binding)
    pages_str   <- ifelse(is.na(book$Number.of.Pages), "Unknown", book$Number.of.Pages)

    tagList(
      h3(book$Title),
      h4(paste("by", book$Author)),
      p(
        strong("Shelf: "), book$Exclusive.Shelf,
        "   |   ",
        strong("My rating: "), book$My.Rating,
        "   |   ",
        strong("Average rating: "), round(book$Average.Rating, 2)
      ),
      p(
        strong("Year published: "), year_str,
        "   |   ",
        strong("Format: "), binding_str,
        "   |   ",
        strong("Pages: "), pages_str
      ),
      br(),
      tags$a(
        href = book$Goodreads_Link,
        target = "_blank",
        class = "btn btn-outline-primary btn-sm",
        "Open on Goodreads"
      )
    )
  })
}

shinyApp(ui, server)
