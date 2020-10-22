library(REigenfaces)

# Compute info on images as dataset
y <- load_dataset(images)

# See above for the definitions of ui and server
ui <- fluidPage(
  titlePanel(h1("Eigenfaces", align = "center")),
  fluidRow(helpText("A hundred images of faces have been loaded of size 64x64.
                 Of those the relevant data has been computed for eigenfaces.", align = "center"), style = "background-color:#66ffff;"),
  # description row
  fluidRow(
    column(4,
      helpText("This column will contain the selected image.")
    ),
    column(4
    ),
    column(4,
      helpText("This column will contain the selected image reconstructed with the top x eigenfaces.")
    )
  ),
  #input row
  fluidRow(
    column(4,
      numericInput(
      inputId = "whichToFocus",
      label = "Select index of the image you want to focus on.",
      value = 1,
      min = 1,
      max = NA,
      step = NA,
      width = NULL)
    ),
    column(4),
    column(4,
      sliderInput("amountEigenfacesToUse", "Number of eigenfaces to reconstruct the selected image with.",
                  min = 1, max = 64, value = 64)
    )
  ),
  #image row
  fluidRow(
    column(4,
           plotOutput("selected_focus")
    ),
    column(4 #,plotOutput("selected_reconstructed"
    ),
    column(4,
           plotOutput("selected_reconstructed_with")
    )
  ),
  fluidRow(
    column(6, p("Four most similar faces to the selected image will be shown.", align = "center"),style = "background-color:#66ffff;")
  ),
  fluidRow(
    column(6,
      plotOutput("selected_similar_eigenfaces")
    ),
    column(6,
      plotOutput(outputId = "distPlot")
    )
  )
)

server <- function(input, output){

  #output$selected_images <- renderPlot({
  #  show_images(
  #    most_important_eigenfaces(y, input$amountEigenfacesToUse),
  #    c(64,64)
  #  )
  #})

  output$selected_focus <- renderPlot({
    show_images(
      images[,input$whichToFocus],
      c(64,64)
    )
  })

  output$selected_reconstructed_with <- renderPlot({
    w <- y$mean

    for(i in 1:input$amountEigenfacesToUse){
      w <- w + y$eigenfaces[,i]*y$image_coef[i,input$whichToFocus]
    }
    show_images(w,c(64,64))
  })

  output$selected_similar_eigenfaces <- renderPlot({
    show_images(images[,similar_faces_indices(y,images[,input$whichToFocus],4)],
      c(64,64))
  })

  output$distPlot <- renderPlot({

    x    <- y$image_coef[1:input$amountEigenfacesToUse,input$whichToFocus]
    print(x)
    plot(1:input$amountEigenfacesToUse,x,

      xlab="x most important eigenfaces",
      ylab = "Coefficients of these accoring to the selected image."
    )

  })

}

shinyApp(ui = ui, server = server)
