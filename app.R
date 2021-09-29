# basic generative app
library(shiny)
library(colorspace)
library(tidyverse)
library(scico)

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel(" "),

    # Sidebar with inputs
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Number of lines:",
                        min = 1,
                        max = 5000,
                        value = 1000),
            sliderInput("u",
                        "Spacing between points:",
                        min = 0,
                        max = 100,
                        value = 15),
            sliderInput("angle",
                        "Phyllotaxis:",
                        min = 0,
                        max = 36,
                        value = pi*(3-sqrt(5))),
            selectInput("color_package", "Color package:",
                        c("scico" = "scico",
                          "colorspace (sequential)" = "colorspace")),
            textInput("color_scale",
                      "Color scale name",
                      "lajolla"),
            sliderInput("color_n",
                        "Number of colors:",
                        min = 1,
                        max = 9,
                        value = 5),
            selectInput("color_pattern", "Color pattern:",
                        c("Radial" = "radial",
                          "Circular" = "circular")),
            checkboxInput("color_direction", "Reverse palette", FALSE),
            sliderInput("spin",
                        "Rotate",
                        min=0,
                        max=360,
                        value = 0),
            textInput("color_bkgnd",
                      "Background color",
                      "white"),
        ),

        # Plot
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic
server <- function(input, output) {

    output$distPlot <- renderPlot({
        phi <- (1 + sqrt(3)) / 2
        golden_angle <- pi*(3-sqrt(5))
        v <- input$spin

        n <- input$n
        u <- input$u
        angle <- input$angle

        # generate data
        df <- tibble(
            idx = c(0:(n-1)), ## increase to use more lines.
            t = seq(0,2*pi,length.out=n),
            r = sqrt(idx), ## radius
            x = r*cos(angle*idx),
            y = r*sin(angle*idx),
            color_angle = atan2(y=y,x=x)
        )

        v <- ifelse(v<u,v,v%%u)
        max_r <- max(df$r)*1.1

        # color scale options
        if(input$color_pattern != "radial"){
            df$plot_color <- df$color_angle
        } else {
            df$plot_color <- df$t
        }
        if (input$color_package == "scico"){
            pal <- scico::scico(n=input$color_n, palette = input$color_scale)
        } else {
            pal <- sequential_hcl(n=input$color_n, palette = input$color_scale)
        }
        if(input$color_direction == TRUE){
            pal <- rev(pal)
        }

        df %>%
            ggplot(aes(x=x,y=y,color=plot_color)) +
            geom_path(data = . %>% filter(idx%%u==v),
                      lineend="round", linejoin="mitre", linemitre=3,
                      aes(size=idx, alpha=idx)) +
            coord_fixed() +
            theme_void() +
            scale_alpha_continuous(guide="none", range=c(0,1), trans="sqrt")+
            scale_size_continuous(guide="none",  range=c(10,0)) +
            scale_color_gradientn(guide="none",
                                  colors=pal) +
            theme(panel.background = element_rect(fill=input$color_bkgnd, color=NA)) +
            expand_limits(x=c(-max_r,max_r),y=c(-max_r,max_r))



    })
}

# Run the application
shinyApp(ui = ui, server = server)
