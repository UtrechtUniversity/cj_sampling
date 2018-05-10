server <- function(input, output, session) {
  set.seed(11)
  library(ggplot2)
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  
  #15x10
  # 600 x 400
  
  # Cases
  init_app <- function(){
    x <- as.vector(replicate(10, sample.int(15, 10))) + rnorm(100, sd = .2)
    y <- rep(1:10, each = 10) + rnorm(100, sd = .2)
    rprob <- runif(1, 0, .8)
    shape <- factor(sapply(x, function(xes){
      ps <- .1 + rprob*(xes/max(x))
      ps <- c(ps, 1-ps)
      sample.int(2, 1, replace = TRUE, prob = ps)
    }))
    order_probs <- sample.int(5, 5)
    fill <- factor(sapply(y, function(ys){
      ps <- c((.05 + .5*(ys/max(y))), (.4*(ys/max(y))))
      ps <- c(ps, rep((1-sum(ps))/3, 3))[order_probs]
      sample.int(5, 1, replace = TRUE, prob = ps)
    }))

    data <- data.frame(x=x, y=y, shape = shape, fill = fill)
    
    # Stick figure
    rx <- sample.int(15, 1)
    ry <- sample.int(10, 1)
    man_data <- data.frame(x = rx+c(0, 0, -.3, -.05, -.3, 0, .1, .35, .1, 0, 0, -.15, 0, .2), y = ry+1*c(0, -.2, -.4, -.6, -.4, -.2, -.4, -.6, -.4, -.2, -.6, -1.1, -.6, -1.1))
    
    
    p <- ggplot(data, aes(x, y)) + 
      geom_point(size = 7, stroke = 1, aes(shape = shape, fill = fill)) + 
      scale_fill_manual(values = c("red", "green", "yellow", "#990000", "#ff6600"), guide = FALSE) +
      scale_shape_manual(values = c(21, 22), guide = FALSE)+
      theme(axis.text.y   = element_blank(),
            axis.text.x   = element_blank(),
            axis.title.y  = element_blank(),
            axis.title.x  = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.border = element_rect(colour = "black", fill=NA, size=5)
            
      ) +
      geom_point(x = rx, y = ry, fill = "black", shape = 21, size = 4) + 
      geom_path(data = man_data, size = 1) + 
      scale_x_continuous(limits = c(min(x), max(x))) +
      scale_y_continuous(limits = c(min(y), max(y)))
      output$plot1 <- renderPlot({
        p
      })
    assign("x", x, .GlobalEnv)
    assign("y", y, .GlobalEnv)
    assign("rx", rx, .GlobalEnv)
    assign("ry", ry, .GlobalEnv)
    assign("data", data, .GlobalEnv)
    assign("man_data", man_data, .GlobalEnv)
    assign("p", p, .GlobalEnv)
  }
  
  init_app()
  
  observeEvent(input$reset, {
    init_app()
  })
  
  
  output$skip_n_ui <- renderUI({
    sliderInput("skip_n", "Sample every Xth case:", min = 0, max = floor(100/input$n), value = 1, step = 1, width = 200)
  })

  output$num_clusters_ui <- renderUI({
    sliderInput("num_clusters", "Number of clusters:", min = 0, max = 40, value = 8, step = 1, width = 200)
  })
  
  output$multistage_ui <- renderUI({
    sliderInput("multistage", "Random sample size:", min = 0, max = 100, value = 20, step = 1, width = 200)
  })
  
  output$stratified_ui <- renderUI({
    selectInput("stratified", "Stratify by:", c("Shape", "Color"), selectize = TRUE, width = 200)
  })
  
  output$color1_ui <- renderUI({
    numericInput("color1", "Red:", value = 0, min = 0, max = sum(data$fill == 1), step = 1, width = 70)
  })
  output$color2_ui <- renderUI({
    numericInput("color2", "Green:", value = 0, min = 0, max = sum(data$fill == 2), step = 1, width = 70)
  })
  output$color3_ui <- renderUI({
    numericInput("color3", "Yellow:", value = 0, min = 0, max = sum(data$fill == 3), step = 1, width = 70)
  })
  output$color4_ui <- renderUI({
    numericInput("color4", "Bordeaux:", value = 0, min = 0, max = sum(data$fill == 4), step = 1, width = 70)
  })
  output$color5_ui <- renderUI({
    numericInput("color5", "Orange:", value = 0, min = 0, max = sum(data$fill == 5), step = 1, width = 70)
  })
  output$shape1_ui <- renderUI({
    numericInput("shape1", "Circle:", value = 0, min = 0, max = sum(data$shape == 1), step = 1, width = 70)
  })
  output$shape2_ui <- renderUI({
    numericInput("shape2", "Square:", value = 0, min = 0, max = sum(data$shape == 2), step = 1, width = 70)
  })
  
  sample_dat_rv <- reactiveVal(NA)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$do_sample, {
    switch(input$sample_type,
           "Convenience" = {
             distances <- as.matrix(dist(matrix(c(rx, x, ry, y), ncol = 2)))[-1,1]
             sample_dat <- data[order(distances)[1:input$n], ]
             sample_plot <- p +
               geom_point(data = sample_dat, size = 7, stroke = 2, aes(shape = shape, fill = fill)) +
               geom_point(x = rx, y = ry, fill = "black", shape = 21, size = 4) + 
               geom_path(data = man_data, size = 1)+
               geom_path(data = circleFun(c(rx,ry), 2*distances[order(distances)[input$n]], npoints = 40), linetype = 2)
             sample_dat_rv(sample_dat)
             },
           "Snowball" = {
             distances <- as.matrix(dist(matrix(c(rx, x, ry, y), ncol = 2)))[-1,1]
             snowball <- rep((as.numeric(names(which.min(distances)))-1), input$n)
             prev_x <- x[snowball[1]]
             prev_y <- y[snowball[1]]
             for(i in 2:input$n){
               snowball[i] <- as.numeric(names(which.min(as.matrix(dist(matrix(c(prev_x, x, prev_y, y), ncol = 2)))[-1,1][-unique(snowball)])))-1
               prev_x <- x[snowball[i]]
               prev_y <- y[snowball[i]]
             }
             sample_dat <- data[snowball, ]
             sample_plot <- p + 
               geom_path(data = rbind(c(rx, ry, NA, NA), sample_dat), linetype = 2) +
               geom_point(data = sample_dat, size = 7, stroke = 2, aes(shape = shape, fill = fill))+
               geom_point(x = rx, y = ry, fill = "black", shape = 21, size = 4) + 
               geom_path(data = man_data, size = 1)
             sample_dat_rv(sample_dat)
           },
           "Purposive" = {
             n_conv <- 1:(input$n*input$conv_percent)
             n_purposive <- (round(input$n*input$conv_percent)+1):input$n
             distances <- as.matrix(dist(matrix(c(rx, x, ry, y), ncol = 2)))[-1,1]
             sample_dat <- data[order(distances)[1:input$n], ]
             sample_dat$colour <- "a"
             sample_dat[n_purposive, ] <- NA
             purposive <- c(order(distances)[n_conv], rep(NA, length(n_purposive)))
             for(i in n_purposive){
               sample_cells <- table(sample_dat[ , c(3,4), drop = F])
               available_combinations <- which(sample_cells < table(data[-na.omit(purposive), c(3,4), drop = F]))
               select_from_these <- which(sample_cells == min(sample_cells[available_combinations]))
               select_from_these <- available_combinations[available_combinations %in% select_from_these]
               more_of_this_cell <- sample(select_from_these, 1)
               more_of_this_fill <- ceiling(more_of_this_cell/2)
               more_of_this_shape <- ifelse(more_of_this_cell%%2 == 0, 2, 1)
               distances <- as.matrix(dist(matrix(c(rx, x, ry, y), ncol = 2)))[-1,1]
               purposive[i] <- as.numeric(names(which.min(distances[data$shape == more_of_this_shape & data$fill == more_of_this_fill & !(names(distances) %in% as.character(purposive+1))])))-1
               sample_dat[i, ] <- c(data[purposive[i], ], "b")
             }
             distances <- as.matrix(dist(matrix(c(rx, sample_dat$x, ry, sample_dat$y), ncol = 2)))[-1,1]
             sample_plot <- p + 
               geom_point(data = sample_dat, size = 7, stroke = 2, aes(shape = shape, fill = fill, colour = colour)) + 
               scale_color_manual(values = c("black", "purple"), guide = FALSE) + 
               geom_path(data = circleFun(c(rx,ry), 2*distances[order(distances)[input$n]], npoints = 40), linetype = 2) +
               geom_point(x = rx, y = ry, fill = "black", shape = 21, size = 4) + 
               geom_path(data = man_data, size = 1)
             sample_dat_rv(sample_dat)

           },
           "Systematic" = {
             distances <- as.matrix(dist(matrix(c(rx, x, ry, y), ncol = 2)))[-1,1]
             sample_dat <- data[order(distances)[seq.int(1, input$n*input$skip_n, by = input$skip_n)], ]
             sample_plot <- p +
               geom_point(data = sample_dat, size = 7, stroke = 2, aes(shape = shape, fill = fill)) +
               geom_point(x = rx, y = ry, fill = "black", shape = 21, size = 4) + 
               geom_path(data = man_data, size = 1)+
               geom_path(data = circleFun(c(rx,ry), 2*distances[order(distances)[input$n*input$skip_n]], npoints = 40), linetype = 2)
             sample_dat_rv(sample_dat)
           },
           "Simple random" = {
             sample_dat <- data[sample.int(nrow(data), input$n), ]

             sample_plot <- p +
               geom_point(data = sample_dat, size = 7, stroke = 2, aes(shape = shape, fill = fill)) +
               geom_point(x = rx, y = ry, fill = "gray70", colour = "gray70", shape = 21, size = 4) + 
               geom_path(data = man_data, size = 1, colour = "gray70")
             sample_dat_rv(sample_dat)
           },
           "Stratified" = {
             distances <- as.matrix(dist(matrix(c(rx, x, ry, y), ncol = 2)))[-1,1]
             sample_dat <- data[order(distances), ]
             select_these <- NULL
             if(input$stratified == "Shape"){
               select_these <- c(
                 if(input$shape1 > 0){ which(sample_dat$shape == 1)[1:input$shape1]},
                 if(input$shape2 > 0){ which(sample_dat$shape == 2)[1:input$shape2]}
                 )
               sample_dat <- sample_dat[select_these, ]
             } else {
               select_these <- c(if(input$color1 > 0){ which(sample_dat$fill == 1)[1:input$color1]},
                                 if(input$color2 > 0){ which(sample_dat$fill == 2)[1:input$color2]},
                                 if(input$color3 > 0){ which(sample_dat$fill == 3)[1:input$color3]},
                                 if(input$color4 > 0){ which(sample_dat$fill == 4)[1:input$color4]},
                                 if(input$color5 > 0){ which(sample_dat$fill == 5)[1:input$color5]}
               )
               sample_dat <- sample_dat[select_these, ]
             }
             #sample_dat <- data[order(distances)[1:input$n], ]
             sample_plot <- p +
               geom_point(data = sample_dat, size = 7, stroke = 2, aes(shape = shape, fill = fill)) +
               geom_point(x = rx, y = ry, fill = "black", shape = 21, size = 4) + 
               geom_path(data = man_data, size = 1)+
               geom_path(data = circleFun(c(rx,ry), 2*sort(distances)[max(select_these)], npoints = 40), linetype = 2)
             sample_dat_rv(sample_dat)
           },
           "Cluster" = {
             x_bins <- seq(min(data$x), max(data$x), length.out = 9)
             y_bins <- seq(min(data$y), max(data$y), length.out = 6)
            
             square <- ((cut(data$y, 5, labels = FALSE)-1)*8) + cut(data$x, 8, labels = FALSE)
             sample_squares <- sample.int(40, input$num_clusters)
             
             sample_dat <- data[square %in% sample_squares, ]
             
             square_dat1 <- data.frame(x = rep(x_bins[(((sample_squares-1) %% 8)+1)], 2),
                                      xend = rep(x_bins[(((sample_squares-1) %% 8)+2)], 2),
                                      y = c(y_bins[((floor((sample_squares-1) / 8))+1)], y_bins[((floor((sample_squares-1) / 8))+2)]),
                                      yend = c(y_bins[((floor((sample_squares-1) / 8))+1)], y_bins[((floor((sample_squares-1) / 8))+2)])
             )
             square_dat1 <- square_dat1[!(duplicated(square_dat1) | duplicated(square_dat1, fromLast = TRUE)), ]
             square_dat2 <- data.frame(x = c(x_bins[(((sample_squares-1) %% 8)+1)], 
                                             x_bins[(((sample_squares-1) %% 8)+2)]),
                                       xend = c(x_bins[(((sample_squares-1) %% 8)+1)], 
                                                x_bins[(((sample_squares-1) %% 8)+2)]),
                                       y = rep(y_bins[((floor((sample_squares-1) / 8))+1)], 2),
                                       yend = rep(y_bins[((floor((sample_squares-1) / 8))+2)], 2))
             square_dat2 <- square_dat2[!(duplicated(square_dat2) | duplicated(square_dat2, fromLast = TRUE)), ]
             sample_plot <- p +
               geom_point(data = sample_dat, size = 7, stroke = 2, aes(shape = shape, fill = fill)) +
               geom_point(x = rx, y = ry, fill = "gray70", colour = "gray70", shape = 21, size = 4) + 
               geom_path(data = man_data, size = 1, colour = "gray70") +
               geom_hline(yintercept = y_bins, linetype = 2, colour = "gray70") + 
               geom_vline(xintercept = x_bins, linetype = 2, colour = "gray70") +
               geom_segment(data = square_dat1, aes(x= x, y=y, xend= xend, yend = yend), linetype = 1, size = 1) +
               geom_segment(data = square_dat2, aes(x= x, y=y, xend= xend, yend = yend), linetype = 1, size = 1)
             sample_dat_rv(sample_dat)
           },
           "Multistage" = {
             x_bins <- seq(min(data$x), max(data$x), length.out = 9)
             y_bins <- seq(min(data$y), max(data$y), length.out = 6)
             
             square <- ((cut(data$y, 5, labels = FALSE)-1)*8) + cut(data$x, 8, labels = FALSE)
             sample_squares <- sample.int(40, input$num_clusters)
             
             sample_dat <- data[square %in% sample_squares, ]
             if(nrow(sample_dat > input$multistage)){
               sample_dat <- sample_dat[sample.int(nrow(sample_dat), input$multistage), ] 
             }
             
             square_dat1 <- data.frame(x = rep(x_bins[(((sample_squares-1) %% 8)+1)], 2),
                                       xend = rep(x_bins[(((sample_squares-1) %% 8)+2)], 2),
                                       y = c(y_bins[((floor((sample_squares-1) / 8))+1)], y_bins[((floor((sample_squares-1) / 8))+2)]),
                                       yend = c(y_bins[((floor((sample_squares-1) / 8))+1)], y_bins[((floor((sample_squares-1) / 8))+2)])
             )
             square_dat1 <- square_dat1[!(duplicated(square_dat1) | duplicated(square_dat1, fromLast = TRUE)), ]
             square_dat2 <- data.frame(x = c(x_bins[(((sample_squares-1) %% 8)+1)], 
                                             x_bins[(((sample_squares-1) %% 8)+2)]),
                                       xend = c(x_bins[(((sample_squares-1) %% 8)+1)], 
                                                x_bins[(((sample_squares-1) %% 8)+2)]),
                                       y = rep(y_bins[((floor((sample_squares-1) / 8))+1)], 2),
                                       yend = rep(y_bins[((floor((sample_squares-1) / 8))+2)], 2))
             square_dat2 <- square_dat2[!(duplicated(square_dat2) | duplicated(square_dat2, fromLast = TRUE)), ]
             sample_plot <- p +
               geom_point(data = sample_dat, size = 7, stroke = 2, aes(shape = shape, fill = fill)) +
               geom_point(x = rx, y = ry, fill = "gray70", colour = "gray70", shape = 21, size = 4) + 
               geom_path(data = man_data, size = 1, colour = "gray70") +
               geom_hline(yintercept = y_bins, linetype = 2, colour = "gray70") + 
               geom_vline(xintercept = x_bins, linetype = 2, colour = "gray70") +
               geom_segment(data = square_dat1, aes(x= x, y=y, xend= xend, yend = yend), linetype = 1, size = 1) +
               geom_segment(data = square_dat2, aes(x= x, y=y, xend= xend, yend = yend), linetype = 1, size = 1)
             sample_dat_rv(sample_dat)
           })
    
    output$plot1 <- renderPlot({
      sample_plot
    })
    
    
  })

  observeEvent(input$plot_descriptives, {
    if(input$plot_shape){
      if(input$plot_color){
        sample_props <- prop.table(table(sample_dat_rv()[ , c(3, 4)]))
        pop_props <- prop.table(table(data[ , c(3, 4)]))
      } else {
        sample_props <- prop.table(table(sample_dat_rv()[ , 3]))
        pop_props <- prop.table(table(data[ , 3]))
      }
    } else {
      sample_props <- prop.table(table(sample_dat_rv()[ , 4]))
      pop_props <- prop.table(table(data[ , 4]))
    }
       
    descriptive_freqs <- data.frame(x = rep(1:length(pop_props), 2), 
                                    y = c(as.vector(sample_props),
                                             as.vector(pop_props)),
                                    fill = rep(c("Sample", "Population"), each = length(pop_props)),
                                    shape = rep(NA, length(pop_props)))

      if(input$plot_shape){
        if(input$plot_color){
          descriptives_plot <- ggplot(data = NULL, aes(x = x, y = y, shape = shape, fill = fill)) + 
            geom_point(data = data.frame(x = 1:10, y = -1/20*max(descriptive_freqs$y), fill = factor(rep(1:5, each = 2)), shape = factor(rep(c(1, 2), 5))), size = 7, stroke = 1) + 
            scale_shape_manual(values = c(21, 22)) +
            geom_bar(data = descriptive_freqs, stat = "identity", position = "dodge") +
            scale_fill_manual(breaks = c("Population", "Sample"), values = c("red", "green", "yellow", "#990000", "#ff6600", "#F8766D", "#00BFC4"))
          } else {
          descriptives_plot <- ggplot(data = NULL, aes(x = x, y = y, shape = shape, fill = fill)) + 
            geom_point(data = data.frame(x = 1:2, y = -1/20*max(descriptive_freqs$y), fill = NA, shape = factor(c(1, 2))), size = 7, stroke = 1) +
            scale_shape_manual(values = c(21, 22)) +
            geom_bar(data = descriptive_freqs, stat = "identity", position = "dodge") +
            scale_fill_manual(breaks = c("Population", "Sample"), values = c("#F8766D", "#00BFC4"))
        }
      } else {
        descriptives_plot <- ggplot(data = NULL, aes(x = x, y = y, shape = shape, fill = fill)) + 
        geom_point(data = data.frame(x = 1:5, y = -1/20*max(descriptive_freqs$y), fill = factor(c(1:5)), shape = factor(rep(1, 5))), size = 7, stroke = 1) +
          scale_shape_manual(values = c(21, 22)) +
          geom_bar(data = descriptive_freqs, stat = "identity", position = "dodge") +
          scale_fill_manual(breaks = c("Population", "Sample"), values = c("red", "green", "yellow", "#990000", "#ff6600", "#F8766D", "#00BFC4"))
      }
      descriptives_plot <- descriptives_plot +
      guides(shape = FALSE, colour = FALSE) +
      geom_hline(yintercept = 0) +
      ylab("Frequency") +
      theme(legend.title = element_blank(),
            axis.text.x   = element_blank(),
            axis.title.x  = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.border = element_blank()
      )
    if(input$plot_perc){
      descriptives_plot <- descriptives_plot + 
        geom_text(data = descriptive_freqs, aes(x= x, y = y+.002, label=paste0(formatC(y*100, 0, format = "f"), "%")), 
                  vjust = 0, 
                  position = position_dodge(width=1), size = 3)
    }
      
    
    output$plot_descriptives <- renderPlot({
      descriptives_plot
    })
  })
  

}

