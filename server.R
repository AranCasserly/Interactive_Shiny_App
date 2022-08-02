# server -----------------------------------------------------------------------

server <- function(input, output, session) {
    #Pop up ------------
    # Show pop up on start up
    shinyalert(title = "About Assignment 2 App",
               "This R Shiny app was created to observe and analyse the Maths Ed dataset for the STAT40830 Assignment 2.\n\n This application can generate:\nAn interactive table of uploaded data\nAn interactive violin/boxplot of results achieved\nAn interactive scatterplot of result achieved for each gender based on VLE usage\nAn interactive bargraph of results achieved for each gender based on their big five personality factors\nAn interactive bargraph of results achieved for each gender based on their approaches to learning\n\nAll plots have interactive labeling and can be exported as a PNG file", 
               animation = TRUE)
    # table -----------------------------------------------       
    output$table <- DT::renderDataTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        maths_ed <- read_excel(inFile$datapath,  col_names = TRUE)
        # change gender names
        maths_ed$Gender_0F_1M <-ifelse(maths_ed$Gender_0F_1M=="1","Male","Female")
        # generate datatable
        datatable(maths_ed, options = list(scrollX=TRUE, scrollCollapse=TRUE))
    })
    # Maths results (v plot) -----------------------------     
    output$boxplot <- renderPlotly({
        
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        maths_box <- read.xlsx(inFile$datapath, check.names = FALSE, header = TRUE, 1)
        
        # change male and female from 1 and 0
        maths_box$Gender_0F_1M <-ifelse(maths_box$Gender_0F_1M=="1","Male","Female")
        maths_box$Gender_0F_1M = as.factor(maths_box$Gender_0F_1M)
        maths_box = na.omit(maths_box)
        
        
        # Custom theme
        blank_theme <- theme_minimal()+
            theme(
                # Lots of blank elements to the graph (looks clean)
                panel.border = element_blank(),
                panel.grid=element_blank(),
                # Only include y axis
                panel.grid.major.y = element_line(colour="gray73",size = 0.5),
                axis.ticks = element_blank(),
                # Title elements
                plot.title=element_text(size=14, face="bold", hjust = 0.5)
            )
        
        # violinplot of results achieved
        box = ggplot(maths_box, aes(x = Gender_0F_1M, y = Maths_Achievement,fill = Gender_0F_1M)) +
            geom_boxplot(width=0.1, show.legend = FALSE) +
            # bw smoothing set to 2 and change transparency
            geom_violin(bw = 2, show.legend = FALSE, alpha=0.2) +
            blank_theme +
            scale_x_discrete(limits=rev) +
            labs(x = input$BoxX, y=input$BoxY, fill= "Gender") +
            ggtitle(input$Boxtit)
        ggplotly(box)
    })
    # VLE usage (s plot) ----------------------
    output$plot <- renderPlotly({
        
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        maths_ed <- read.xlsx(inFile$datapath, check.names = FALSE, header = TRUE, 1)
        # set grade ranges of 10% intervals as bins
        maths_ed$Grade_Range = cut(maths_ed$Maths_Achievement, breaks = c(30,40,50,60,70,80,90,100))
        maths_ed$Grade_Range = factor(maths_ed$Grade_Range, levels = rev(levels(maths_ed$Grade_Range)))
        maths_ed = na.omit(maths_ed)
        # change gender names
        maths_ed$Gender_0F_1M <-ifelse(maths_ed$Gender_0F_1M=="1","Male","Female")
        maths_ed$Gender_0F_1M = as.factor(maths_ed$Gender_0F_1M)
        
        
        # only males
        Male = maths_ed %>%
            subset(Gender_0F_1M == "Male")
        # only female
        Female = maths_ed %>%
            subset(Gender_0F_1M == "Female")
        Female = na.omit(Female)
        # data input
        data_choice <- reactive({
            # Return the appropriate data source depending on male or female
            if (input$choice == "Male") {
                data <- Male
            } else if (input$choice == "Female") {
                data <- Female
            } 
            return(data)
        })
        
        # Custom theme
        blank_theme <- theme_minimal()+
            theme(
                # Lots of blank elements to the graph (looks clean)
                panel.border = element_blank(),
                panel.grid=element_blank(),
                # Only include y axis
                panel.grid.major.y = element_line(colour="gray73",size = 0.5),
                axis.ticks = element_blank(),
                # Title elements
                plot.title=element_text(size=14, face="bold", hjust = 0.5)
            )
        
        ggplot(data = data_choice(), aes(x = Maths_Achievement , y =VLE_Use, colour = Grade_Range)) +
            geom_point() +
            blank_theme +
            labs(x = input$scattX, y = input$scattY, colour = "Maths Achievement") +
            ggtitle(input$scattit) +
            scale_color_discrete(labels=c("(90,100]" = "90-100%",
                                          "(80,90]" = "80-90%", 
                                          "(70,80]" =  "70-80%", 
                                          "(60,70]" = "60-70%", 
                                          "(50,60]" = "50-60%", 
                                          "(40,50]" = "40-50%", 
                                          "(30,40]" = "30-40%"))
        
        
    })
    # Mini-IPIP (b plot) -----------       
    output$histagram <- renderPlotly({
        
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        maths_ed <- read.xlsx(inFile$datapath, check.names = FALSE, header = TRUE, 1)
        
        # new data making ranges of results
        maths_ed$Grade_Range = cut(maths_ed$Maths_Achievement, breaks = c(30,40,50,60,70,80,90,100))
        maths_ed$Grade_Range = factor(maths_ed$Grade_Range, levels = rev(levels(maths_ed$Grade_Range)))
        maths_ed = na.omit(maths_ed)
        
        # Extraversion breaks
        maths_ed$Extraversion = cut(maths_ed$Extraversion, breaks = c(0,5,10,15,20))
        # Agreeableness breaks
        maths_ed$Agreeableness = cut(maths_ed$Agreeableness, breaks = c(0,5,10,15,20))
        # Conscientiousness breaks
        maths_ed$Conscientiousness = cut(maths_ed$Conscientiousness, breaks = c(0,5,10,15,20))
        # Neuroticism breaks
        maths_ed$Neuroticism = cut(maths_ed$Neuroticism, breaks = c(0,5,10,15,20))
        # Openness breaks
        maths_ed$Openness = cut(maths_ed$Openness, breaks = c(0,5,10,15,20))
        
        
        maths_ed$Gender_0F_1M <-ifelse(maths_ed$Gender_0F_1M=="1","Male","Female")
        maths_ed$Gender_0F_1M = as.factor(maths_ed$Gender_0F_1M)
        
        
        
        # only males
        Male = maths_ed %>%
            subset(Gender_0F_1M == "Male")
        Male = na.omit(Male)
        # only female
        Female = maths_ed %>%
            subset(Gender_0F_1M == "Female")
        Female = na.omit(Female)
        # data input
        data_choice <- reactive({
            # Return the appropriate data source depending on male or female
            if (input$Bchoice == "Male") {
                data <- Male
            } else if (input$Bchoice == "Female") {
                data <- Female
            } 
            return(data)
        })
        
        # bin input
        IPIP_Range <- reactive({
            if (input$Bchoice == "Male" & input$bin == "Extraversion") {
                x <- Male$Extraversion
            } 
            else if (input$Bchoice == "Male" & input$bin == "Agreeableness") {
                x <- Male$Agreeableness
            }
            else if (input$Bchoice == "Male" & input$bin == "Conscientiousness") {
                x <- Male$Conscientiousness
            }
            else if (input$Bchoice == "Male" & input$bin == "Neuroticism") {
                x <- Male$Neuroticism
            }
            else if (input$Bchoice == "Male" & input$bin == "Openness") {
                x <- Male$Openness
            }
            else if (input$Bchoice == "Female" & input$bin == "Extraversion") {
                x <- Female$Extraversion
            }
            else if (input$Bchoice == "Female" & input$bin == "agree_bin") {
                x <- Female$agree_bin
            }
            else if (input$Bchoice == "Female" & input$bin == "Conscientiousness") {
                x <- Female$Conscientiousness
            }
            else if (input$Bchoice == "Female" & input$bin == "Neuroticism") {
                x <- Female$Neuroticism
            }
            else if (input$Bchoice == "Female" & input$bin == "Openness") {
                x <- Female$Openness
            }
            return(x)
        })
        
        
        # Common maths legend change
        prec_change = scale_fill_discrete(labels=c("(90,100]" = "90-100%",
                                                   "(80,90]" = "80-90%", 
                                                   "(70,80]" =  "70-80%", 
                                                   "(60,70]" = "60-70%", 
                                                   "(50,60]" = "50-60%", 
                                                   "(40,50]" = "40-50%", 
                                                   "(30,40]" = "30-40%"))
        
        bin_change = scale_x_discrete(labels=c("(0,5]" = "0 - 5",
                                               "(5,10]" = "5 - 10",
                                               "(10,15]" = "10 - 15",
                                               "(15,20]" = "15 - 20"))
        
        # Custom theme
        blank_theme <- theme_minimal()+
            theme(
                # Lots of blank elements to the graph (looks clean)
                panel.border = element_blank(),
                panel.grid=element_blank(),
                # Only include y axis
                panel.grid.major.y = element_line(colour="gray73",size = 0.5),
                axis.ticks = element_blank(),
                # Title elements
                plot.title=element_text(size=14, face="bold", hjust = 0.5)
            )
        
        # barplot
        mini = ggplot(data = data_choice(), aes(x = IPIP_Range(), fill = Grade_Range)) +
            geom_bar() +
            blank_theme +
            ggtitle(input$bartit) +
            labs(x = input$barX, y = "Count", fill = "Maths Achievement (%)") +
            theme(axis.text.x = element_text(angle = 45, size = 8, hjust = 1)) +
            prec_change +
            bin_change
        
        ggplotly(mini)
    })
    # Biggs (b plot) ----------- 
    output$biggs <- renderPlotly({
        
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        maths_ed <- read.xlsx(inFile$datapath, check.names = FALSE, header = TRUE, 1)
        
        # new data making ranges of results
        maths_ed$Grade_Range = cut(maths_ed$Maths_Achievement, breaks = c(30,40,50,60,70,80,90,100))
        maths_ed$Grade_Range = factor(maths_ed$Grade_Range, levels = rev(levels(maths_ed$Grade_Range)))
        maths_ed = na.omit(maths_ed)
        
        # DeepMotive breaks
        maths_ed$DeepMotive = cut(maths_ed$DeepMotive, breaks = c(0,5,10,15,20))
        # DeepStrategy breaks
        maths_ed$DeepStrategy = cut(maths_ed$DeepStrategy, breaks = c(0,5,10,15,20))
        # SurfaceMotive breaks
        maths_ed$SurfaceMotive = cut(maths_ed$SurfaceMotive, breaks = c(0,5,10,15,20))
        # SurfaceStrategy breaks
        maths_ed$SurfaceStrategy = cut(maths_ed$SurfaceStrategy, breaks = c(0,5,10,15,20))
        
        
        maths_ed$Gender_0F_1M <-ifelse(maths_ed$Gender_0F_1M=="1","Male","Female")
        maths_ed$Gender_0F_1M = as.factor(maths_ed$Gender_0F_1M)
        
        
        
        # only males
        Male = maths_ed %>%
            subset(Gender_0F_1M == "Male")
        Male = na.omit(Male)
        # only female
        Female = maths_ed %>%
            subset(Gender_0F_1M == "Female")
        Female = na.omit(Female)
        # data input
        data_choice <- reactive({
            # Return the appropriate data source depending on male or female
            if (input$Bchoice2 == "Male") {
                data <- Male
            } else if (input$Bchoice2 == "Female") {
                data <- Female
            } 
            return(data)
        })
        
        # bin input
        Biggs_Range <- reactive({
            if (input$Bchoice2 == "Male" & input$bin2 == "DeepMotive") {
                x <- Male$DeepMotive
            } 
            else if (input$Bchoice2 == "Male" & input$bin2 == "DeepStrategy") {
                x <- Male$DeepStrategy
            }
            else if (input$Bchoice2 == "Male" & input$bin2 == "SurfaceMotive") {
                x <- Male$SurfaceMotive
            }
            else if (input$Bchoice2 == "Male" & input$bin2 == "SurfaceStrategy") {
                x <- Male$SurfaceStrategy
            }
            else if (input$Bchoice2 == "Female" & input$bin2 == "DeepMotive") {
                x <- Female$DeepMotive
            }
            else if (input$Bchoice2 == "Female" & input$bin2 == "DeepStrategy") {
                x <- Female$DeepStrategy
            }
            else if (input$Bchoice2 == "Female" & input$bin2 == "SurfaceMotive") {
                x <- Female$SurfaceMotive
            }
            else if (input$Bchoice2 == "Female" & input$bin2 == "SurfaceStrategy") {
                x <- Female$SurfaceStrategy
            }
            return(x)
        })
        
        
        # Common maths legend change
        prec_change = scale_fill_discrete(labels=c("(90,100]" = "90-100%",
                                                   "(80,90]" = "80-90%", 
                                                   "(70,80]" =  "70-80%", 
                                                   "(60,70]" = "60-70%", 
                                                   "(50,60]" = "50-60%", 
                                                   "(40,50]" = "40-50%", 
                                                   "(30,40]" = "30-40%"))
        
        bin_change = scale_x_discrete(labels=c("(0,5]" = "0 - 5",
                                               "(5,10]" = "5 - 10",
                                               "(10,15]" = "10 - 15",
                                               "(15,20]" = "15 - 20"))
        
        # Custom theme
        blank_theme <- theme_minimal()+
            theme(
                # Lots of blank elements to the graph (looks clean)
                panel.border = element_blank(),
                panel.grid=element_blank(),
                # Only include y axis
                panel.grid.major.y = element_line(colour="gray73",size = 0.5),
                axis.ticks = element_blank(),
                # Title elements
                plot.title=element_text(size=14, face="bold", hjust = 0.5)
            )
        
        # barplot
        magic = ggplot(data = data_choice(), aes(x = Biggs_Range(), fill = Grade_Range)) +
            geom_bar() +
            blank_theme +
            ggtitle(input$bartit2) +
            labs(x = input$barX2, y = "Count", fill = "Maths Achievement (%)") +
            theme(axis.text.x = element_text(angle = 45, size = 8, hjust = 1)) +
            prec_change +
            bin_change
        
        ggplotly(magic)
        
    })
    
    
}
