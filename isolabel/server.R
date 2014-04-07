library(shiny)
library(lubridate)
library(stringr)
library(datasets)
library(ggplot2)
library(grid)
library(plyr)
library(reshape2)

# isotope functions
iso.RtoF<-function(ratio) ratio/(1+ratio) # ratio to fraction
iso.FtoR<-function(frac) frac/(1-frac) # fraction to ratio
iso.RtoD<-function(ratios, refs) ratios/refs - 1 #fraction to delta value (NOT 1000x!!)
iso.RtoDx<-function(ratios, refs) iso.RtoD(ratios, refs)*1000 #fraction to delta value (1000x)
iso.DtoR<-function(deltas, refs) (deltas+1) * refs #delta to fraction (delta NOT 1000x!!)
iso.DxtoR<-function(deltas, refs) iso.DtoR(deltas/1000, refs) #delta to fraction (deltas are 1000x)
iso.FtoD<-function(fracs, refs) iso.RtoD(iso.FtoR(fracs), refs) #fractions to delta values (deltas NOT 1000x)
iso.FtoDx<-function(fracs, refs) iso.FtoD(fracs, refs)*1000 #fractions to delta values  (deltas are 1000x)
iso.DtoF<-function(deltas, refs) iso.RtoF(iso.DtoR(deltas, refs)) #delta values to fractions (deltas NOT 1000x)
iso.DxtoF<-function(deltas, refs) iso.DtoF(deltas/1000, refs) #delta values to fractions (deltas are 1000x)
iso.DtoA<-function(deltas1, deltas2) (deltas1 + 1)/(deltas2 + 1) #delta notation to alpha fractionation factor (not 1000X)
iso.DxtoA<-function(deltas1, deltas2) iso.DtoA(deltas1/1000, deltas2/1000) #delta (in permil, i.e. 1000x) to alpha
iso.AtoEx<-function(alphas) (alphas-1)*1000 #alpha to epsilon (as permill, i.e. 1000x)
iso.ExtoA<-function(eps) eps/1000 + 1 #epsilon in permil to alpha
iso.DxtoEx<-function(deltas1, deltas2) iso.AtoEx(iso.DxtoA(deltas1, deltas2)) # from delta notation (in permil, i.e. 1000x) to epsilon in permil

#' Constants used for isotopic reference values
c.R_nat <<- mutate(data.frame(
    minor = c("2H", "13C", "15N", "18O", "34S"),
    major = c("1H", "12C", "14N", "16O", "32S"),
    ratio = c(0.00015575, 0.011237, 0.003677, 0.0020052, 0.0045005),
    ref = c("VSMOW", "VPDB", "Air", "VSMOW", "CDT"),
    stringsAsFactors=F),
    abundance = iso.RtoF(ratio),
    label = paste0(minor, "/", major))

#' Duration labels to the closest large denominator (e.g. 86400s = ~24 hours)
#'@param ds vector of lubridate:::duration objects or time in seconds
duration_label <- function(ds) {
    sapply(ds, function(x) { 
        if (grepl("^[0-9\\.]+s$", (d <- duration(x, "seconds")))) return(paste0('~', round(as.numeric(d), 1), " seconds"))
        else return(str_extract(d, "~.[^\\)]*"))
    })
}

#' Labeling time as function of doubling time, desired isotopic enrichment and strength of label
#' @param dblt doubling times, a vector of lubridate:::duration objects
#' @param Ex_sample sample enrichment (in permil)
#' @param F_label labeling strength (in fractional abundance)
#' @param R_nat natural abundance of the labeled material (isotope ratio)
#' @return data frame with input columns and additional columns label.time, alpha and s
label_time <- function(dblt, Ex_sample, F_label, R_nat) {
    df <- expand.grid(dblt = dblt, Ex_sample = Ex_sample, F_label = F_label, R_nat = R_nat, stringsAsFactors = FALSE)
    mutate(df, 
           dblt.label = duration_label(dblt), # add labels
           alpha = iso.ExtoA(Ex_sample), # convert epsilon to alpha
           s = F_label / iso.RtoF(R_nat), # convert label to s (labeling strenth relative to natural abundance)
           labeling_time = as.numeric(dblt) * 1/log(2) * log((1 - s) / (alpha - s)), # calculate label time
           labeling_time.label = duration_label(labeling_time)) # label
}

#' Isotopic enrichment as a function of labeling time, doubling time and strength of label
#' @param time labeling times, a vector of lubridate:::duration objects
#' @param dblt doubling times, a vector of lubridate:::duration objects
#' @param F_label labeling strength (in fractional abundance)
#' @param R_nat natural abundance of the labeled material (isotope ratio)
label_strength <- function(time, dblt, F_label, R_nat) {
    df <- expand.grid(time = time, dblt = dblt, F_label = F_label, R_nat = R_nat, stringsAsFactors = FALSE)
    mutate(df,
           time.label = duration_label(time), # add labels
           dblt.label = duration_label(dblt), # add labels
           p = log(2)/as.numeric(dblt), # specific growth rate
           nat.abundance = iso.RtoF(R_nat), # natural abundance
           enrich.abundance = F_label - (F_label - nat.abundance) * exp(-p * as.numeric(time)), # predicted isotopic composition
           enrich.permil = iso.FtoDx(enrich.abundance, R_nat) # enrichment in permil
           )
}

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
    
    # Inputs processing ------------------------------------------------
    
    # doubling times
    dbltsInput <- reactive({
        # find doubling times to plot
        dblts <- c()
        dblts.labels <- c()
        for (i in 1:5) { 
            if (paste0("dblt", i) %in% names(input)) {
                value <- input[[paste0("dblt", i)]]
                unit <- paste0(c(input[[paste0("dblt", i, "_units")]]), if (value > 1) "s")
                dblt <- duration(as.integer(value), unit)
                if (! (dblt %in% dblts) ) { # only take unique dblts
                    dblts <- c(dblts, dblt)
                    dblts.labels <- c(dblts.labels, paste(value, unit))
                }
            } else 
                break
        }
        list(dblts = dblts, dblts.labels = dblts.labels)
    })
    
    # labeling strengths
    labelsInput <- reactive({
        data <- list()
        
        # natural abundance reference 
        data$nat <- subset(c.R_nat, minor == input$ref)
        
        # target enrichment
        if (input$targetType == 'permil')
            data$enrichPermil <- input$intensity_permil
        else 
            data$enrichPermil <- iso.FtoDx(input$intensity_F, data$nat$ratio)
        data$enrichF <- iso.DxtoF(data$enrichPermil, data$nat$ratio)
        
        # labels
        data$labels <- sapply(1:3, function(i) input[[paste0("label", i)]], simplify = T)
        data$labels.show <- 1:3 %in% as.integer(input$show)
        data$labels.error <- (data$labels <= data$enrichF)
        
        return(data)
    })
    
    # dataset 
    datasetInput <- reactive({
        dblts <- dbltsInput()
        data <- labelsInput()
        
        # labels
        show <- (which(data$labels.show & !data$labels.error))
        
        # errors
        if (length(show) == 0)
            stop("Please enable at least one valid labeling strength.")
        if (data$enrichF <= data$nat$abundance)
            stop("Please choose a higher target enrichment.")
        
        # labeling times for plot
        plot.df <- label_time(
            dblt = dblts$dblts, 
            Ex_sample = round(data$enrichPermil), 
            F_label = data$labels[show], 
            R_nat = data$nat$ratio)
        plot.df$Label <- factor(paste0(100*plot.df$F_label, "%")) # legend in %
        
        # enrichment for table
        table.df <- data.frame()
        for (label in data$labels[show]) {
            df <- subset(plot.df, F_label == label)
            table.df <- rbind(table.df, label_strength(
                time = duration(df$labeling_time, "seconds"), 
                dblt = duration(df$dblt, "seconds"), 
                F_label = unique(df$F_label),
                R_nat = unique(df$R_nat))
            )
        }
        
        # merge with labels
        table.df <- merge(table.df, data.frame(dblt = dblts$dblts, dblt.userlabel = paste0(dblts$dblts.labels, " doubling")), by = 'dblt')
        
        # adjust headers
        table.df <- mutate(
            table.df, 
            `Label` = paste0(100*F_label, "%"),
            `Incubation time [s]` = as.numeric(time), 
            `Incubation time` = time.label)
        
        if (input$tableDataType == "permil") {
            table.df$enrichment <- paste0(signif(table.df$enrich.permil, 2))
        } else {
            table.df$enrichment <- paste0(signif(100*table.df$enrich.abundance, 2))
        }
        
        # cast 
        table.df <- dcast(table.df, `Label` + `Incubation time [s]` + `Incubation time` ~ dblt.userlabel, value.var = "enrichment" )
        header_order <- match(paste0(dblts$dblts.labels, " doubling"), names(table.df))
        table.df <- table.df[c(1,3,header_order)]
        
        return(c(
            dblts, data, list(plot.df = plot.df, table.df = table.df)))
    })
    
    # plot
    plotInput <- reactive({
        data <- datasetInput()
        ggplot(data$plot.df,
               aes(x = dblt, y = labeling_time, fill = Label, colour = Label)) + 
            geom_segment(aes(x=0, xend=dblt, y=labeling_time, yend=labeling_time), 
                         arrow=arrow(length=unit(0.4,"cm"), ends="first", type="open", 
                                     angle=15), linetype=2) + 
            geom_segment(aes(x=dblt, xend=dblt, y=0, yend=labeling_time), 
                         arrow=arrow(length=unit(0.4,"cm"), type="open", angle=15), 
                         linetype=2) + 
            geom_line(linetype=1) + geom_point(colour="black", shape = 21, size=4) +
            scale_y_log10(
                expand = c(0, 0.1), breaks = unique(data$plot.df$labeling_time), 
                labels = unique(data$plot.df$labeling_time.label)) + 
            scale_x_log10(expand = c(0, 0.2), breaks = data$dblts, 
                          labels = data$dblts.labels) + 
            labs(title = paste0("Required labeling times for ", round(data$enrichPermil), 
                                " permil enrichment (", signif(data$enrichF * 100, 3),
                                "% ", input$ref, ")\n"), x = "Doubling time", y = "") + 
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
            theme(legend.position = "right")
    })
    
    
    # Outputs rendering ---------------------------------------------
    
    # natural abundance info output
    output$nat <- renderUI({ 
        nat <- subset(c.R_nat, minor == input$ref)
        paste0("Reference ", nat$label, " ratio: ", nat$ratio, " (", nat$ref, ")")
    })
    
    # labeling strengths messages output
    generate_label_msg <- function(i) {
        renderUI({
            data <- labelsInput()
            if (data$labels.error[i]) {
                return (paste0("Too small, can't possibly reach target enrichment (", signif(data$enrichF * 100, 3), "%)."))
            } else
                return ("")
        })
    }
    output$label1_message <- generate_label_msg(1)
    output$label2_message <- generate_label_msg(2)
    output$label3_message <- generate_label_msg(3)   
    
    # enrichment info output
    output$intensity_F_message <- renderUI({ 
        data <- labelsInput()
        if (data$enrichF <= data$nat$abundance)
            return(paste0("Too small, target enrichment must exceed natural abundance."))
        else
            return ("")
    })
    
    # rendering plot
    output$plot <- renderPlot({
        withProgress(session, min=1, max=5, {
            setProgress(message = 'Rendering plot ...')
            setProgress(value = 2)
            p <- plotInput()
            setProgress(value = 4)
            suppressWarnings(print(p))
            setProgress(value = 5)
        })
    })
    
    # saving plot
    output$downloadPlot <- downloadHandler(
        filename = function() { paste0('isotope_labeling_', input$ref, '.pdf') },
        content = function(file) {
            device <- function(..., version="1.4")
                grDevices::pdf(..., version=version)
            ggsave(file, plot = plotInput(), device = device)
        }
    )
    
    # rendering table
    output$table <- renderTable({
        withProgress(session, min=1, max=5, {    
            setProgress(message = 'Rendering table ...')
            setProgress(value = 2)
            data.table <- datasetInput()$table.df
            setProgress(value = 4)
            return(data.table)
        })
    }, digits = 0, include.rownames = F, align = rep('r', ncol(datasetInput()$table.df) + 1))
    
    # saving table 
    output$downloadTable <- downloadHandler(
        filename = function() { paste0('isotope_labeling_', input$ref, '.csv') },
        content = function(file) { write.csv(datasetInput()$table.df, file) }
    )
    
})
