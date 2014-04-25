library(shiny)
library(isotopia)
library(lubridate)
library(stringr)
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

#' Register isotope reference materials 
use_permil(TRUE)
register_standard(ratio(`2H` = 0.00015575, major = "1H", compound = "VSMOW"))
register_standard(ratio(`13C` = 0.011237, major = "12C", compound = "VPDB"))
register_standard(ratio(`15N` = 0.003677, major = "14N", compound = "Air"))
register_standard(ratio(`18O` = 0.0020052, major = "16O", compound = "VSMOW"))
register_standard(ratio(`34S` = 0.0045005, major = "32S", compound = "CDT"))

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
#' 
#' Formula: label_time = dblt / log(2) * log [(F_spike - F_natural) / (F_spike - F_target)]
#' 
#' @param dblt doubling times - a vector of lubridate:::duration objects
#' @param target enrichment - an isotope value object (usually abundance or delta value) 
#' @param spike - strength of label, another isotope value object (usually abundance but delta value works too)
#' @param natural - natural abundance (ratio or abundance)
#' @return data frame with input columns and additional columns target.ab, spike.ab (the abundance objects) and labeling_time
#' @note the conversion to an abundance value if coming from delta target or spike requires
#' the delta values to have their ref_ratio set (or the standard registered) and is only 100%
#' accurate in 2 isotope systems (othewise some error introduced when converting ratio to abundance
#' without taking the other minor isotopes into consideration)
label_time <- function(dblt, target, spike, natural) {
    df <- expand.grid(dblt = dblt, target = target, spike = spike, natural = natural, stringsAsFactors = FALSE)
    df <- mutate(df, 
           dblt.label = duration_label(dblt), # add labels
           target.ab = as.abundance(target), # convert to abundance 
           spike.ab = as.abundance(spike), # convert to abundance
           natural.ab = as.abundance(natural), # convert to abundance
           labeling_time = as.numeric(dblt)/log(2) * 
               log((as.value(spike.ab) - as.value(natural.ab)) / (as.value(spike.ab) - as.value(target.ab))), # calculate label time
           labeling_time.label = duration_label(labeling_time)) # label
    df
}

#' Isotopic enrichment as a function of labeling time, doubling time and strength of label
#' 
#' Formula: total = F_natural * exp(-p t) + F_spike * (1 - exp(-p t))
#' 
#' @param time labeling times, a vector of lubridate:::duration objects
#' @param dblt doubling times, a vector of lubridate:::duration objects
#' @param spike - strength of label, isotope value object (usually abundance but delta value works too)
#' @param natural - natural abundance (ratio or abundance)
label_strength <- function(time, dblt, spike, natural) {
    df <- expand.grid(time = time, dblt = dblt, spike.ab = as.abundance(spike), natural.ab = as.abundance(natural), stringsAsFactors = FALSE)
    mutate(df,
           time.label = duration_label(time), # add labels
           dblt.label = duration_label(dblt), # add labels
           p = log(2)/as.numeric(dblt), # specific growth rate
           decay = exp(-p * as.numeric(time)), # decay of old material
           total.ab = weight(natural.ab, decay) + weight(spike.ab, 1 - decay), # mass balance
           total.delta = as.delta(as.ratio(total.ab), ref_ratio = as.ratio(natural)) # permil value
           )
}


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
        data$nat <- get_standards(minor = input$ref)[[1]]
        
        # target enrichment
        if (input$targetType == 'permil')
            data$target <- delta(input$intensity_permil, ref_ratio = data$nat)
        else 
            data$target <- abundance(input$intensity_F)
        
        data$target <- isotopia:::update_iso(data$target, list(isoname = data$nat@isoname, major = data$nat@major)) # set minor isotope name
        data$target.ab <- as.abundance(as.ratio(data$target)) # abundance value
        data$target.delta <- as.delta(as.ratio(data$target), ref_ratio = data$nat) # delta value
        
        # weighted natural abundance
        wnat <- abundance(rep(as.value(data$nat), 3), weight = input$label.ref_vol * input$label.ref_conc)
        
        # weighted spikes
        data$strengths <- sapply(1:3, function(i) input[[paste0("label", i)]], simplify = T)
        data$vols <- sapply(1:3, function(i) input[[paste0("label", i, "_vol")]], simplify = T)
        data$concs <- sapply(1:3, function(i) input[[paste0("label", i, "_conc")]], simplify = T)
        wspikes <- abundance(data$strengths, weight = data$vols * data$concs)
        
        # spike sum
        data$spikes <- isotopia:::update_iso(abundance(as.value(wnat + wspikes)), list(isoname = data$nat@isoname, major = data$nat@major))
        data$spikes.show <- 1:3 %in% as.integer(input$show)
        data$spikes.error <- (data$spikes <= data$target.ab)
        
        return(data)
    })
    
    # dataset 
    datasetInput <- reactive({
        dblts <- dbltsInput()
        data <- labelsInput()
        
        # labels
        show <- (which(data$spikes.show & !data$spikes.error))
        
        # errors
        if (length(show) == 0)
            stop("Please enable at least one valid labeling strength.")
        if (data$target.ab <= as.abundance(data$nat))
            stop("Please choose a higher target enrichment.")
        
        # spikes data frame
        spikes.df <- mutate(data.frame(data[c("spikes", "vols", "concs", "strengths")]),
                         Label = paste0("Vol: ", vols, ", Conc: ", concs, ", Strength: ", 100*strengths, "%"))
        
        # labeling times for plot
        plot.df <- label_time(
            dblt = dblts$dblts, 
            target = data$target.ab, 
            spike = data$spikes[show],
            natural = data$nat)
        plot.df <- merge(plot.df, spikes.df, by.x = "spike", by.y = "spikes")
        plot.df$Label <- factor(plot.df$Label, levels = spikes.df$Label) # legend
        
        # data table for enrichment curves
        plot2.df <- label_strength(
            time = duration(c(1, seq(0, max(plot.df$labeling_time) * input$plot2Xzoom, length.out = 50)), "seconds"), 
            dblt = dblts$dblts, 
            spike = data$spikes[show],
            natural = data$nat)
        # merge with user defined dblts and define factors and reporting format depending on request
        plot2.df <- merge(plot2.df, data.frame(dblt = dblts$dblts, dblt.userlabel = dblts$dblts.labels), by = 'dblt')
        plot2.df <- merge(plot2.df, spikes.df, by.x = "spike.ab", by.y = "spikes")
        plot2.df <- mutate(plot2.df,
            DBLT = factor(dblt.userlabel, levels = dblts$dblts.labels),
            Spike = factor(Label, levels = spikes.df$Label),
            enrichment = as.value(if(input$plot2DataType == "permil") total.delta else total.ab))
        
        
        
        # enrichment for table
        table.df <- data.frame()
        for (i in 1:length(data$spikes[show])) {
            ispike <- data$spikes[show][i]
            df <- subset(plot.df, spike.ab == ispike)
            table.df <- rbind(table.df, 
              label_strength(
                time = duration(df$labeling_time, "seconds"), 
                dblt = duration(df$dblt, "seconds"), 
                spike = as.abundance(ispike),
                natural = data$nat)
            )
        }
        
        # merge with labels and spikes
        table.df <- merge(table.df, data.frame(dblt = dblts$dblts, dblt.userlabel = paste0(dblts$dblts.labels, " doubling")), by = 'dblt')
        table.df <- merge(table.df, spikes.df[c("spikes", "Label")], by.x = "spike.ab", by.y = "spikes")
        
        # adjust headers
        table.df <- mutate(
            table.df, 
            `Incubation time [s]` = as.numeric(time), 
            `Incubation time` = time.label)
        
        if (input$tableDataType == "permil") {
            table.df$enrichment <- signif(as.value(table.df$total.delta), 2) 
        } else {
            table.df$enrichment <- paste0(signif(100*as.value(table.df$total.ab), 2)) 
        }
        
        # cast 
        table.df <- dcast(table.df, `Label` + `Incubation time [s]` + `Incubation time` ~ dblt.userlabel, value.var = "enrichment" )
        header_order <- match(paste0(dblts$dblts.labels, " doubling"), names(table.df))
        table.df <- table.df[c(1,3,header_order)]
        
        return(c(
            dblts, data, list(plot.df = plot.df, plot2.df = plot2.df, table.df = table.df)))
    })
    
    # plot1
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
            labs(title = paste0("Required labeling times for enrichment to:\n", 
                               label(data$target.ab), " = ", signif(as.value(data$target.ab)*100, 3), "% / ",
                               name(data$target.delta), " = ", round(as.value(data$target.delta)), isotopia:::unit(data$target.delta))) + 
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
            theme(legend.position = "right")
    })
    
    # plot
    plot2Input <- reactive({
        data <- datasetInput()
        if (input$plot2DataType == "permil") {
            ylab <- name(data$plot2.df$total.delta)
            ylabeller <- function(x) format(x, big.mark = ",", scientific = FALSE)
        } else {
            ylab <- name(data$plot2.df$total.ab)
            ylabeller <- function(x)  paste0(signif(100*x, 1), "%")
        }
        
        p <- ggplot(data$plot2.df,
               aes(x = time, y = enrichment, colour = DBLT)) + 
            geom_line(linetype=1) + 
            scale_x_continuous(breaks = function(limits) pretty(limits, 10), labels = duration_label) +
            labs(x = "", colour = "Doubling time", y = ylab) + 
            facet_grid(~Spike) + 
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
            theme(legend.position = "right", axis.text.x = element_text(angle = 60, hjust = 1))
        if (input$plot2Y == TRUE)
            p + scale_y_log10(label = ylabeller)
        else 
            p + scale_y_continuous(label = ylabeller)
    })
    
    
    
    # Outputs rendering ---------------------------------------------
    
    # natural abundance info output
    output$nat <- renderUI({
        ref <- get_standards(minor = input$ref)[[1]]
        paste0("Reference ", label(ref), ": ", as.numeric(ref))
    })
    
    # labeling strengths messages output
    generate_label_error <- function(i) {
        renderUI({
            data <- labelsInput()
            if (data$spikes.error[i]) {
                return (paste0("Too small, can't possibly reach target enrichment."))
            } else
                return ("")
        })
    }
    generate_label_msg <- function(i) {
        renderUI({
            data <- labelsInput()
            if (data$spikes.error[i]) {
                return("")
            } else
                return (paste0("Effective labeling strength: ", signif(as.numeric(data$spikes[i]) * 100, 3), "% ", data$spikes@isoname, "."))
        })
    }
    
    output$label1_error <- generate_label_error(1)
    output$label2_error <- generate_label_error(2)
    output$label3_error <- generate_label_error(3)   
    output$label1_msg <- generate_label_msg(1)
    output$label2_msg <- generate_label_msg(2)
    output$label3_msg <- generate_label_msg(3) 
    
    # enrichment info output
    output$intensity_F_message <- renderUI({ 
        data <- labelsInput()
        if (data$target.ab <= as.abundance(data$nat))
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
    
    # rendering plot2
    output$plot2 <- renderPlot({
        withProgress(session, min=1, max=5, {
            setProgress(message = 'Rendering plot ...')
            setProgress(value = 2)
            p <- plot2Input()
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
