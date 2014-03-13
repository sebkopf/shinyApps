library(shiny)
library(lubridate)
library(stringr)
library(datasets)
library(ggplot2)
library(grid)

#' Duration labels to the closest large denominator (e.g. 86400s = ~24 hours)
#'@param ds vector of lubridate:::duration objects or time in seconds
duration_label <- function(ds) {
    sapply(ds, function(x) { 
        if (grepl("^[0-9\\.]+s$", (d <- duration(x, "seconds")))) return(paste0('~', round(as.numeric(d), 1), " seconds"))
        else return(str_extract(d, "~.[^\\)]*"))
    })
}

library(plyr)
#' Labeling time as function of doubling time, desired isotopic enrichment and strength of label
#'@param dblt doubling times, a vector of lubridate:::duration objects
#'@param Ex_sample - sample enrichment (in permil)
#'@param F_label - labeling strength (in fractional abundance)
#'@param R_nat - natural abundance of the labeled material (isotope ratio)
#'@return data frame with input columns and additional columns label.time, alpha and s
label_time <- function(dblt, Ex_sample, F_label, R_nat) {
    df <- expand.grid(dblt = dblt, Ex_sample = Ex_sample, F_label = F_label, R_nat = R_nat, stringsAsFactors = FALSE)
    mutate(df, 
           dblt.label = duration_label(dblt), # add labels
           alpha = iso.ExtoA(Ex_sample), # convert epsilon to alpha
           s = F_label / iso.RtoF(R_nat), # convert label to s
           labeling_time = dblt * 1/log(2) * log((1 - s) / (alpha - s)), # calculate label time
           labeling_time.label = duration_label(labeling_time)) # label
}

# isotope functions
iso.NtoR<-function(NM, Nm) Nm/NM
iso.NtoF<-function(NM, Nm) Nm/(NM + Nm)
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

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
    
    datasetInput <- reactive({
        # dates to plot
        dblts <- list(c(1, "hour"), c(1, "day"), c(1, "month"), c(1, "year"), c(10, "years"), c(100, "years"))
    
        c.RDnat <- 0.00015575 # D/H of VSMOW
        c.R15Nnat <- 0.003677 # 15N/14N of Air
        c.R13Cnat <- 0.011237 # 13C/12C of VPDB
        c.R18Onat <- 0.0020052 # 18O/16O of VSMOW
        c.R34Snat <- 0.0045005 # 34S/32S of CDT
        refs <- c("2H" = c.RDnat, "13C" = c.R13Cnat, "15N" = c.R15Nnat, "18O" = c.R18Onat, "34S" = c.R34Snat)
        
        # enrichment
        if (input$targetType == 'permil')
            enrich <- input$intensity_permil
        else 
            enrich <- iso.FtoDx(input$intensity_F, refs[[input$ref]])
        
        # labels
        labels <- c(input$label1, input$label2, input$label3)
        labels <- labels[as.integer(input$show)]
        
        # FIXME: implement check that enrichment is possible with the given label and exclude labels that are not possible
        
        # labeling times
        plot.df <- label_time(
            dblt = sapply(dblts, function(x) duration(as.integer(x[1]), x[2])), 
            Ex_sample = round(enrich), 
            F_label = labels, 
            R_nat = refs[[input$ref]])
        plot.df$Label <- factor(paste0(100*plot.df$F_label, "%")) # legend in %
        
        return(plot.df)
    })
 
    # The output$summary depends on the datasetInput reactive expression, 
    # so will be re-executed whenever datasetInput is invalidated
    # (i.e. whenever the input$dataset changes)
    output$summary <- renderPrint({
        dataset <- datasetInput()
        summary(dataset)
    })
    
    
    output$plot <- renderPlot({
        plot.df <- datasetInput()
        dblts <- list(c(1, "hour"), c(1, "day"), c(1, "month"), c(1, "year"), c(10, "years"), c(100, "years"))
        p <- ggplot(plot.df,
               aes(x = dblt, y = labeling_time, fill = Label, colour = Label)) + 
            geom_segment(aes(x=0, xend=dblt, y=labeling_time, yend=labeling_time), 
                         arrow=arrow(length=unit(0.4,"cm"), ends="first", type="open", 
                                     angle=15), linetype=2) + 
            geom_segment(aes(x=dblt, xend=dblt, y=0, yend=labeling_time), 
                         arrow=arrow(length=unit(0.4,"cm"), type="open", angle=15), 
                         linetype=2) + 
            geom_line(linetype=1) + geom_point(colour="black", shape = 21, size=4) +
            scale_y_log10(
                expand = c(0, 0.1), breaks = unique(plot.df$labeling_time), 
                labels = unique(plot.df$labeling_time.label)) + 
            scale_x_log10(expand = c(0, 0.2), breaks = unique(plot.df$dblt), 
                          labels = sapply(dblts, function(x) paste(x[1], x[2]))) + 
            labs(title = paste0("Required labeling times for ", round(unique(plot.df$Ex_sample)), 
                                " permil enrichment (", 
                                round(iso.DxtoF(unique(plot.df$Ex_sample), unique(plot.df$R_nat)) * 100, 2),
                                "% ", input$ref, ")\n"), x = "Doubling time", y = "") + 
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
            theme(legend.position = "right")
        suppressWarnings(print(p))
    })
})
