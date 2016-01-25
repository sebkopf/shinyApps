

#' Isotopia options
set_iso_opts(
  default_delta_notation = "permil",
  default_ab_notation = "raw"
)


#' Duration labels to the closest large denominator (e.g. 86400s = ~24 hours)
#'@param ds vector of lubridate:::duration objects or time in seconds
duration_label <- function(ds) {
  sub("~", "", sapply(ds, function(x) { 
    if (grepl("^[0-9\\.]+s$", (d <- duration(x, "seconds")))) return(paste0('~', round(as.numeric(d), 1), " seconds"))
    else return(str_extract(d, "~.[^\\)]*"))
  }))
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
               target.ab = to_ab(target), # convert to abundance 
               spike.ab = to_ab(spike), # convert to abundance
               natural.ab = to_ab(natural), # convert to abundance
               labeling_time = as.numeric(dblt)/log(2) * 
                 log((get_value(spike.ab) - get_value(natural.ab)) / (get_value(spike.ab) - get_value(target.ab))), # calculate label time
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
  df <- expand.grid(time = time, dblt = dblt, spike.ab = to_ab(spike), natural.ab = to_ab(natural), stringsAsFactors = FALSE)
  mutate(df,
         time.label = duration_label(time), # add labels
         dblt.label = duration_label(dblt), # add labels
         p = log(2)/as.numeric(dblt), # specific growth rate
         decay = exp(-p * as.numeric(time)), # decay of old material
         total.ab = quietly(set_attrib(weight(natural.ab, decay) + weight(spike.ab, 1 - decay), compound = "Sample")), # mass balance
         total.delta = to_delta(to_ratio(total.ab), ref_ratio = to_ratio(natural)) # permil value
  )
}
