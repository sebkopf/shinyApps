#### Plotting ####

.base_plot <- 
  ggplot() + 
  aes(x = value.x, y = value.y, color = process) + 
  geom_path(size = 2) + 
  theme_bw() + theme_public() + theme_xangle() + 
  labs(x="", y="", color = "") 

add_arrows <- function (df) {
  arrow_df <- merge(
    df %>% subset(i == max(i) - 1), 
    (df %>% subset(i == max(i)) %>% mutate(xend = value.x, yend = value.y))
    [c("process", "scenario", "variable.x", "variable.y", "xend", "yend")])  
  
  geom_segment(data = arrow_df, aes(xend = xend, yend = yend), size = 2,
               arrow = arrow(length = unit(0.5, "cm"), type = "open")) 
}

plot_in_grid <- function(df, plot = .base_plot) {
  plot %+% subset(df, i < max(i)) + add_arrows(df) + 
    facet_grid(variable.y ~ variable.x, scales = "free", labeller = function(var, val) {
      return(sapply(val, function(i) {
        latex2exp(paste0("$", get_p(as.character(i), "latex"), "$"))
      }))
    })
}

plot_in_wrap <- function(df, plot = .base_plot) {
  p <- plot %+% subset(df, i < max(i)) + add_arrows(df) + 
    facet_wrap(~ variable.y + variable.x, nrow = length(df$variable.y %>% as.character() %>% unique()), scales = "free") 
  
  facet_wrap_labeller(p, labeller = function(var, val) { 
    lapply(val, function(id) {
      parts <- strsplit(id, ", ", fixed = T)[[1]]
      latex2exp(paste0("$", get_p(parts[1], "latex"), "$ vs. $", get_p(parts[2], "latex"), "$"))
    })
  }) 
}

#### Parameters ####

#' get a parameter value
get_p <- function(id, col = "default") {
  if (!id %in% names(plist)) stop(id, " is not a valid parameter id")
  if (!col %in% names(plist[[1]])) stop(col, " is not a valid parameter property")
  return(plist[[id]][[col]])
}


#### Data modelling ####

max_substrate_check <- function(init, D_conc, substrate_ID, substrate = sub("^c_(.*)$", "\\1", substrate_ID)) {
  if (D_conc > init[1, substrate_ID]) {
    D_conc <- init[1, substrate_ID]
    message("WARNING: Trying to remove more ", substrate, 
            " than available. Max removal flux adjusted to total ", substrate, ".")
  }
  return(D_conc)
}

#' run denitrification
#' @param D_conc change in substrate concentration (both nitrate and nitrite)
run_denitrification <- function(init, D_conc, ...) {
  D_conc <- max_substrate_check(init, D_conc, "c_NO3")
  init %>% mutate(
    Dc_NO3 = seq(0, -D_conc, length = length(i)),
    Dc_NO2 = -Dc_NO3,
    c_NO3 = c_NO3[1] + Dc_NO3, # nitrate removal
    c_NO2 = c_NO2[1] + Dc_NO2, # nitrite accumulate
    f_NO3 = c_NO3/c_NO3[1], ln_f_NO3 = log(f_NO3), # ln f
    d15_NO3 = d15_NO3[1] - eps15_NAR * ln_f_NO3,
    d18_NO3 = d18_NO3[1] - eps15_NAR / eps15_eps18_NAR_ratio * ln_f_NO3,
    id15_NO2 = d15_NO3 - eps15_NAR,
    d15_NO2_new = d15_NO3[1] + eps15_NAR * f_NO3/(1-f_NO3) * ln_f_NO3,
    d15_NO2 = mass_balance( c_NO2[1], d15_NO2[1], Dc_NO2, d15_NO2_new ),
    id18_NO2 = d18_NO3 + eps18_NAR_b,
    d18_NO2_new = d18_NO3[1] + eps18_NAR_b * f_NO3/(1-f_NO3) * ln_f_NO3 + eps15_NAR / eps15_eps18_NAR_ratio,
    d18_NO2 = mass_balance( c_NO2[1], d18_NO2[1], Dc_NO2, d18_NO2_new )
  )
}

#' run nitrite reduction
#' @todo: implement what it means for N2O
run_nitrite_reduction <- function(init, D_conc, ...) {
  D_conc <- max_substrate_check(init, D_conc, "c_NO2")
  init %>% mutate(
    Dc_NO2 = seq(0, -D_conc, length = length(i)),
    c_NO2 = c_NO2[1] + Dc_NO2, # nitrite removal
    f_NO2 = c_NO2/c_NO2[1], ln_f_NO2 = log(f_NO2), 
    d15_NO2 = d15_NO2[1] - eps15_NIR * ln_f_NO2,
    d18_NO2 = d18_NO2[1] - eps15_NIR / eps15_eps18_NAR_ratio * ln_f_NO2
  )
}

#' run nitrite oxidation
run_nitrite_oxidation <- function(init, D_conc, ...) {
  D_conc <- max_substrate_check(init, D_conc, "c_NO2")
  init %>% mutate(
    Dc_NO2 = seq(0, -D_conc, length = length(i)),
    Dc_NO3 = -Dc_NO2,
    c_NO2 = c_NO2[1] + Dc_NO2, # nitrite oxidation
    c_NO3 = c_NO3[1] + Dc_NO3, # nitrate accumulation
    f_NO2 = c_NO2/c_NO2[1], ln_f_NO2 = log(f_NO2), # ln f in NO2
    d15_NO2 = d15_NO2[1] - eps15_NIX * ln_f_NO2,
    d18_NO2 = d18_NO2[1] - eps18_NIX * ln_f_NO2,
    id15_NO3 = d15_NO2 - eps15_NIX,
    d15_NO3_new = d15_NO2[1] + eps15_NIX * f_NO2/(1-f_NO2) * ln_f_NO2,
    d15_NO3 = mass_balance( c_NO3[1], d15_NO3[1], Dc_NO3, d15_NO3_new ),
    id18_NO3 = 2/3 * (d18_NO2 - eps18_NIX) + 1/3 * (d18_H2O - eps18_WOI2),
    d18_NO3_new = 2/3 * (d18_NO2[1] + eps18_NIX * f_NO2/(1-f_NO2) * ln_f_NO2) + 
      1/3 * (d18_H2O - eps18_WOI2),
    d18_NO3 = mass_balance( c_NO3[1], d18_NO3[1], Dc_NO3, d18_NO3_new )
  )
}

#' run ammonium oxidation
#' @note assumes O2 is an infinite reservoir
run_ammonium_oxidation <- function(init, D_conc, ...) {
  D_conc <- max_substrate_check(init, D_conc, "c_NH4")
  init %>% mutate(
    Dc_NH4 = seq(0, -D_conc, length = length(i)),
    Dc_NO2 = -Dc_NH4,
    c_NH4 = c_NH4[1] + Dc_NH4, # ammonium oxidation
    c_NO2 = c_NO2[1] + Dc_NO2, # nitrite accumulation
    f_NH4 = c_NH4/c_NH4[1], ln_f_NH4 = log(f_NH4), # ln f in NH4
    d15_NH4 = d15_NH4[1] - eps15_AMX * ln_f_NH4,
    id15_NO2 = d15_NH4 - eps15_AMX,
    d15_NO2_new = d15_NH4[1] + eps15_AMX * f_NH4/(1-f_NH4) * ln_f_NH4,
    d15_NO2 = mass_balance( c_NO2[1], d15_NO2[1], Dc_NO2, d15_NO2_new ),
    id18_NO2 = (1 + x_aob)/2 * d18_H2O + (1 - x_aob)/2 * (d18_O2 - eps18_O2_WOI1) + x_aob * eps18_NIEX,
    d18_NO2_new = id18_NO2,
    d18_NO2 = mass_balance( c_NO2[1], d18_NO2[1], Dc_NO2, d18_NO2_new )
  )
}

#' run nitrate assimilation
run_nitrate_assimilation <- function(init,  D_conc, ...) {
  D_conc <- max_substrate_check(init, D_conc, "c_NO3")
  init %>%
    mutate(
      Dc_NO3 = seq(0, -D_conc, length = length(i)),
      Dc_Norg = -Dc_NO3,
      c_NO3 = c_NO3[1] + Dc_NO3, # nitrate removal
      c_Norg = c_Norg[1] + Dc_Norg, # assimilated N
      f_NO3 = c_NO3/c_NO3[1], ln_f_NO3 = log(f_NO3), # ln f
      d15_NO3 = d15_NO3[1] - eps15_NAS * ln_f_NO3,
      d18_NO3 = d18_NO3[1] - eps15_NAS / eps15_eps18_NAR_ratio * ln_f_NO3,
      id15_Norg = d15_NO3 - eps15_NAS,
      d15_Norg_new = d15_NO3[1] + eps15_NAS * f_NO3/(1-f_NO3) * ln_f_NO3,
      d15_Norg = mass_balance( c_Norg[1], d15_Norg[1], Dc_Norg, d15_Norg_new )
    ) 
}

#' run ammonium assimilation
run_ammonium_assimilation <- function(init,  D_conc, ...) {
  D_conc <- max_substrate_check(init, D_conc, "c_NH4")
  init %>%
    mutate(
      Dc_NH4 = seq(0, -D_conc, length = length(i)), 
      Dc_Norg = -Dc_NH4,
      c_NH4 = c_NH4[1] + Dc_NH4, # ammonium consumption
      c_Norg = c_Norg[1] + Dc_Norg, # assimilated N
      f_NH4 = c_NH4/c_NH4[1], ln_f_NH4 = log(f_NH4), 
      d15_NH4 = d15_NH4[1] - eps15_AAS * ln_f_NH4,
      id15_Norg = d15_NH4 - eps15_AAS,
      d15_Norg_new = d15_NH4[1] + eps15_AAS * ( f_NH4/(1-f_NH4) * ln_f_NH4 ) %|% 0,
      d15_Norg = mass_balance( c_Norg[1], d15_Norg[1], Dc_Norg, d15_Norg_new )
    ) 
}

#' run nitrogen fixation
run_nitrogen_fixation <- function(init,  D_conc, ...) {
  init %>%
    mutate(
      Dc_Norg = seq(0, D_conc, length = length(i)),
      c_Norg = c_Norg[1] + Dc_Norg, 
      id15_Norg = d15_N2 - eps15_NFX,
      d15_Norg_new = id15_Norg,
      d15_Norg = mass_balance( c_Norg[1], d15_Norg[1], Dc_Norg, d15_Norg_new)
    ) 
}

#' run nitrite water exchange
run_nitrite_water_exchange <- function(init,  D_conc, ...) {
  D_conc <- max_substrate_check(init, D_conc, "c_NO2")
  init %>%
    mutate(
      Dc_NO2_ex = seq(0, D_conc, length = length(i)),
      x_ex = Dc_NO2_ex / c_NO2[1], # amount of exchange
      id18_NO2 = d18_H2O + eps18_NIEX, # instantaneous value of new NO2
      d18_NO2 = mass_balance(1 - x_ex, d18_NO2[1], x_ex, d18_H2O + eps18_NIEX)
    ) 
}

#' run mass balance
mass_balance <- function(ma, da, mb, db) {
  ma <- ifelse( is.nan(ma) | is.na(ma), 0, ma)
  a <- ifelse(ma == 0, 0, ma * da)
  mb <- ifelse( is.nan(mb) | is.na(mb), 0, mb)
  b <- ifelse(mb == 0, 0, mb * db)
  return( (a + b) / (ma + mb) )
}

#' run all the different processes
run_processes <- function(processes, scenarios, init, steps = 5, 
                          vars = subset(params, type %in% c("var", "inst"))$id) {
  all <- data.frame()
  iframe <- init[rep(1,steps+1),] %>% mutate(i = 0:steps) # replicate init data frame for number of steps

  for (proc_i in 1:nrow(processes)) {
    for (scenario in scenarios) {
      all <- all %>% rbind(
        do.call(processes$call[proc_i], args = c(list(iframe), scenario$args))[c("i", vars)] %>%
          mutate(process = processes$name[proc_i], scenario = scenario$name))
    }
  }
  
  # remove isotope values for conc <= 0 & calculate various universal output measures
  all %>% 
    mutate( 
      d15_NO3 = ifelse(c_NO3 > 0, d15_NO3, NA), 
      d18_NO3 = ifelse(c_NO3 > 0, d18_NO3, NA), 
      d15_NO2 = ifelse(c_NO2 > 0, d15_NO2, NA), 
      d18_NO2 = ifelse(c_NO2 > 0, d18_NO2, NA), 
      d15_Norg = ifelse(c_Norg > 0, d15_Norg, NA),
      d15_NH4 = ifelse(c_NH4 > 0, d15_NH4, NA), 
      ln_f_NO3 = log(c_NO3/c_NO3[1]),
      ln_f_NO2 = log(c_NO2/c_NO2[1]),
      D_15_18 = (d15_NO3 - d15_NO3[1]) - get_p ("eps15_eps18_NAR_ratio") * (d18_NO3 - d18_NO3[1]),
      D_dNOx = d15_NO3 - d15_NO2
    ) %>%
    melt(id.vars = c("process", "scenario", "i"))
}
