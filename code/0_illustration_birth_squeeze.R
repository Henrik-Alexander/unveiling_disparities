########################################
# Project: Subnational birth squeezes  #
# Purpose: Illustration birth squeeye  #
# Author: Henrik-Alexander schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 18.07.2024                     #
########################################

library(latex2exp)
library(MetBrewer)
library(patchwork)


# Load the graphic template
source("functions/Graphics.R")

## STRUCTURE:
# 1. Population difference
# 2. Age-pattern difference
# 3. Birth squeeze

par(las=1, family="serif", mar = c(4, 4, 3, 3), font.lab=2)

colours <- MetBrewer::MetPalettes$Cassatt1


select_colours <- function(nr) {
  colours[[1]][which(colours[[2]]==nr)]
}

# Functions -------------------------------------------

# Function to write the ratio
write_ratio <- function(R, maxvalue=100, rounding=2) {
  if (R > 1) {
    # If the ratio is higher than 1, than value is written on top of the figure
    mtext(text=round(R,rounding), side=3, at=maxvalue * 1/R, col="grey")
  } else {
    # If the ratio is lower than 1, than value is written on top of the figure
    mtext(text=round(R,rounding), side=4, at=maxvalue * R, col="grey", las=1)
  }
}


# Plot the point for the point-bar illustration
plot_point_bar <- function(data, sex="F", label="TFR", rounding=2) {
  # Define the colours
  col1 <- ifelse(sex=="F", rgb(1, 0.2, 0), rgb(0.2, 0.5, 1)) # Strong
  col2 <- ifelse(sex=="F", rgb(1, 0.2, 0, 0.1), rgb(0.2, 0.5, 1, 0.1)) # Shading
  
  # Plot the data
  points(x=data$pop, y=data$births,
         pch=ifelse(sex=="F", 17, 18), cex=2, col=col1)
  text(x=data$pop, y=data$births, label=TeX(paste0("$", label, "_", sex, "=", round(data$births/data$pop, rounding), "$")), pos=3, offset=0.1*max(data$births), col=col1)
  for (i in seq_along(data$pop)){
  polygon(x=c(0, 0, data$pop[i], data$pop[i]),
          y=c(0, data$birth[i], data$birth[i], 0),
          col=col2, border=col1)
  }
}

# Function to plot the rectangle in Plot 1
plot_bar <- function(position, value, maxposition=2, colour="pink", label="") {
  position <- position-1
  polygon(x = c(position/maxposition, position/maxposition, (position+1)/maxposition,(position+1)/maxposition), 
          y = c(0, value, value, 0),
          col = colour)
  text(x=(position+0.5)/maxposition, y = 0.5*value,
       label=paste0(label, value), col="white", family="serif", cex=2, font=2)
}

# Graph the points on the male-female TFR plane
plot_points <- function(tfr_f, tfr_m, colour="red") {
  points(x=tfr_f, y=tfr_m, col=colour)
  segments(x0=tfr_f, y0=0, x1=tfr_f, y1=tfr_m, lty=2, col=colour)
  segments(x0=0, y0=tfr_m, x1=tfr_f, y1=tfr_m, lty=2, col=colour)
  # text(x=tfr_f, y=tfr_m, label = TeX(paste0("$TFR_m=", tfr_m, "$")), pos=3, offset=0.5, col=colour)
  # text(x=tfr_f, y=tfr_m, label = TeX(paste0("$TFR_f=", tfr_f, "$")), pos=1, offset=0.5, col=colour)
  text(x=tfr_f, y=tfr_m, label = TeX(paste0("$R=\\frac{TFR_m=", tfr_m, "}{TFR_f=", tfr_f, "}=", round(tfr_m/tfr_f, 2), "$")), col=colour)
  
}


# Plot axis function
plot_axis <- function(maxvalue=120, minvalue=0) {
  axis(1, tick = T, at = seq(minvalue, maxvalue, by = 10), lwd=2)
  axis(1, tick = T, at = minvalue:maxvalue, labels=F, tck=-0.01)
  axis(2, tick = T, at = seq(minvalue, maxvalue, by = 10), las = 1, lwd=2)
  axis(2, tick = T, at = minvalue:maxvalue, labels=F, tck=-0.01)
}


# 1. Population difference ----------------------------

# Create the data
bs_f <- list(pop = c(80),
             births= c(10))
bs_m <- list(pop = c(100),
             births= c(10))


pdf(file="figures/illustrate_tfr_ratio_1.pdf",
    height=10, width=12)
# Bar graph
plot(NA, 
     xlim=c(0, 1), ylim = c(0, 120),
     xlab="Sex", ylab="Count",
     main = "Population contribution to birth squeezes",
     axes=FALSE, las=1, yaxs="i", xaxs="i", font=2)
grid(nx=NA, ny=10)

# Illustrate the start
plot_bar(1, bs_f$pop, colour=select_colours(1), label="Population=")
plot_bar(1, bs_f$births, colour=select_colours(3), label="Births=")
plot_bar(2, bs_m$pop, colour=select_colours(4), label="Population=")
plot_bar(2, bs_m$births, colour=select_colours(2), label="Births=")

text(x=0.25, y=bs_f$pop, label=TeX(paste0("$TFR_f=", bs_f$births/bs_f$pop, "$")), offset=0.5, pos=3, cex=2, col=select_colours(3))
text(x=0.75, y=bs_m$pop, label=TeX(paste0("$TFR_m=",bs_m$births/bs_m$pop, "$")), offset=0.5, pos=3, cex=2, col=select_colours(2))

# Create the axis
axis(1, tick = F, at = c(0.25, 0.75), labels=c("Female", "Male"), lwd=2, cex=5, font=2)
axis(2, tick = T, at = seq(0, 120, by = 10), las = 1, lwd=2)
axis(2, tick = T, at = 1:120, labels=F, tck=-0.01)
dev.off()

# outer(c("Population", "Births"), c("Female", "Male"), paste)
## Plot the ratio pattern
pdf(file="figures/illustrate_tfr_ratio_2.pdf",
    height=10, width=12)
# Adjust the right margin
plot(NA, 
     xlim=c(0, 120), ylim = c(0, 101),
     xlab="Population", ylab="Births", 
     axes=FALSE, las=1, yaxs="i", xaxs="i", font=2,
     main="Male-Female TFR ratio")
grid()
plot_axis()

plot_point_bar(data=bs_f, "F")
plot_point_bar(data=bs_m, "M")
dev.off()

#write_ratio(0.2)
#lapply(seq(0, 3, by=0.1), write_ratio)
#lapply(seq(-100, 100, 10), FUN = function(x) abline(b=1, a=x, col="lightgrey"))

#plot_point_bar(data=bs_f, "F")
#plot_point_bar(data=bs_m, "M")


# 2. Age-pattern difference ---------------------------

pdf(file="figures/illustrate_tfr_ratio_3.pdf",
    height=10, width=12)

# Create the data
bs2_f <- list(pop = c(10, 20, 5),
             births= c(5, 0, 0))
bs2_m <- list(pop = c(10, 20, 5),
             births= c(0, 5, 0))

plot(NA, 
     xlim=c(0, 25), ylim = c(0, 25),
     xlab="Population", ylab="Births", 
     axes=FALSE, las=1, yaxs="i", xaxs="i", font=2,
     main="Male-Female TFR ratio")
abline(a=0, b=1)
plot_axis()
plot_point_bar(data=bs2_f, "F", label="ASFR")
plot_point_bar(data=bs2_m, "M", label="ASFR")
dev.off()

# 3. Birth squeeze ------------------------------------


pdf(file="figures/illustrate_tfr_ratio_4.pdf",
    height=10, width=12)

# Define the ratio of the cohort size
d <- 1.5
# Create the data
bs2_f <- list(pop = c(10),
              births= c(5))
bs2_m <- list(pop = c(10)*d,
              births= c(5))

plot(NA, 
     xlim=c(0, 25), ylim = c(0, 25),
     xlab="Population", ylab="Births", 
     axes=FALSE, las=1, yaxs="i", xaxs="i", font=2,
     main="Male-Female TFR ratio")
abline(a=0, b=1)
plot_axis()
plot_point_bar(data=bs2_f, "F", label="TFR")
plot_point_bar(data=bs2_m, "M", label="TFR")
dev.off()


pdf(file="figures/illustrate_tfr_ratio_5.pdf",
    height=10, width=12)


# Create the lexis diagram
plot(NA, 
     xlim=c(-10, 45), ylim = c(0, 45),
     xlab="Year/Cohort", ylab="Age", 
     axes=FALSE, las=1, yaxs="i", xaxs="i", font=2,
     main="Birth squeeze phenomenon")


grid()
plot_axis(maxvalue=50, minvalue=-10)

# Draw the measurement period
polygon(x=c(42, 42, 43, 43), y=c(0, 50,50, 0), col="lightgrey")

# Create the male cohort
polygon(x=c(-1, 49, 50, 0),
        y=c(0, 50, 50, 0), col=select_colours(4))

polygon(x=c(0, 50, 53, 3),
        y=c(0, 50, 50, 0), col=select_colours(1))

# Write the cohort name
mtext(side=1, text="c", at=1.5)
mtext(side=1, text="c-3", at=-1.5)

dev.off()



## Make small lexis diagram

pdf(file="figures/illustrate_tfr_ratio_6.pdf",
    height=10, width=12)

plot(NA, 
     xlim=c(0, 3), ylim = c(0, 3),
     xlab="Year/Cohort", ylab="Age", 
     axes=FALSE, las=1, yaxs="i", xaxs="i", font=2,
     main="Birth squeeze phenomenon")


# Draw the measurement period
polygon(x=c(1, 1, 2, 2), y=c(0, 5, 5, 0), col="lightgrey")

# Create the male cohort
polygon(x=c(0, 4, 5, 1),
        y=c(0, 4, 4, 0), col=select_colours(1))

polygon(x=c(-1, 3, 4, 0),
        y=c(0, 4, 4, 0), col=select_colours(4))
grid()
axis(1, tick = T, at=0:3, labels=TeX(paste0("$", paste("T_{", seq(-1, 2, by=1)), "}$")), lwd=2)
axis(2, tick = T, at=0:3, labels=35:38, las = 1, lwd=2)
axis(2, at=0:50, labels=F)


# Write the cohort name
mtext(side=2, text="c-3", at=0.5, line=0.5)
mtext(side=1, text="c", at=0.5)

# Write the text
text(x=1.5, y=1, label=TeX(paste0("$TFR_m=\\frac{B=10}{P_c=100}=0.1$")), col="black", cex=0.8)
text(x=1.5, y=2, label=TeX(paste0("$TFR_m=\\frac{B=10}{P_{c-3}=80}=0.125$")), col="black", cex=0.8)
dev.off()

# 4. Plot the ratios =-------------------------------



pdf(file="figures/illustrate_tfr_ratio_7.pdf",
    height=10, width=12)
# Create the ratio data
tfrs <- c(0.8, 1.2, 1.5, 1.8, 2.1)
ratios <- expand.grid(tfr_m = tfrs,
                      tfr_f = tfrs)
ratios$tfr_ratio <- with(ratios, tfr_m / tfr_f)

plot(NA, 
     xlim=c(min(tfrs), max(tfrs)), ylim = c(min(tfrs), max(tfrs)),
     xlab=TeX('$\\textrm{TFR}_f$'),
     ylab=TeX('$\\textrm{TFR}_m$'), 
     axes=FALSE, las=1, yaxs="i", xaxs="i", font=2)
title("Male-female TFR ratio", line=2)
lapply(ratios$tfr_ratio[!is.na(ratios$tfr_ratio)], FUN = function(ratio) curve(expr=x*ratio, add=T, col="grey", from=0, to=3))
abline(a=0, b=1, col="black")
lapply(ratios$tfr_ratio[!is.na(ratios$tfr_ratio)], write_ratio, rounding=2, maxvalue=max(tfrs))
axis(1, tick = T, at = tfrs, lwd=2)
axis(1, at=seq(min(tfrs), max(tfrs), 0.1), labels=F)
axis(2, tick = T, at = tfrs, las = 1, lwd=2)
axis(2, at=seq(min(tfrs), max(tfrs), 0.1), labels=F)


plot_points(tfr_f=c(1.1, 1.8, 2),
            tfr_m=c(1.5, 1.4, 1.8), 
            colour=sapply(1:3, select_colours))
dev.off()



# US examples -----------------------------------

# Path to the US data
path_usa <- "U:/data/usa/fertility_rates/data/asfr_us.Rda"
load(path_usa)
asfr_us <- rename(asfr_us, region = state)

# Load the Mexican dta
load("U:/data/mex/fertility_rates/asfr_mex.Rda")
asfr_mex_reg <- asfr_mex_reg %>% 
  select(region, year, age, starts_with("births"), starts_with("exposure"), starts_with("asfr"))
names(asfr_mex_reg) <- str_replace(names(asfr_mex_reg), "exposure", "pop")


# Select the two regions
regions <- c("District of Columbia", "Alaska", "New York")

# Subset the state
illustrate_birth_squeeze <- function(region, data=asfr_us, year=2004, aggregate=TRUE) {
  
  # Subset a state
  df <- data[data$year==year & data$region==region, ]
  
  # Aggregate the data
  if(aggregate) {
    df <- df %>% 
      mutate(age = cut(age, breaks=seq(15, 60, by=5), include.lowest = T, labels=c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55+"))) %>% 
      group_by(age) %>% 
      summarise(across(.cols=ends_with("male"), .fns=sum))
  }
  
  # Plot the data
  df <- pivot_longer(df, cols = ends_with("male"), names_pattern="(.+)_(.+)", names_to = c("var", "sex"))
  df <- pivot_wider(df, names_from = "var", values_from ="value")
  
  # Estimate the transformation ratio
  asfr_ratio <- max(df$asfr) / max(df$pop)
  
  # Estiamte the TFR ratio
  tfr_ratio <- round(sum(df$asfr[df$sex=="male"]) / sum(df$asfr[df$sex=="female"]), 2)
  max_pop <- round(max(df$pop) / 10000) * 12000
  
  # Plot the data
  
  base_plot <- ggplot(df, aes(x=age, group=sex, colour=sex, fill=sex)) +
    geom_area(aes(y=asfr/asfr_ratio), alpha=0.3, positio=position_identity()) +
    geom_col(aes(y=pop, alpha="Population"), position=position_dodge()) +
    geom_col(aes(y=births, alpha="Births"), position=position_dodge()) +
    geom_line(aes(y=asfr/asfr_ratio, linetype=sex), linewidth=2) +
    geom_point(aes(y=asfr/asfr_ratio, shape=sex), size=5) +
    ggtitle(paste("TFR ratio of", tfr_ratio, "in", region, "in", year)) +
    scale_y_continuous("Population and birth count", expand = c(0, 0), n.breaks=10, limits=c(0, max_pop), sec.axis = sec_axis(trans= ~ . * asfr_ratio, name="Age-specific fertility rate", breaks=seq(0, 1, by = 0.1))) +
    scale_alpha_manual("Counts:", values = c(1, 0.3)) +
    scale_colour_manual("Sex:", values=c(MPIDRgreen, MPIDRpurple)) +
    scale_fill_manual("Sex:", values=c(MPIDRgreen, MPIDRpurple)) +
    scale_shape_manual("Age-specific fertility rate:", values=c(16, 17)) +
    scale_linetype_manual("Age-specific fertility rate:", values=c("solid", "solid"))
  
  
  
  if (aggregate) {
    p <- base_plot + scale_x_discrete("Age", expand=c(0, 0))
  } else {
    p <- base_plot + scale_x_continuous("Age", expand = c(0, 0), n.breaks=10)
  }
  ggsave(plot=p, filename=paste0("figures/illustration_tfr_ratio_", state, ifelse(aggregate, "_aggregate", ""),year, ".pdf"),
         height=15, width=22, unit="cm")
  
    return(p)

}

plots <- lapply(regions, illustrate_birth_squeeze)

illustrate_birth_squeeze(region="Aguascalientes", data=asfr_mex_reg, year=2000)




illustrate_birth_squeeze(region="Oaxaca", data=asfr_mex_reg, year=2021) + plots[[2]] +
  plot_layout(axis = "collect", guides = "collect") +
  plot_annotation(tag_levels = "A", tag_suffix = ")")
ggsave(filename="figures/illustration_birthsqueeze_usa_joint.pdf", height=18, width=30, unit="cm")

### END ######################################