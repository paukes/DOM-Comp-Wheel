
# Composition Wheel Template

Here you will find the code to create your own Composition Wheel. Feel
free to swap out parameters with what you like (just be sure to
‘normalize’ the values as scale between 0 and 1 based on the max and min
in the dataset).

## Load required libraries and dataset

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.1.0     v dplyr   1.0.5
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
#Dataset
DOM_CW <- read.csv('test_dataset.csv')
```

## Set up the Composition Wheel

Scale all parameters to form 4 axes with values from 0 to 1 (the min and
max of that parameter in the dataset).

``` r
DOM_CW.scaled <- na.omit(DOM_CW)   #remove any 'NA

DOM_CW.scaled <- 
  DOM_CW.scaled %>%
   transmute(
     #Keep the columns that help identify the samples
     Area = Area,
     Type = Type,
     Sample = Sample,
     DOC_mg.L = DOC_mg.L,
     #Create the Composition Wheel scale
     HS  =  ( HS. - min(HS.) )   / (max(HS.) - min(HS.) ),
     C.N =  ( DOC.DON - min(DOC.DON) )   / (max(DOC.DON) - min(DOC.DON) ),
     SUVA = ( SUVA - min(SUVA) )   / (max(SUVA) - min(SUVA) ),
     S275 = ( S275 - min(S275) )   / (max(S275) - min(S275) )
   ) %>%
  #Re-arrange table so we get one row per composition measure per sample
  #I would use pivot_longer(), but there is something with dplyr that messes up the CW. Thus, stuck with 'melt' for now
   reshape2::melt(id.vars = c('Area','Type','Sample','DOC_mg.L')) 

#Create a radial coordinate system with closed points and linear lines
#Taken from Erwan Le Pennec (2016):
#http://www.cmap.polytechnique.fr/~lepennec/R/Radar/RadarAndParallelPlots.html

coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}
```

## Plot the Composition Wheel

### Single Sample

``` r
ggplot(DOM_CW.scaled, aes(x=variable, y=value))+
  geom_polygon(data=subset(DOM_CW.scaled, Sample == 'L123'),
               aes(group=Sample, colour=Area),
               fill= NA,
               size=1)+
  xlab("")+
  ylab("")+
  guides(colour='none')+
  scale_y_continuous(limits=c(0,1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1))+
  theme_bw(base_size = 12)+
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        axis.text.x = element_blank())+
  coord_radar()
```

![](Composition-Wheel-Template_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
\#\#\# Series of Composition Wheels  
Note the facet\_wrap() to allow this.

``` r
ggplot(DOM_CW.scaled, aes(x=variable, y=value))+
  geom_polygon(aes(group=interaction(Sample,Area), colour=Area),
               fill= NA,
               size=1)+
  xlab("")+
  ylab("")+
  scale_y_continuous(limits=c(0,1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1))+
  facet_wrap(.~Type)+
  theme_bw(base_size = 12)+
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        axis.text.x = element_blank())+
  coord_radar()
```

![](Composition-Wheel-Template_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
