## Code to replicate Kapur et al (202X)
## Spatial reference points for next-gen assessment models

require(dplyr)
require(here)
require(ggplot2);require(ggsidekick)
require(reshape2)

source(here("R","fnxs.R"))

## build datasets to spec (will autosave figure)
dat <- makeDat(wa = c(5,5),
               fec_a50 = c(6,6),
               fec_a95 = c(12,12),
               slx_a50 = c(9,9),
               slx_a95 = c(13,13),
               pStay = c(0.9,0.6))

