install.packages("dplyr")
library(dplyr)
msrm <- read.csv(file = "data/mushrooms.csv")

# Omit missing value
na.omit(msrm)
msrm <- msrm[msrm$stalk.root != "?",]

# Convert all fields to factors
msrm$class <- recode_factor(msrm$class, e = "edible", p = "poisonous")
msrm$cap.shape <- recode_factor(msrm$cap.shape, b = "bell", c = "conical", x = "convex", f = "flat", k = "knobbed", s = "sunken")
msrm$cap.surface <- recode_factor(msrm$cap.surface, f = "fibrous", g = "grooves", y = "scaly", s = "smooth")
msrm$cap.color <- recode_factor(msrm$cap.color, n = "brown", b = "buff", c = "cinnamon", g = "gray", r = "green", p = "pink", u = "purple", e = "red", w = "white", y = "yellow")
msrm$bruises <- recode_factor(msrm$bruises, t = "yes", f = "no")
msrm$odor <- recode_factor(msrm$odor, a = "almond", l = "anise", c = "creosote", y = "fishy", f = "foul", m = "musty", n = "none", p = "pungent", s = "spicy")
msrm$gill.attachment <- recode_factor(msrm$gill.attachment, a = "attached", d = "descending", f = "free", n = "notched")
msrm$gill.spacing <- recode_factor(msrm$gill.spacing, c = "close", w = "crowded", d = "distant")
msrm$gill.size <- recode_factor(msrm$gill.size, b = "broad", n = "narrow")
msrm$gill.color <- recode_factor(msrm$gill.color, k = "black", n = "brown", b = "buff", h = "chocolate", g = "gray",  r = "green", o = "orange", p = "pink", u = "purple", e = "red", w = "white", y = "yellow")
msrm$stalk.shape <- recode_factor(msrm$stalk.shape, e = "enlarging", t = "tapering")
msrm$stalk.root <- recode_factor(msrm$stalk.root, b = "bulbous", c = "club", u = "cup", e = "equal", z = "rhizomorphs", r = "rooted")
msrm$stalk.surface.above.ring <- recode_factor(msrm$stalk.surface.above.ring, f = "fibrous", y = "scaly", k = "silky", s = "smooth")
msrm$stalk.surface.below.ring <- recode_factor(msrm$stalk.surface.below.ring, f = "fibrous", y = "scaly", k = "silky", s = "smooth")
msrm$stalk.color.above.ring <- recode_factor(msrm$stalk.color.above.ring, n = "brown", b = "buff", c = "cinnamon", g = "gray", o = "orange", p = "pink", e = "red", w = "white", y = "yellow")
msrm$stalk.color.below.ring <- recode_factor(msrm$stalk.color.below.ring, n = "brown", b = "buff", c = "cinnamon", g = "gray", o = "orange", p = "pink", e = "red", w = "white", y = "yellow")
msrm$veil.color <- recode_factor(msrm$veil.color, n = "brown", o = "orange", w = "white", y = "yellow")
msrm$ring.number <- recode_factor(msrm$ring.number, n = "none", o = "one", t = "two")
msrm$ring.type <- recode_factor(msrm$ring.type, c = "cobwebby", e = "evanescent", f = "flaring", l = "large", n = "none", p = "pendant", s = "sheathing", z = "zone")
msrm$spore.print.color <- recode_factor(msrm$spore.print.color, k = "black", n = "brown", b = "buff", h = "chocolate", r = "green", o = "orange", u = "purple", w = "white", y = "yellow")
msrm$population <- recode_factor(msrm$population, a = "abundant", c = "clustered", n = "numerous", s = "scattered", v = "several", y = "solitary")
msrm$habitat <- recode_factor(msrm$habitat, g = "grasses", l = "leaves", m = "meadows", p = "paths", u = "urban", w = "waste", d = "woods")

# Omit column with single value
msrm$veil.type <- NULL

write.csv(msrm, "data/mushroom_factored.csv", quote = FALSE, row.names = FALSE)
