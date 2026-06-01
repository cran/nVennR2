## ----setup, F, include = FALSE------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.show='hide'
)
library(knitr)
uuid <- function() {
  hex_digits <- c(as.character(0:9), letters[1:6])
  hex_digits <- toupper(hex_digits)
  paste(sample(hex_digits, 8), collapse='')
}

subsuid <- function(regex, strng){
  l <- gregexpr(regex, strng, perl = T)
  for (x in regmatches(strng, l)){ 
    m <- regexpr('([^\\{ \\.\\#]+)', x, perl = T)
    names <- regmatches(x, m)
    gstr = strng
    for (name in names){
      nname <- paste('([^\\d\\w<>]', name, ')', sep="")
      gstr <- gsub(nname, paste('\\1', '_', uuid(), sep=""), gstr, perl = T) 
    }
    return(gstr)
  }
}

knit_print.nVennR2 = function(x, ...) {
    g <- getVennSvg(x)
    knitr::asis_output(g)
}
# register the method
registerS3method("knit_print", "nVennObj", knit_print.nVennR2)
local({
  hook_source <- knitr::knit_hooks$get('source')
  knitr::knit_hooks$set(source = function(x, options) {
    x <- x[!grepl('#noshow$', x)]
    hook_source(x, options)
  })
})


## ----setup--------------------------------------------------------------------
library(nVennR2)
exampledf

## ----example1-----------------------------------------------------------------
myv <- nVennDiagram(exampledf)
myv #noshow

## ----example2-----------------------------------------------------------------
myv <- nVennDiagram(exampledf, verbose = F)
myv #noshow

## ----fromText-----------------------------------------------------------------
toVenn <- 'Set1 Set2 Set3
a a b
b q d
c  e'
myv2 <- nVennDiagram(toVenn, byCol = 1, verbose = F)
myv2 #noshow

## ----obj----------------------------------------------------------------------
myv2 <- nVennDiagram(myv2, verbose = F)
myv2 #noshow

## ----exhaustive---------------------------------------------------------------
estTime <- estimateExhaustiveRunTime(exampledf, 4)
estTime
if (estTime < 10){
  myv2 <- nVennDiagram(exampledf, maxlevel = 4)
}
myv2 #noshow

## ----getRegion----------------------------------------------------------------
getVennRegion(myv, c('SAS', 'R'))

## ----listRegions--------------------------------------------------------------
regs <- listVennRegions(myv)
regs

## ----opacity------------------------------------------------------------------
myv2 <- setVennOpts(myv2, opacity = 0.2, lineWidth = 2, palette = 3, showRegions = F)
myv2 #noshow

## ----colors-------------------------------------------------------------------
myv2 <- setVennColor(myv2, "Set2", 'black')
myv2 #noshow

## ----optPalette---------------------------------------------------------------
myv2 <- setVennOpts(myv2, palette = 2)
myv2 #noshow

## ----setPalette---------------------------------------------------------------
myv2 <- setVennPalette(myv2, palette = 2)
myv2 #noshow

## ----colorVector--------------------------------------------------------------
colorVector <- c("red", "grey")
myv2 <- setVennColors(myv2, colorVector)
myv2 #noshow

## ----colorList----------------------------------------------------------------
colorList <- list(Set1="blue", Set3="#00ff11")
myv2 <- setVennColors(myv2, colorList)
myv2 #noshow

## ----skin---------------------------------------------------------------------
mytheme <- list(opacity=0.2, lineWidth=2, fontSize=16, showRegions=F, colors=c("red", "green", "blue", "black", "#ffff00"))
myv2 <- setVennSkin(myv2, mytheme)
myv <- setVennSkin(myv, mytheme)
myv2 #noshow
myv #noshow

## ----rotate-------------------------------------------------------------------
plotVenn(myv2)
myv2 #noshow
myv2 <- rotateVenn(myv2, 30)
myv2 #noshow

