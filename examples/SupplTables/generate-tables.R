library(openxlsx)
library(pedsuite)
library(tibble)

fil = "SupplementaryTables.xlsx"

### Create workbook for storing datasets 
wb = createWorkbook(fil)   # wb = loadWorkbook(fil)

### DVI dataset

dvi = readRDS(file = "../example2-dvi/dvi.rds")

PM = getGenotypes(dvi$pm) |> as.data.frame() |> rownames_to_column("ID")
PM
AM = getGenotypes(dvi$am, "R1") |> as.data.frame() |> rownames_to_column("ID")
AM

addWorksheet(wb, "DVI")
writeData(wb, sheet = "DVI", rbind(PM, AM))


### Dataset for parametric example

swapped = readRDS(file = "../example3-parametric/swapped.rds")

ids = LETTERS[1:4]
g = getGenotypes(swapped, ids) |> as.data.frame() |> rownames_to_column("ID")
g

addWorksheet(wb, "Parametric")
writeData(wb, sheet = "Parametric", g)


### Database (ladder format)

db = getFreqDatabase(swapped, format = "ladder") |> as.data.frame() |> 
  rownames_to_column("Allele")
db

addWorksheet(wb, "Database")
writeData(wb, sheet = "Database", db)

### SAVE
saveWorkbook(wb, fil, overwrite = TRUE) 
