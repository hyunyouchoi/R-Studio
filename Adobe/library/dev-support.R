
### General Purpose Utilities ##################################################

zip_to_list = function(a,b) {
  z = list()
  i = 1
  for (element in a) {
    z[[element]] = b[i]
    i = i + 1
  }
  z
}

rm_blanks = function(u) {

   i = which(u=="")
   if (length(i) == 0) {
     out = u
   } else {
     out = u[-i]
   }
  out
}

concat = function(...) {
  paste(..., sep="")
}

get_excel = function(file, sheet) {
  # needs openxlsx package
  read.xlsx(
      file
    , sheet=sheet
    , colNames=TRUE
    , startRow=1
    , skipEmptyRows=FALSE
    , skipEmptyCols=FALSE
    , detectDates=TRUE
    , check.names=TRUE
    , na.strings="NA"
  )
}

