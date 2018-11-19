# Converts the list output of RIT to a data frame
# Not exported
convert.to.data.frame <- function(Inter_Prev_List) {
  str_inter <- character(length(Inter_Prev_List$Interactions))
  for (i in seq_along(Inter_Prev_List$Interactions)) {
    str_inter[i] <- paste(Inter_Prev_List$Interactions[[i]],collapse=" ")
  }
  names <- setdiff(names(Inter_Prev_List), "Interactions")
  data <- data.frame(Interaction=str_inter, stringsAsFactors = FALSE)
  for (name in names) {
      data[[name]] <- Inter_Prev_List[[name]]
  }
  return(data)
}
