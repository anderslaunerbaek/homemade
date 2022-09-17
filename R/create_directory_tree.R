# file_names


dir <- "~/DTU/Courses/MSc/Code"
ignore <- ".DS_Store|.git|.Rproj.user|.tex|Final project/|Cases/|Presentations/"


file_names <- list.files(path = dir,recursive = T, all.files = T, full.names = F)
file_names <- sort(file_names[!stringi::stri_detect(file_names, regex = ignore)])
file_names <- stringi::stri_split(file_names, regex = "/")

file_names
tmp_list_names <- c()
for (i in 1:length(file_names)) {
  tmp_list_names <- c(tmp_list_names, file_names[[i]][1])
}

list_names <- list()
for (i in unique(tmp_list_names)) {
  list_names[[i]]
}


