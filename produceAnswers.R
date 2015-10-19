library(write.table)


# 
# 
# ## Create the required working folder.
# if (!file.exists("problem_results")) {
#   dir.create("problem_results")
# }
# 

answers <- result
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)
