# generate constrain files
print("Note: Checking the No. of Constraint")
nrow.check=0
# select min,max or plan dim
index=cstr.name!=names(result)[loop.cstr]
ex.cstr.input=ex.cstr.input[,!cstr.name[index],with=F]
temp.cstr.input=ex.cstr.input[!is.na(ex.cstr.input[[names(result)[loop.cstr]]])]
if (nrow(temp.cstr.input)!=0) {
  nrow.check=1
}


