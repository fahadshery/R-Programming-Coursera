complete <- function(directory, id = 1:332) {
        
        #set the path
        path = "C:/Users/Fahad/Documents/R Projects/Coursera/R-Programming-Coursera/Data/"
        path = c(path,directory)
        path = paste(path,collapse="")
        #get the file List in that directory
        fileList = list.files(path)
        
        #extract the file names and store as numeric for comparison
        file.names = as.numeric(sub("\\.csv$","",fileList))
        
        #select files to be imported based on the user input or default
        selected.files = fileList[match(id,file.names)]
        
        #import data
        Data = lapply(file.path(path,selected.files),read.csv)
        
        #get the complete cases
        completeCases = lapply(Data, function(Data) Data[complete.cases(Data),])
        
        #create a dataframe to return
        res = data.frame()
        
        #get the number of complete cases and put it in the result dataframe
        for(i in 1:length(completeCases)){
                res <- rbind(res, nobs=nrow(completeCases[[i]]))
        }
        
        #add ID column
        res <- cbind(res,id=id)
        
        #remove rownames
        rownames(res) <- NULL
        
        #add column names
        colnames(res) <- c("nobs","id")
        
        #re-arrange column names
        res = res[c("id", "nobs")]
        
        #return the results dataframe
        res

}

