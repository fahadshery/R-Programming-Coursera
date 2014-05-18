corr <- function(directory, threshold = 0) {
        
        #set the path
        path = "C:/Users/Fahad/Documents/R Projects/Coursera/R-Programming-Coursera/Data/"
        path = c(path,directory)
        path = paste(path,collapse="")
        
        #get the file List in that directory
        fileList = list.files(path)
        
        #import data
        Data = lapply(file.path(path,fileList),read.csv)
        
        #get the complete cases
        completeCases = lapply(Data, function(Data) Data[complete.cases(Data),])
        
        #create a dataframe to return the results
        res = numeric()
        
        
        #compare the no. of obs in complete cases against threshold and calculate 
        #correlations and save results in the res vector
        for(i in 1:length(completeCases)){
                if(nrow(completeCases[[i]])>=threshold){
                        res2 = data.frame()
                        res2 <- as.data.frame(completeCases[[i]])
                        res[i] <- cor(res2$sulfate, res2$nitrate)
                        rm(res2)
                
                }
                
        }
        res = res[!is.na(res)]
        res
}


