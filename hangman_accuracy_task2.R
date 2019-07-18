library(stringi)
library(stringr)
getwd()
words<-read.delim("words.txt",header=F) 

consonants <- c("b","c","d","f","g","h","j","k","l","m","n","p","q",
                "r","s","t","v","w","x","y","z")
vowels <- c("a","e","i","o","u")

catn <- function(s) #this function is used for printing a variable and for next line  
{
  cat(s,"\n",sep="") 
}
inputword<-""
x=0
len=0

start<-function(extracted_word) #main function ----this starts the game ---takes an input first, give your input in the console  
{
  inputword<-extracted_word# reading the string from console (user input)
  inputword<-tolower(inputword)
  x <- nchar(inputword) 
  guessed_string<-"" #guessed string--->will be storing the correctly guessed characters
  missed<-""  #missed---> stores the wrong guessed characters
  

  len <- nchar(inputword)
  #printing the empty base before the program makes a guess.
  missed<-paste(missed,"missed :")
  attempt=0
  cal=0
  count=0
  # from here program starts guessing the letters, after 6 wrong attempts it will stop.
  while(attempt!=6)
  {
    
    val<-sample(vowels,size=1) 
    val1<-sample(consonants,size=1)
    if(str_detect(inputword,val))  #checks if the letter exists in the given string
    {
      if(str_detect(guessed_string,val)==F){
      count=count+1
        guessed_string<-paste(guessed_string,val)
      }
      if(nchar(guessed_string)==nchar(inputword)) #if matched print the output and program terminates here.
      {
        break;
      }
      val=""
    }
    else if(str_detect(inputword,val1))
    {
      if(str_detect(guessed_string,val1)==F){
      count=count+1
      guessed_string<-paste(guessed_string,val1)
      }
    if(nchar(guessed_string)==nchar(inputword)) #if matched print the output and program terminates here.
    {
    break;
    }
      val1=""
      
    }
    else  #letters which do not match are stored in missed.
    {
      re<-str_locate_all(pattern=val,missed)
      
      re1<-str_locate_all(pattern=val1,missed) #------------>line mentioned in the note at starting
      
      if(length(re[[1]])==0) #condition makes sure if the letter is not repeated. If yes then attempt is not counted.
      {
        
        missed<-paste(missed,val)
       
        attempt=attempt+1
      }
      else if(length(re1[[1]])==0)  #------------>lines mentioned in the note at starting
      {
        
        missed<-paste(missed,val1)
        
        attempt=attempt+1
      }
    }
  }
  cal=(count/len)*100
  cal=round(cal,0)
  cat(c(inputword,"--->",cal,"%","\n"))
}


j=1
while(j<=nrow(words))
{
  start(words[j,1])
  j=j+1
}
