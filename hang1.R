#NOTE:- Please do read this..... 
#I have made this Hangman program using R language. 
#I have tried to develope my program as near possibly correct.
#I have tried my best and put my best efforts.

#*********************************************************************

#In my program if we comment the lines 97,106-112. 
#My program will always end up in finding the correct word,and it will always win. 
#But if we keep these lines  95,104-110. uncommented, the program might win/lose in some cases.
# start()---> this will begin the program just write start() and then next line give input.

#***************************************************************************************
library(stringi)
library(stringr)
consonants <- c("b","c","d","f","g","h","j","k","l","m","n","p","q",
                "r","s","t","v","w","x","y","z")
vowels <- c("a","e","i","o","u")
catn <- function(s) #this function is used for printing a variable and for next line  
{
  cat(s,"\n",sep="") 
}

start<-function() #main function ----this starts the game ---takes an input first, give your input in the console  
{
  inputword<-readline()# reading the string from console (user input)
inputword<-tolower(inputword)
  guessed_string<-NULL #guessed string--->will be storing the correctly guessed characters
missed<-NULL  #missed---> stores the wrong guessed characters

  x <- nchar(inputword) 
len <- nchar(inputword)
#printing the empty base before the program makes a guess.
while (x != 0) 
  {
    guessed_string=paste(guessed_string,'_',sep="")
    x = x - 1
    if (x == 0) {
      missed<-paste(missed,"missed :")
    }
    
  }
catn(c(guessed_string,missed))
attempt=0
# from here program starts guessing the letters, after 6 wrong attempts it will stop.
while(attempt!=6)
{
  
  val<-sample(vowels,size=1) 
  val1<-sample(consonants,size=1)
  i=1
  if(str_detect(inputword,val)) #checks if the letter exists in the given string
  {
      cat("guess :",val,"\n\n")
      index<-str_locate_all(pattern=val,inputword)
      while(i<=len)
      {
        if(is.element(i,index[[1]]))
        {
          substr(guessed_string,i,i)<-val
        }
        i=i+1
      }
      if(guessed_string==inputword) #if matched print the output and program terminates here.
      {
        catn(c(guessed_string,missed))
        break;
      }
      catn(c(guessed_string,missed))
      val=""
    }
    else if(str_detect(inputword,val1))
    {
      cat("guess :" ,val1,"\n\n")
      index<-str_locate_all(pattern=val1,inputword)
      while(i<=len)
      {
        if(is.element(i,index[[1]]))
        {
          substr(guessed_string,i,i)<-val1
        }
        i=i+1
      }
      if(guessed_string==inputword)
      {
        catn(c(guessed_string,missed))
        break;
      }
      catn(c(guessed_string,missed))
      val1=""
      
    }
  else  #letters which do not match are stored in missed.
  {
    re<-str_locate_all(pattern=val,missed)
    
    re1<-str_locate_all(pattern=val1,missed) #------------>line mentioned in the note at starting
    
    if(length(re[[1]])==0) #condition makes sure if the letter is not repeated. If yes then attempt is not counted.
     {
      cat("guess :",val,"\n\n")
     missed<-paste(missed,val)
      catn(c(guessed_string,missed))
      attempt=attempt+1
    }
    else if(length(re1[[1]])==0)  #------------>lines mentioned in the note at starting
    {
    cat("guess :",val1,"\n\n")
      missed<-paste(missed,val1)
      catn(c(guessed_string,missed))
      attempt=attempt+1
    }
  }
}
}
start()
