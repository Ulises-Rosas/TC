
IDs = utils::read.table("segmentacion.txt",
                        sep = "\n",
                        header = F,
                        stringsAsFactors = F)

is.there_contiguous_repetition <- function(string){

        len = nchar(string)
        i = 1

        while(i < len){

                first_Letter = substr(string, i, i)

                second_Letter = substr(string, i + 1, i + 1)

                if(first_Letter == second_Letter){

                        return(TRUE)

                        break
                }
                i = i + 1
        }
        return(FALSE)
}

is.there_vowel <- function(string){

        vowels = c("a", "e", "i", "o", "u")

        all_vowels = c(vowels, toupper(vowels))

        all_characters = strsplit(string, split = "" )[[1]]

        if(any(all_characters %in% all_vowels) == T){
                TRUE
        }else{
                FALSE
        }

}

is.there_oddDigit <- function(string){

        odd_Digits = c("1", "3", "5", "7", "9")

        all_characters = strsplit(string, split = "")[[1]]

        if(any( all_characters %in% odd_Digits  ) == T ){
                TRUE
        }else{
                FALSE
        }

}


is.upperCase_dominated <- function(string){

        all_characters = strsplit(string, split = "")[[1]]

        upperCase = all_characters[all_characters %in% LETTERS]

        lowerCase = all_characters[all_characters %in% letters]


        if( length(upperCase) > length(lowerCase) ){
                TRUE
        }else{
                FALSE
        }

}

are.there_specific_Seqs <- function(string){

        #bunch of sequences which will be assessed through the string
        Seqs = c("jn", "cg", "ar", "mp", "fs", "ic")

        len = length(Seqs)
        i = 1

        while(i <= len){
                ## assess every single sequence till
                ## get a match with the string
                if( grepl( Seqs[i], string ) ){

                        return(TRUE)
                        ## upon having a match,
                        ## break the while loop
                        break
                }
                ## if there was any match,
                ## allow next assessment
                i = i + 1
        }
        ## return a FALSE if there were specific sequences within
        ## the string
        return(FALSE)
}


client_classifier <- function(IDS){

        categories = vector("character")


        pb <- txtProgressBar(min = 0, max = length(IDS), style = 3, char = "*")

        for( i in  1:length(IDS) ){

                if(  is.there_contiguous_repetition(  IDS[i] ) &&
                     is.there_vowel( IDS[i] )  ){

                        categories[i] ="A"
                        #print("A")
                }else{
                        if(  is.upperCase_dominated( IDS[i] ) ||
                             is.there_oddDigit( IDS[i]  )  ){

                                categories[i] ="B"
                                #print("B")
                        }else{
                                if( are.there_specific_Seqs( IDS[i] )  ){

                                        categories[i] ="C"
                                        #print("C")

                                }else{
                                        categories[i] ="D"
                                        #print("D")
                                }
                        }

                }

                setTxtProgressBar(pb, i)
        }
        close(pb)
        return( table(categories) )
}

results <- client_classifier( IDs$V1 )

write.table(results, "results1.txt", sep = "\t", quote = F, row.names = F)
