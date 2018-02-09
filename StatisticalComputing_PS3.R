# Let's Make a Deal

# S3

a <- sample(1:3, 1) #Choose a random door (a), numbered between 1 & 3
class(a) <- "door" #set class of a to door

#define generic PlayGame method
PlayGame <- function(input) {
  useMethod("PlayGame", input)
}

#define door's PlayGame Method
PlayGame.door <- function(x) {
  val = sample(1:3, 1) # randomly choose location of car
  # if car location is the same as initially selected door, you win
  if(val == x) {
    print("Congrats - you win big!")
  }
  # if car location is not the same as initially selected door, you lose
  else {
    print("Lol sorry, you get a goat!")
  }
}

PlayGame.door(a) #test

# S4

# create door class with one argument (numeric) that allows for making of a door object
setClass(Class="door",
         representation = representation(
           door = "numeric"
         ),
         prototype = prototype(
           door = c()
         )
)

# validation function
# ensure that any object of class door must have a value of 1, 2, or 3, else return error message
setValidity("door", function(object){
  test <- (object@door == 1 | object@door == 2 | object@door == 3)
  if(!test){return("@door is not a valid value")}
} )

# initialize
setMethod("initialize", "door", function(.Object, ...) {
  value = callNextMethod()
  validObject(value)
  return(value)
})

c <- new("door", door = 1) # test door creation

# create generic PlayGames method
setGeneric("PlayGames",
           function(object="door") {
             standardGeneric("PlayGames")
           } )
# create PlayGames.door method
setMethod("PlayGames", "door",
          function(object){
            val = sample(1:3, 1) # randomly choose location of car
            # if car location is the same as initially selected door, you win
            if(val == object@door) {
              return("Congrats!")
            }
            # if car location is not the same as initially selected door, you lose
            else {
              return("Sorry, you lose!")
            }
          } )

PlayGames(c) # test function



