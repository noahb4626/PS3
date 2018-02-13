# Activity 5: Sorting Hat

#1 Students

# create generic assign method
assign <- function(name) {
  UseMethod("assign", name)
}
# assign student 4 random integer values b/w 1-100 for 4 attributes
assign.student <- function(name) {
  courage <- sample(1:100, 1)
  ambition <- sample(1:100, 1)
  intelligence <- sample(1:100, 1)
  effort <- sample(1:100, 1)
  a <- (c(courage, ambition, intelligence, effort)) # set 'a' to the set of these 4 values
  class(a) <- "student" # set a's class to student
  return(a) #return a
}
# test with Luna
Luna <- assign.student(Luna)
Luna
class(Luna)

#2 Sorter

# create generic sort method
sort <- function(name) {
  UseMethod("sort", name)
}
# create sort method for student class that accepts a name and matrix multiplier as input
sort.student <- function(name, x) {
  finalsort <- t(x) %*% (assign.student(name))
  finalsort <- t(finalsort)
  print(finalsort) # optional step to display the values associated with each attribute
  # depending on maximum value in final matrix, return house corresponding to largest value
  if(max(finalsort) == finalsort[1]) {return("GRYFFINDOR!")}
  if(max(finalsort) == finalsort[2]) {return("SLYTHERIN!")}
  if(max(finalsort) == finalsort[3]) {return("RAVENCLAW!")}
  if(max(finalsort) == finalsort[4]) {return("HUFFLEPUFF!")}
}
# test sort with Draco
sort.student(Draco, diag(4))

#3 Modifications

# create generic sort2 method
sort2 <- function(name) {
  UseMethod("sort2", name)
}
# create sort2 method for student class that accepts a name and matrix multiplier as input
sort2.student <- function(name, x) {
  name <- assign.student(name)
  finalsort <- t(x) %*% (name)
  finalsort <- t(finalsort)
  # depending on maximum value in final matrix, add a new class to student corresponding to that house
  # return class of the entered student (should be student & [INSERT HOUSE HERE])
  if(max(finalsort) == finalsort[1]) {
    class(name) <- c(class(name), "Gryffindor")
    return(name)
  }
  if(max(finalsort) == finalsort[2]) {
    class(name) <- c(class(name), "Slytherin")
    return(name)
  }
  if(max(finalsort) == finalsort[3]) {
    class(name) <- c(class(name), "Ravenclaw")
    return(name)
  }
  if(max(finalsort) == finalsort[4]) {
    class(name) <- c(class(name), "Hufflepuff")
    return(name)
  }
}
# test case with Harry
Harry <- sort2.student(Harry, diag(4))
class(Harry)

#4 Curfew

# create 4 new environments, one for each house
Gryffindor_Tower <- new.env()
Black_Lake <- new.env()
Ravenclaw_Tower <- new.env()
Basement <- new.env()
# create generic curfew function
curfew <- function(name) {
  UseMethod("curfew", name)
}
# create curfew commands for each house that accept a student and place them in their dormitory
curfew.Gryffindor <- function(name) {
  Gryffindor_Tower$student <- c(Gryffindor_Tower$student, name)
}
curfew.Slytherin <- function(name) {
  Black_Lake$student <- c(Black_Lake$student, name)
}
curfew.Ravenclaw <- function(name) {
  Ravenclaw_Tower$student <- c(Ravenclaw_Tower$student, name)
}
curfew.Hufflepuff <- function(name) {
  Basement$student <- c(Basement$student, name)
}
# test: create & sort three new students
Dean <- sort2.student(Dean, diag(4))
Seamus <- sort2.student(Seamus, diag(4))
Cho <- sort2.student(Cho, diag(4))
# send them to their dormitories
curfew(Dean)
curfew(Seamus)
curfew(Cho)
# display integer values associated with each student in its appropriate environment
ls.str(Gryffindor_Tower)
ls.str(Black_Lake)
ls.str(Ravenclaw_Tower)
ls.str(Basement)
