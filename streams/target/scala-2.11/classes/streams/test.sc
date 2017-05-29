val levelVector = Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
val c = 'o'

//{for{
//  i <- 0 to levelVector.length
//  if(i<levelVector.length)
//  j <- 0 to levelVector(i).length
//  if(j<levelVector(i).length && levelVector(i)(j) == c)
//} yield (i,j)}.head


levelVector.foldRight(){
  (v,x) => v.indexOf(c)
}
