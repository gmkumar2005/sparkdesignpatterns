
val postsXML = sc.textFile("Posts.xml")
val counts = postsXML.flatMap(line => line.split(" "))
  .map(word => (word, 1)) // Map Phase
  .reduceByKey(_ + _) // Reduce phase
counts.saveAsTextFile("simplewordcountresultsch1") // serialize the results
