
val postsXML = sc.textFile("Posts.xml")

// to handle bad records
case class corruptedPost(line: String)

// to handle good records
case class Post(
                 Id: Long,
                 PostTypeId: Long,
                 AcceptedAnswerId: Long,
                 CreationDate: String,
                 Score: Int,
                 ViewCount: Int,
                 Body: String,
                 OwnerUserId: Long,
                 LastEditorUserId: Long,
                 LastEditorDisplayName: String,
                 LastEditDate: String,
                 LastActivityDate: String,
                 Title: String,
                 Tags: String,
                 AnswerCount: Int,
                 CommentCount: Int,
                 FavoriteCount: Int,
                 CommunityOwnedDate: String
                 )

def toInt(s: String): Int = {
  try {
    s.trim.toInt
  } catch {
    case e: Exception => 0
  }
}
def toLong(s: String): Long = {
  try {
    s.trim.toLong
  } catch {
    case e: Exception => 0
  }
}
def toText(s: String): String = {
  try {
    StringEscapeUtils.unescapeHtml(s.trim)
  } catch {
    case e: Exception => ""
  }
}


def parsePostsXML(line: String) = {
  try {
    val parsedLine = scala.xml.XML.loadString(line)
    Post(
      toInt(parsedLine \ "@Id" + ""),
      toLong(parsedLine \ "@PostTypeId" + ""),
      toLong(parsedLine \ "@AcceptedAnswerId" + ""),
      toText(parsedLine \ "@CreationDate" + ""),
      toInt(parsedLine \ "@Score" + ""),
      toInt(parsedLine \ "@ViewCount" + ""),
      toText(parsedLine \ "@Body" + ""),
      toLong (parsedLine \ "@OwnerUserId" + ""),
      toLong(parsedLine \ "@LastEditorUserId" + ""),
      toText(parsedLine \ "@LastEditorDisplayName" + ""),
      toText(parsedLine \ "@LastEditDate" + ""),
      toText(parsedLine \ "@LastActivityDate" + ""),
      toText(parsedLine \ "@Title" + ""),
      toText(parsedLine \ "@Tags" + ""),
      toInt(parsedLine \ "@AnswerCount" + ""),
      toInt(parsedLine \ "@CommentCount" + ""),
      toInt(parsedLine \ "@FavoriteCount" + ""),
      toText(parsedLine \ "@CommunityOwnedDate" + "")
    )
  } catch {
    case e: Exception => corruptedPost(line)
  }
}

val parsedObjects = postsXML.map(parsePostsXML)
val goodRecords = parsedObjects.filter { case p: Post => true case _ => false}
// remove all single quotes, remove all special characters
//val countsMapper = goodRecords.flatMap{case p:Post => p.Body.toLowerCase().replaceAll("'", "").replaceAll("[^a-zA-Z]", " ").split(" ").map(word => (word, 1))}
// remove all single quotes, remove all special characters and remove all words with length less than two
val countsMapper = goodRecords.flatMap { case p: Post => p.Body.replaceAll("\\b[\\w']{1,2}\\b", "").replaceAll("[^a-zA-Z]", " ").replaceAll("\\s{2,}", " ").split(" ").map(word => (word, 1))}
val countReducer = countsMapper.reduceByKey(_ + _)


