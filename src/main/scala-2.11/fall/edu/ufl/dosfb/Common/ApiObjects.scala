package fall.edu.ufl.dosfb.Common

import fall.edu.ufl.dosfb.fbserver.HTTPServer.{posts, userPosts}
import spray.http.DateTime
import spray.httpx.SprayJsonSupport
import spray.json._
import scala.collection.mutable
import scala.collection.mutable._



/**
 * Created by pranav on 11/30/15.
 */
case class User(var id:Long,
                firstName:String,
                lastName:String,
                email:Option[String],
                birthday:Option[DateTime],
                gender:Option[String],
                pubKey1:Option[String],
                pubKey2:Option[String],
                friendsList:ListBuffer[Long],
                photos:ListBuffer[Long],
                posts:ListBuffer[Long],
                albums: ListBuffer[Long])

case class Page(var id:Long,
                title:Option[String],
                description:Option[String],
                website:Option[String],
                var writtenBy:Option[Long],
                photos:ListBuffer[Long],
                posts:ListBuffer[Long],
                albums: ListBuffer[Long])

case class post(var id:Long, // The ID of the owner of this Post
                message:String,
                // Optional -- Remove this if possible -- Have to connect to a post/page/whatever if possible
                var picture:Option[Photo], //Link to a picture
                description:Option[String],
                link:Option[String], //Link associated with the post
                var edgeInfo:Option[Edge])


case class Photo(var id:Long, // The ID of the owner of this Photo
                 height:Int,
                 width:Int,
                 link:String, //Link from which the photo should be picked
                 var edgeInfo:Option[Edge])

case class Album(var id:Long,
                 var coverPhoto:Option[Photo],
                 name:Option[String],
                 var count: Option[Long],
                 var photos:List[Photo],
                 var edgeInfo:Option[Edge])


case class Edge(owner: Long, ownerNodeType:NodeType.NodeType, createdBy:Long)
//case class Edge(owner: String, ownerNodeType:NodeType.NodeType, createdBy:String)
case class ObjectId(id:Long)
case class Success(status:String="Success")

object DefaultJsonFormatterProtocol extends DefaultJsonProtocol with SprayJsonSupport{
  implicit object dateFormat extends RootJsonFormat[spray.http.DateTime] {
    def read(json: JsValue): spray.http.DateTime =
      DateTime.fromIsoDateTimeString(json.convertTo[String]) getOrElse (null)
    def write(date: DateTime) = JsString(date.toIsoDateTimeString)
  }

  implicit object objectTypeFormat extends RootJsonFormat[NodeType.NodeType] {
    def read(json: JsValue): NodeType.NodeType = {
      var nodeType = NodeType.User
      val strValue = json.compactPrint
      if(strValue.equals("Page"))
        nodeType = NodeType.Page
      nodeType
    }
    def write(_type: NodeType.NodeType) = JsString(_type.toString)
  }

  implicit def muListBufferFormat[T :JsonFormat] = viaSeq[mutable.ListBuffer[T], T](seq => mutable.ListBuffer(seq :_*))

  case class ekeys(var id: Long,
                   var key: String)

  case class encryptedPost( var post:post,
                            var accessList:ListBuffer[ekeys],
                            var IV : String)

  implicit val successFormat  = jsonFormat1(Success)
  implicit val edgeFormat     = jsonFormat3(Edge)
  implicit val pageFormat     = jsonFormat8(Page)
  implicit val photoFormat    = jsonFormat5(Photo)
  implicit val postFormat     = jsonFormat6(post)
  implicit val albumFormat    = jsonFormat6(Album)
  implicit val objectIdFormat = jsonFormat1(ObjectId)
  implicit val userFormat     = jsonFormat12(User)
  implicit val keyFormat      = jsonFormat2(ekeys)
  implicit val ePost          = jsonFormat3(encryptedPost)
  implicit val postsFormat    = jsonFormat1(posts)
}
