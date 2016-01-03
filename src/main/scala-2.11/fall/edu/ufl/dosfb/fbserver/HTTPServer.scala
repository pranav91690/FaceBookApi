package fall.edu.ufl.dosfb.fbserver

import java.security.{KeyPairGenerator, Signature, PublicKey}
import java.util.Base64
import java.util.concurrent.TimeUnit
import fall.edu.ufl.dosfb.Common.DefaultJsonFormatterProtocol.encryptedPost
import fall.edu.ufl.dosfb.Common.KeyValuePairs
import akka.actor.{ActorLogging, Props, Actor, ActorSystem}
import akka.util.Timeout
import fall.edu.ufl.dosfb.Common.NodeType.NodeType
import fall.edu.ufl.dosfb.Common._
import spray.http.{HttpMethods, HttpResponse, Uri, HttpRequest}
import spray.util.{SprayActorLogging, LoggingContext}
import akka.actor.{ Actor, ActorLogging, ActorSystem, Props }
import akka.io.IO
import spray.can.Http
import spray.http.MediaTypes.`application/json`
import akka.actor.{ActorContext, ActorRef, Actor}
import spray.routing._
import spray.routing.directives.{DebuggingDirectives, LoggingMagnet}
import akka.pattern.{AskTimeoutException, ask}
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContextExecutor, Future}
import spray.routing.RejectionHandler.Default
import spray.http.{HttpRequest, MediaTypes}
import scala.concurrent.duration._
import scala.util.Random


/**
 * Created by pranav on 11/26/15.
 */
// This is the Central HTTP Server which receives HTTP Requests from the Clients
object HTTPServer extends App {
  // User Case Classes
  case object Tick

  case class CreateUser(User: User)

  case class UpdateUser(User: User)

  case class GetUser(id: Long)

  case class addFriend(User: Long, Friend: Long)

  case class GetFriends(UserID: Long)

  // Page Case Classes
  case class CreatePage(Page: Page)

  case class UpdatePage(Page: Page)

  case class GetPage(id : Long)

  // Post Case Classes
  case class CreatePost(Post: post)
  case class CreateEPost(ePost : encryptedPost)

  case class GetPosts(id: Long, Type: NodeType)

  // Photos Case Classes
  case class PostPhoto(Photo: Photo)

  case class GetPhotos(id: Long, Type: NodeType)

  // Albums Case Classes
  case class CreateAlbum(Album: Album, Type: NodeType)

  case class GetAlbums(id: Long, Type: NodeType)

  // Fb Exception
  case class FBException(message: String) extends Exception(message)

  case class posts(posts : ListBuffer[encryptedPost])

  // Statistics
  var requestHandled = 0
  
  var userPosts = new ListBuffer[encryptedPost]


  val generator = KeyPairGenerator.getInstance("RSA")
  generator.initialize(1024)
  val keyPair = generator.genKeyPair()

  // Convert String into Bytes to Decrypt
  def base64Decode(encodedStr: String): Array[Byte] = {
    Base64.getDecoder.decode(encodedStr)
  }

  // Verify Method
  def verify(data:String, signedValue:String, pubKey: PublicKey):Boolean = {
    val verifier = Signature.getInstance("SHA256withRSA")
    verifier.initVerify(pubKey)
    verifier.update(data.getBytes("UTF-8"))
    verifier.verify(base64Decode(signedValue))
  }

  println("Server Started")

  // This is the Point at which our service starts
  System.setProperty("java.net.preferIPv4Stack", "true")
  implicit val system = ActorSystem("FBSimulator")
  // Use Akka to Create our Spray Service
  val server = system.actorOf(Props(new Server()), "ServerSystem")
  // Create the Worker Actor
  val userWorker = system.actorOf(Props[UserWorker], name = "userWorker")
  // Create PageWorker
  val pageWorker = system.actorOf(Props[PageWorker], name = "pageWorker")
  // and Bind to Akka's I/O Interface
  IO(Http) ! Http.Bind(server, interface = "localhost", port = 8080)
  var counter : Long = 0

  // The Worker Actor that implements the Business Logic for User
  class UserWorker extends Actor {
//    import context.dispatcher
//    val cancel = context.system.scheduler.schedule(0 milliseconds, 120000 milliseconds, self, Tick)
    def receive = {
      // Receive Messages From the Service
      case CreateUser(user) =>  {
        println("User Create Request Rvd")
        // Put User Object in KV Pair -
        // Return Success to Sender
        requestHandled += 1
        counter += 1
        user.id = counter
        KeyValuePairs.putUser(user)
        sender() ! user
      }

      case UpdateUser(user) => {
        requestHandled += 1
        KeyValuePairs.updateUser(user)
        sender() ! "Success"
      }

      case GetUser(userId) => {
        requestHandled += 1
        KeyValuePairs.getUser(userId)
        sender() ! "Success"
      }

      case addFriend(userID, friendID) => {
        println("Add Friend Request Rvd")
        requestHandled += 1
        KeyValuePairs.CreateFriendList(userID, friendID)
        sender() ! "Success"
        // Add Friend, Return Success
      }

      case GetFriends(userID) => {
        requestHandled += 1
        KeyValuePairs.getFriend(userID)
        sender() ! "Success"
      }

      case CreatePost(post) => {
        // Verify User
        println("Create Post Request Rvd")
        requestHandled += 1
        counter += 1
        post.id = counter
        sender() ! "Success"
      }

      case CreateEPost(ePost) => {
        println("Create Encrypted Post Request Rvd")
        requestHandled += 1
        counter += 1
        ePost.post.id = counter
        userPosts += ePost
        sender() ! "Success"
      }

      case GetPosts(id, nodeType) => {
        println("Get Encrypted Posts Rvd")
        requestHandled += 1
        val tempPosts = new ListBuffer[encryptedPost]
        
        for(i <- 0 until Random.nextInt(5)) {
         tempPosts += userPosts(Random.nextInt(userPosts.size))
        }

        val resp = posts(tempPosts)
        sender() ! resp
        println("Sent Encrypted Posts")
      }

      case PostPhoto(photo) => {
        requestHandled += 1
        counter += 1
        photo.id = counter
        KeyValuePairs.putPhoto(photo)
        sender() ! "Success"
      }

      case GetPhotos(id, nodeType) => {
        requestHandled += 1
        KeyValuePairs.getPhoto(id)
        sender() ! "Success"
      }

      case CreateAlbum(album, nodeType) => {
        requestHandled += 1
        counter += 1
        album.id = counter
        KeyValuePairs.putAlbum(album)
        sender() ! "Success"
      }

      case GetAlbums(id, nodeType) => {
        requestHandled += 1
        KeyValuePairs.getAlbum(id)
        sender() ! "Success"
      }

      case `Tick` => {
//        println("Number of Requests Handles Till Now U : " + requestHandled)
      }
    }
  }

  // The Worker Actor that implements the Business Logic for Page
  class PageWorker extends Actor {
//    import context.dispatcher
//    val cancel = context.system.scheduler.schedule(60000 milliseconds, 120000 milliseconds, self, Tick)
    def receive = {
      case CreatePage(page) => {
        println("Create Page Request Rvd")
        requestHandled += 1
        counter += 1
        page.id = counter
        KeyValuePairs.putPage(page)
        sender() ! page
      }

      case UpdatePage(page) => {
        requestHandled += 1
        KeyValuePairs.updatePage(page)
        sender() ! "Success"
      }

      case GetPage(pageID) => {
        requestHandled += 1
        KeyValuePairs.getPage(pageID)
        sender() ! "Success"
      }

      case CreatePost(post) => {
        requestHandled += 1
        counter += 1
        post.id = counter
        KeyValuePairs.putPagePosts(post)
        sender() ! "Success"
      }

      case GetPosts(id, nodeType) => {
        requestHandled += 1
////        val posts = new ListBuffer[post]
//        val actualPosts = KeyValuePairs.getPagePosts(id)
//        val tempPosts = new ListBuffer[post]
//        actualPosts.foreach(x => tempPosts += x.get)
//        val resp = posts(tempPosts)
//        println(tempPosts)
//        sender() ! resp
      }

      case PostPhoto(photo) => {
        requestHandled += 1
        counter += 1
        photo.id = counter
        KeyValuePairs.putPagePhoto(photo)
        sender() ! "Success"
      }

      case GetPhotos(id, nodeType) => {
        requestHandled += 1
        KeyValuePairs.getPagePhoto(id)
        sender() ! "Success"
      }

      case CreateAlbum(album, nodeType) => {
        requestHandled += 1
        counter += 1
        album.id = counter
        KeyValuePairs.putPageAlbum(album)
        sender() ! "Success"
      }

      case GetAlbums(id, nodeType) => {
        requestHandled += 1
        KeyValuePairs.getPageAlbum(id)
        sender() ! "Success"
      }

      case `Tick` => {
//        println("Number of Requests Handles Till Now : P " + requestHandled)
      }
    }
  }

  // This is the Main Spray Service Actor that Receives the Requests
  class Server extends Actor with SpraySampleService {
    // Actor System
    implicit val system = context.system

    // Actor Reference
    def actorRefFactory = context

    // Run the Different Spray Routes Here --->
    def receive = runRoute(userRouter ~ pageRouter)
  }

  trait SpraySampleService extends HttpService {
    implicit def exceptionHandler(implicit log: LoggingContext) =
      ExceptionHandler {
        case e: AskTimeoutException =>
          complete(FBException("TimeOut Exception"))
      }

    implicit val ec = actorRefFactory.dispatcher
    implicit val timeout = Timeout(10, TimeUnit.SECONDS)

    import fall.edu.ufl.dosfb.Common.DefaultJsonFormatterProtocol._

    val userRouter = {
      path("user") {
        // Insert a New User with given Details
        put {
          entity(as[User]) { user =>
            respondWithMediaType(`application/json`) {
              onSuccess(userWorker ? CreateUser(user)) {
                case result: User =>
                  complete(result)
                case ex: FBException =>
                  respondWithStatus(400) {
                    complete(ex)
                  }
              }
            }
          }
        } ~ post {
          // Update a User with the updated Details
          entity(as[User]) { updatedUser =>
            respondWithMediaType(`application/json`) {
              onSuccess(userWorker ? UpdateUser(updatedUser)) {
                case result: String =>
                  complete(result)
                case ex: FBException =>
                  respondWithStatus(400) {
                    complete(ex)
                  }
              }
            }
          }
        }
      } ~ pathPrefix("user" / LongNumber) { (id) => // If there is a User Id number along with a user
        pathEnd {
          get {
            // Get the User Details for that ID
            respondWithMediaType(`application/json`) {
              onSuccess(userWorker ? GetUser(id)) {
                case result: String =>
                  complete(result)
                case ex: FBException =>
                  respondWithStatus(401) {
                    complete(ex)
                  }
              }
            }
          }
        }
      } ~ path("user" / LongNumber / "friends") { (id) => // If there is friends along with it
        put {
          entity(as[ObjectId]) { friendId =>
            respondWithMediaType(`application/json`) {
              onSuccess(userWorker ? addFriend(id, friendId.id)) {
                case result: String =>
                  complete(result)
                case ex: FBException =>
                  respondWithStatus(400) {
                    complete(ex)
                  }
              }
            }
          }
        } ~ get {
          respondWithMediaType(`application/json`) {
            onSuccess(userWorker ? GetFriends(id)) {
              case result: String =>
                complete(result)
              case ex: FBException =>
                respondWithStatus(400) {
                  complete(ex)
                }
            }
          }
        }
      } ~ path("user" / LongNumber / "posts") { (id) => // ID of the Page on which the post is to be made
        put {
//          entity(as[post]) { post =>
//            onSuccess(userWorker ? CreatePost(post)) {
//              case result: String =>
//                complete(result)
//              case ex: FBException =>
//                respondWithStatus(400) {
//                  complete(ex)
//                }
//            }
//          }
            // Handle the Encrypted Post
            entity(as[encryptedPost]) { ePost =>
              onSuccess(userWorker ? CreateEPost(ePost)) {
                case result: String =>
                  complete(result)
                case ex: FBException =>
                  respondWithStatus(400) {
                    complete(ex)
                  }
              }
            }
        } ~ get {
          onSuccess(userWorker ? GetPosts(id, NodeType.User)) {
            case result : posts =>
              complete(result)
            case ex: FBException =>
              respondWithStatus(400) {
                complete(ex)
              }
          }
        }
      } ~ path("page" / LongNumber / "photos") { (id) =>
        put {
          entity(as[Photo]) { photo =>
            onSuccess(userWorker ? PostPhoto(photo)) {
              case result: String =>
                complete(result)
              case ex: FBException =>
                respondWithStatus(400) {
                  complete(ex)
                }
            }
          }
        } ~ get {
          onSuccess(userWorker ? GetPhotos(id, NodeType.User)) {
            case result: String =>
              complete(result)
            case ex: FBException =>
              respondWithStatus(400) {
                complete(ex)
              }
          }
        }
      }  ~ path("user" / LongNumber / "albums") { (id) =>
        put {
          entity(as[Album]) { album =>
            onSuccess(userWorker ? CreateAlbum(album, NodeType.User)) {
              case result: String =>
                complete(result)
              case ex: FBException =>
                respondWithStatus(400) {
                  complete(ex)
                }
            }
          }
        } ~ get {
          onSuccess(userWorker ? GetAlbums(id, NodeType.User)) {
            case result: String =>
              complete(result)
            case ex: FBException =>
              respondWithStatus(400) {
                complete(ex)
              }
          }
        }
      }
    }

    val pageRouter = {
      path("page") {
        // Insert a New User with given Details
        put {
          entity(as[Page]) { page =>
            respondWithMediaType(`application/json`) {
              onSuccess(pageWorker ? CreatePage(page)) {
                case result: Page =>
                  complete(result)
                case ex: FBException =>
                  respondWithStatus(400) {
                    complete(ex)
                  }
              }
            }
          }
        } ~ post {
          // Update a User with the updated Details
          entity(as[Page]) { updatedPage =>
            respondWithMediaType(`application/json`) {
              onSuccess(pageWorker ? UpdatePage(updatedPage)) {
                case result: String =>
                  complete(result)
                case ex: FBException =>
                  respondWithStatus(400) {
                    complete(ex)
                  }
              }
            }
          }
        }
      } ~ pathPrefix("page" / LongNumber) { (id) => // If there is a User Id number along with a user
        pathEnd {
          get {
            // Get the User Details for that ID
            respondWithMediaType(`application/json`) {
              onSuccess(pageWorker ? GetPage(id)) {
                case result: String =>
                  complete(result)
                case ex: FBException =>
                  respondWithStatus(400) {
                    complete(ex)
                  }
              }
            }
          }
        }
      } ~ path("page" / LongNumber / "posts") { (id) => // ID of the Page on which the post is to be made
        put {
          entity(as[post]) { post =>
            onSuccess(pageWorker ? CreatePost(post)) {
              case result: String =>
                complete(result)
              case ex: FBException =>
                respondWithStatus(400) {
                  complete(ex)
                }
            }
          }
        } ~ get {
          onSuccess(pageWorker ? GetPosts(id, NodeType.Page)) {
            case result:String =>
              complete(result)
            case ex: FBException =>
              respondWithStatus(400) {
                complete(ex)
              }
          }
        }
      } ~ path("page" / LongNumber / "photos") { (id) =>
        put {
          entity(as[Photo]) { photo =>
            onSuccess(pageWorker ? PostPhoto(photo)) {
              case result: String =>
                complete(result)
              case ex: FBException =>
                respondWithStatus(400) {
                  complete(ex)
                }
            }
          }
        } ~ get {
          onSuccess(pageWorker ? GetPhotos(id, NodeType.Page)) {
            case result: String =>
              complete(result)
            case ex: FBException =>
              respondWithStatus(400) {
                complete(ex)
              }
          }
        }
      } ~ path("page" / LongNumber / "albums") { (id) =>
        put {
          entity(as[Album]) { album =>
            onSuccess(userWorker ? CreateAlbum(album, NodeType.Page)) {
              case result: String =>
                complete(result)
              case ex: FBException =>
                respondWithStatus(400) {
                  complete(ex)
                }
            }
          }
        } ~ get {
          onSuccess(userWorker ? GetAlbums(id, NodeType.Page)) {
            case result: String =>
              complete(result)
            case ex: FBException =>
              respondWithStatus(400) {
                complete(ex)
              }
          }
        }
      }
    }
  }

}