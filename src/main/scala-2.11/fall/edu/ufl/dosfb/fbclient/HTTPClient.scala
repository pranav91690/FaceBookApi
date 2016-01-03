package fall.edu.ufl.dosfb.fbclient

/**
 * Created by pranav on 11/30/15.
 */

import java.security.spec.X509EncodedKeySpec
import java.util.concurrent.TimeUnit
import akka.actor.{ActorRef, Props, Actor, ActorSystem}
import akka.event.Logging
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.ConfigException.Null
import fall.edu.ufl.dosfb.fbserver.HTTPServer.{posts, userPosts}
import shapeless.~>
import spray.can.Http
import spray.client.pipelining._
import spray.http.{DateTime, HttpResponse}
import spray.httpx.SprayJsonSupport
import spray.util._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Random, Failure, Success}
import fall.edu.ufl.dosfb.Common._
import spray.json.{JsString, DefaultJsonProtocol}

import java.security._
import java.util.Base64
import javax.crypto.{SecretKey, KeyGenerator, Cipher}
import javax.crypto.spec.{SecretKeySpec, IvParameterSpec}

object HTTPClient extends App{
  println("Client Started")

  // Case Classes - To get the User Going
  case class Start()
  case class GetProfile()
  case class GetPosts()
  case class GetPhotos()
  case class GetAlbums()
  case class GetFriends()
  case class AddFriend(id : Long)
  case class InitUserFriends()
  case class CreatePost()
  case class CreatePostWithPhoto()
  case class PostPhoto()
  case class CreateAlbum()
  case class CreateAPage()
  case class StartSim()

  // Statistics Messages
  case class PutPostSuccess()
  case class PutPostFailure()
  case class GetPostSuccess()
  case class GetPostFailure()

  case class PutUserSuccess()
  case class PutUserFailure()
  case class GetUserSuccess()
  case class GetUserFailure()

  case class PutPageSuccess()
  case class PutPageFailure()
  case class GetPageSuccess()
  case class GetPageFailure()

  case class finished()


  // Prefixes
  val urlPrefix = "http://localhost:8080"
  val userPrefix = urlPrefix + "/user"
  val pagePrefix = urlPrefix + "/page"
  val postsSuffix = "/posts"
  val photosSuffix = "/photos"
  val friendsSuffix = "/friends"
  val albumsSuffix = "/albums"
  // List of Total Page IDs/ User IDs -- Update this Whenever you create a Page ID/U ser ID
  val userIds = new ListBuffer[Long]
  val pageIds = new ListBuffer[Long]
  val userRef = new ListBuffer[ActorRef]
  // Store all the Users id and keyPairs
  val keys = new mutable.HashMap[Long,KeyPair]()

  var putPostSuccess = 0
  var putPostFailure = 0
  var getPostSuccess = 0
  var getPostFailure = 0

  var success = 0

  // we need an ActorSystem to host our application in
  val system = ActorSystem("facebook-simple-spray-client")

  // execution context for futures below
  val log = Logging(system, getClass)


  // Create a Sim Actor
  val coordinator = system.actorOf(Props[fakeClientManager], name = "clientManager")

  coordinator ! Start


  private def isUser :Boolean = {
    if (Random.nextInt(2) == 0){
      true
    }else{
      false
    }
  }

  private def onSelf : Boolean = {
    if (Random.nextInt(2) == 0){
      true
    }else{
      false
    }
  }

  private def restUrlBuilder(prefix:String, nodeId:Long, suffix:Option[String]): String = {
    val suffixVal = suffix match {
      case Some(suffixStr) => suffixStr
      case None => ""
    }
    prefix + "/" + nodeId + suffixVal.trim
  }

  class fakeClientManager extends Actor with SprayJsonSupport {
    import spray.client.pipelining._
    import spray.http._
    import fall.edu.ufl.dosfb.Common.DefaultJsonFormatterProtocol._
    implicit val ec = context.dispatcher
    implicit val timeout = Timeout(120, TimeUnit.SECONDS)

    // Defined Below
    val userPipeline: HttpRequest => Future[User] = sendReceive ~> unmarshal[User]
    val pagePipeline: HttpRequest => Future[Page] = sendReceive ~> unmarshal[Page]

    override def receive = {
      // Create the Fake Users
      case Start => {
        println("Creating Users")
        for(i <- 0 until 5){
          // User Attributes
          val first_name = faker.Name.first_name
          val last_name = faker.Name.last_name
          val name =  first_name + " " + last_name
          val randdate = (1970+ Random.nextInt(35)) + "-" + (Random.nextInt(12)+1).formatted("%02d") +"-" +
            (Random.nextInt(28)+1).formatted("%02d") + "T01:01:01"
          val birthday = DateTime.fromIsoDateTimeString(randdate)
          val gender = Random.nextInt(2) match {
            case 0 => "male"
            case 1 => "female"
          }

          val friends : ListBuffer[Long]    = new ListBuffer[Long]()
          val photos  : ListBuffer[Long]    = new ListBuffer[Long]()
          val posts   : ListBuffer[Long]    = new ListBuffer[Long]()
          val albums  : ListBuffer[Long]    = new ListBuffer[Long]()

          val user = User(-1,first_name,last_name, Some(faker.Internet.free_email(name)),birthday,
          Some(gender), None, None, friends, photos, posts, albums)

          // Send a Request and Wait for the Future
          val response = userPipeline(Put(userPrefix,user))
            response onComplete {
              case Success(result : User) =>
                // Add the result to list
                userIds += result.id
                println("User Id Created --> " + result.id)

                // Generate the Public Key, Private Key and store in the User
                val keyPair = generateRSAKeyPair()
                val kv = result.id -> keyPair
                keys += kv

                // Create the Fake User
                val ref = system.actorOf(Props(new fakeUser(result, keyPair)),result.id.toString)
                userRef += ref

              case Success(somethingUnexpected) =>
                self ! PutUserFailure
              case Failure(error) =>
                self ! PutUserFailure
            }
          Thread.sleep(1000)
        }

        println("Finished Creating Users")


        // Wait For Some Time Before Creating Page
        Thread.sleep(3000)

        println("Creating Pages")
        // Create Some Pages
        for(i <- 0 until 10){
          val title = faker.Lorem.sentence()
          val description = faker.Lorem.paragraph()
          val website = "http://www." + faker.Internet.domain_name
          val photos  : ListBuffer[Long]    = new ListBuffer[Long]()
          val posts   : ListBuffer[Long]    = new ListBuffer[Long]()
          val albums  : ListBuffer[Long]    = new ListBuffer[Long]()
          val page = Page(-1, Some(title), Some(description), Some(website),None,posts,photos,albums)

          // Send a Request and Wait for the Future
          val response = pagePipeline(Put(pagePrefix,page))
          response onComplete {
            case Success(result : Page) =>
              // Add the ID to the List
              pageIds += result.id
              self ! PutPageSuccess
            case Success(somethingUnexpected) =>
              self ! PutPageFailure
            case Failure(error) =>
              self ! PutPageFailure
          }
        }
        println("Finished Creating Pages")

        Thread.sleep(3000)

        println("Started Initializing Friend Lists for all Users")
        // Initiate the Friends List For Each User
        for(ref <- userRef){
          ref ! InitUserFriends
          // Wait for Some Time Before Sending Next Request
          Thread.sleep(2000)
        }

        println("Finished Initializing Friend Lists")


        println(userRef)

        // Wait for Some Time
//        Thread.sleep(10000)
        Thread.sleep(3000)

        println("BootStrap Success")

        // Start Simulation
        self ! StartSim
      }

      case PutPostSuccess   => {
        putPostSuccess += 1
      }

      case PutPostFailure   => {
        putPostFailure += 1
      }

      case GetPostSuccess   => {
        getPostSuccess += 1
        success += 1
//        if(success == 5) {
//          coordinator ! finished
//        }
      }
      case GetPostFailure   => {
        getPostFailure += 1
        success += 1
//        if(success == 5) {
//          coordinator ! finished
//        }
      }

      case StartSim => {
        // Start the Simulation
        // Create Post
        for(ref <- userRef) {
          for (i <- 0 until 5){
            ref ! CreatePost
            Thread.sleep(200)
          }
        }
//        for(i <- 0 until 5){
//          userRef(Random.nextInt(userRef.size)) ! CreatePost
//          Thread.sleep(1000)
//        }


        Thread.sleep(20000)


        for(ref <- userRef){
          ref ! GetPosts
          Thread.sleep(1000)
        }
//        // Retrive Posts
//        for(i <- 0 until 5){
//          userRef(Random.nextInt(userRef.size)) ! GetPosts
//          Thread.sleep(1000)
//        }
      }

//      case finished => {
//        println("End of Simulation")
//        system.terminate()
//      }
    }
  }

  class fakeUser(User : User, keyPair: KeyPair) extends Actor with SprayJsonSupport {
    import spray.client.pipelining._
    import spray.http._
    import fall.edu.ufl.dosfb.Common.DefaultJsonFormatterProtocol._
    implicit val ec = context.dispatcher
    implicit val timeout = Timeout(120, TimeUnit.SECONDS)

    val successPipeLine: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]

    val postsPipeLine: HttpRequest => Future[posts] = sendReceive ~> unmarshal[posts]

    var friendIds = User.friendsList

    def receive = {
      // PUT --------> Requests
      case CreatePost =>
        // Security Part -- Here
        // Generate the AES key
        val Aeskey = generateAESKey()

        // Generate the IV
        val IV = genIV()

        // Use this key to Encrypt all Messages
        val message = faker.Lorem.sentence()
        val eMessage = encrypt(message.toString, Aeskey, IV)

        val description = faker.Lorem.sentence()
        val eDesc = encrypt(description.toString, Aeskey, IV)

        val link = "http://www." + faker.Internet.domain_name + "/" + Random.alphanumeric.take(10).mkString
        val eLink = encrypt(link.toString, Aeskey, IV)

        // Determine the Node Type
        var edge : Edge = null

        edge = Edge(User.id, NodeType.User, User.id)

        // Create the Post Object
        val post = fall.edu.ufl.dosfb.Common.post(-1,eMessage,None,Some(eDesc),Some(eLink),Some(edge))

        // Give Access to the Respective Users
        val enKeys = new ListBuffer[ekeys]
        userIds.foreach(x => {
          // Generate the Encypted Key
          val kv = ekeys(x, encryptSharedKey(Aeskey,keys(x).getPublic))
          enKeys += kv
        })

        // The Encypted Post
        val ePost = encryptedPost(post,enKeys,base64Encode(IV))

        // Authenticate
        val signature = sign(ePost.toString,keyPair.getPrivate)

        // Print the Encypted Message
        println("Encrypted Message")
        println(ePost)

        // Get the User's to whom to share the Post

        // STATS Purpose
        // Send a Request and Wait for the Future
        val response = successPipeLine(Put(restUrlBuilder(userPrefix,User.id,Some(postsSuffix)),ePost))
        response onComplete {
          case Success(result : String) =>
            println("Post Created")
            coordinator ! PutPostSuccess

          case Success(somethingUnexpected) =>
            coordinator ! PutPostFailure

          case Failure(error) =>
            coordinator ! PutPostFailure
        }

      case CreatePostWithPhoto =>
        val message = faker.Lorem.sentence()
        val description = faker.Lorem.sentence()
        val link = "http://www." + faker.Internet.domain_name + "/" + Random.alphanumeric.take(10).mkString
        val photoLink = "http://www." + faker.Internet.domain_name + "/" + Random.alphanumeric.take(10).mkString + ".jpg"
        // Create a Photo Object
        val photo = Photo(-1,640,480,photoLink, None)

        // Determine the Node Type
        var edge : Edge = null
        if(isUser){
          edge = Edge(User.id, NodeType.User, User.id)
        }else{
          edge = Edge(pageIds(Random.nextInt(pageIds.size)),NodeType.Page,User.id)
        }

        // Create the Post Object
        val post = fall.edu.ufl.dosfb.Common.post(-1,message,Some(photo),Some(description),Some(link),Some(edge))

        // Send a Request and Wait for the Future
        successPipeLine(Put(restUrlBuilder(userPrefix,User.id,Some(postsSuffix)),post))


      case PostPhoto =>
        val photoLink = "http://www." + faker.Internet.domain_name + "/" + Random.alphanumeric.take(10).mkString + ".jpg"
        val photo = Photo(-1,1920,1080,photoLink, None)

        // Determine the Node Type
        var edge : Edge = null
        if(isUser){
          edge = Edge(User.id,NodeType.User,User.id)
        }else{
          edge = Edge(pageIds(Random.nextInt(pageIds.size)),NodeType.Page,User.id)
        }

        // Send a Request and Wait for the Future
        successPipeLine(Put(restUrlBuilder(userPrefix,User.id,Some(photosSuffix)),photo))

      case CreateAlbum =>
        val numPhotos = 5 + Random.nextInt(20)
        val photos = new ListBuffer[Photo]()
        for(i <-0 to numPhotos){
          val photolink = "http://www." + faker.Internet.domain_name + "/" + Random.alphanumeric.take(10).mkString + ".jpg"
          photos += Photo(-1,1920,1080,photolink, None)
        }
        val album = Album(-1, None, Some(faker.Lorem.sentence()),None, photos.toList, None)

        // Determine the Node Type
        var edge : Edge = null
        if(isUser){
          edge = Edge(User.id,NodeType.User,User.id)
        }else{
          edge = Edge(pageIds(Random.nextInt(pageIds.size)),NodeType.Page,User.id)
        }

        // Send a Request and Wait for the Future
        successPipeLine(Put(restUrlBuilder(userPrefix,User.id,Some(albumsSuffix)),album))

        // ---> Get Post
      case GetPosts =>
        // Pick a Random ID From the FriendList
        var id : Long = 0
//        if(isUser) {
//          id = User.id
//        }else{
//          id = pageIds(Random.nextInt(pageIds.size))
//        }

        id = User.id

        // Send a Request and Wait for the Future
//        val response = successPipeLine(Get(restUrlBuilder(userPrefix,id,Some(postsSuffix))))
        val response = postsPipeLine(Get(restUrlBuilder(userPrefix,id,Some(postsSuffix))))
        response onComplete {
          case Success(result : posts) =>
            println("Posts After Decryption")
            // Extract the Encrypted Posts and print them

            result.posts.foreach(x => {
              // Get the Private Kye o the user
              x.accessList.foreach(y => {
                // Check if y is the User
                if(y.id == User.id){
                  // Extract the Private key
                  val privateKey = keyPair.getPrivate
                  // Get Secret Key
                  val secretKey = decryptSharedKey(base64Decode(y.key), privateKey)
                  // Decrypt Info Now
                  val IV = x.IV

                  // Decrypt the Post nd Print the Details
                  println("Encrypted Message --> " + x.post.message)
                  println("Decrypted Message --> " + decrypt(x.post.message, secretKey, base64Decode(IV)))

                  println("Encrypted Message --> " + x.post.description)
                  println("Decrypted Message --> " + decrypt(x.post.description.get, secretKey, base64Decode(IV)))

                  println("Encrypted Message --> " + x.post.link)
                  println("Decrypted Message --> " + decrypt(x.post.link.get, secretKey, base64Decode(IV)))
                }
              })
            })


            coordinator ! GetPostSuccess

          case Success(somethingUnexpected) =>
            coordinator ! GetPostFailure

          case Failure(error) =>
            coordinator ! GetPostFailure
        }

      case GetPhotos =>
        // Pick a Random ID From the FriendList
        var id : Long = 0
        if(isUser) {
          id = User.id
        }else{
          id = pageIds(Random.nextInt(pageIds.size))
        }

        // Send a Request and Wait for the Future
        successPipeLine(Get(restUrlBuilder(userPrefix,id,Some(photosSuffix))))

      case GetAlbums =>
        // Pick a Random ID From the FriendList
        var id : Long = 0
        if(isUser) {
//          id = friendIds(Random.nextInt(friendIds.size)).toLong
          id = User.id
        }else{
          id = pageIds(Random.nextInt(pageIds.size))
        }

        // Send a Request and Wait for the Future
        successPipeLine(Get(restUrlBuilder(userPrefix,id,Some(albumsSuffix))))

        // Message to Self to Add a Friend
      case AddFriend(id : Long) =>
        // Get the Posts for this ID
        // Determine the Return Type
        val friend = ObjectId(id)
        // Add to Friend IDS
        friendIds += id
        // Send a Request and Wait for the Future
        successPipeLine(Put(restUrlBuilder(userPrefix,User.id,Some(friendsSuffix)),friend))

      case GetFriends() =>
        // Get all the Friends for this User ID
        // Send a Request and Wait for the Future
        successPipeLine(Get(restUrlBuilder(userPrefix,User.id,Some(friendsSuffix))))

        // Add a Set of Friends to Each User
      case InitUserFriends => {
        // Randomly PickUp 5 - 20 Friends
        val friends = Random.nextInt(4) + 1
        for(i <- 0 until friends){
          val randomIndex = Random.nextInt(userIds.size)
          val friendId = userIds(randomIndex)
          self ! AddFriend(friendId)
        }
      }
    }
  }

  // Security Related Functions
  // Function to Re Build the AES key using the private key of the user
  def buildAESKey(encodedKey:String):SecretKey = new SecretKeySpec(base64Decode(encodedKey),"AES")

  // -----> May be these are for the Digital Signature <-------
  // What is this the purpose of this function
  def buildRSAPublicKey(encodedKey:String):PublicKey = {
    val keySpec = new X509EncodedKeySpec(base64Decode(encodedKey))
    val keyFactory = KeyFactory.getInstance("RSA")
    val pubKey = keyFactory.generatePublic(keySpec)
    pubKey
  }

  // What is this RSA Key Pair???
  def generateRSAKeyPair():KeyPair ={
    val generator = KeyPairGenerator.getInstance("RSA")
    generator.initialize(1024)
    generator.genKeyPair()
  }

  // Convert Bytes to String to Encrypt
  def base64Encode(bytes:Array[Byte]) = {
    Base64.getEncoder.encodeToString(bytes)
  }

  // Convert String into Bytes to Decrypt
  def base64Decode(encodedStr: String): Array[Byte] = {
    Base64.getDecoder.decode(encodedStr)
  }

  // Encrypt the AES key with the public key of the user you want share the item with
  def encryptSharedKey(secretKey:SecretKey, publicKey: PublicKey):String = {
    val cipher = Cipher.getInstance("RSA/ECB/PKCS1Padding")
    cipher.init(Cipher.ENCRYPT_MODE, publicKey)
    base64Encode(cipher.doFinal(secretKey.getEncoded))
  }

  // Decrypt and extract the AES key with the user's private key
  def decryptSharedKey(encryptedKey:Array[Byte], privateKey:PrivateKey):SecretKey = {
    val cipher = Cipher.getInstance("RSA/ECB/PKCS1Padding")
    cipher.init(Cipher.DECRYPT_MODE, privateKey)
    val decBytes = cipher.doFinal(encryptedKey)
    new SecretKeySpec(decBytes,"AES")
  }

  // Generate the random AES key to be generated for each item by the user
  def generateAESKey():SecretKey ={
    val generator = KeyGenerator.getInstance("AES")
    generator.init(128)
    generator.generateKey
  }

  // Function Module to Encypt the message using the AES Key -- Returns a String
  def encrypt(data:String, secretKey:SecretKey, initVector:Array[Byte]):String = {
    val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    cipher.init(Cipher.ENCRYPT_MODE, secretKey, new IvParameterSpec(initVector))
    base64Encode(cipher.doFinal(data.getBytes("UTF-8")))
  }

  // Function Module to Decrypt the message using the AES key - Returns a String
  def decrypt(data:String, secretKey:SecretKey, initVector:Array[Byte]):String = {
    val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    cipher.init(Cipher.DECRYPT_MODE, secretKey, new IvParameterSpec(initVector))
    new String(cipher.doFinal(base64Decode(data)))
  }

  // Generate the Random Initialization vector
  def genIV():Array[Byte] ={
    val sRNG = SecureRandom.getInstance("SHA1PRNG")
    sRNG.setSeed(sRNG.generateSeed(32))
    val initV = new Array[Byte](16)
    sRNG.nextBytes(initV)
    initV
  }


  // Authentication Code
  def sign(data:String, privateKey: PrivateKey):String = {
    val singer =  Signature.getInstance("SHA256withRSA")
    singer.initSign(privateKey)
    singer.update(data.getBytes("UTF-8"))
    base64Encode(singer.sign())
  }
  }
