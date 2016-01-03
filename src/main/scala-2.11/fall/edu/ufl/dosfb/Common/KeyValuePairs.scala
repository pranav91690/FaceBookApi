package fall.edu.ufl.dosfb.Common

import fall.edu.ufl.dosfb._
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.{ListBuffer}

/**
 * Created by SampathYadav on 12/1/2015.
 */
object KeyValuePairs {
  /*
   * Map data structures to store key-value pairs
   */
  var userMap: TrieMap[Long, Common.User] = new TrieMap[Long, Common.User]()
  var fbPostMap: TrieMap[Long, Common.post] = new TrieMap[Long, Common.post]()
  var pageMap: TrieMap[Long, Common.Page] = new TrieMap[Long, Common.Page]()
  var userAlbumMap: TrieMap[Long, Common.Album] = new TrieMap[Long,Common.Album]()
  var userPhotoMap: TrieMap[Long, Common.Photo] = new TrieMap[Long, Common.Photo]()
  var pagePostMap: TrieMap[Long, Common.post] = new TrieMap[Long, Common.post]()
  var pageAlbumMap: TrieMap[Long, Common.Album] = new TrieMap[Long,Common.Album]()
  var pagePhotoMap: TrieMap[Long, Common.Photo] = new TrieMap[Long, Common.Photo]()
  var friendsMap: TrieMap[Long, Common.User] = new TrieMap[Long,Common.User]()

  //USER KEY VALUE PAIRS
  //Get User
  def getUser(userID: Long): Option[Common.User] = {
    userMap.get(userID)
  }
  //Put User
  def putUser(user: Common.User): Boolean = {
    userMap.put(user.id, user)
    true
  }
  //Update User
  def updateUser(user: Common.User): Boolean = {
    userMap.update(user.id, user)
    true
  }

  //USER POSTS KEY VALUE PAIRS
  //Get Posts
  def getPosts(userId: Long): ListBuffer[Option[Common.post]] = {
    val posts : ListBuffer[Option[Common.post]] = null
    //Get all the post ids of this user and send these ids to Posts to get the actual post data of this User
    userMap.get(userId) match {
      case Some(s) => for(a <- 1 to s.posts.size){
        posts += (fbPostMap.get(s.posts(a)))
      }
    }
    posts
  }
  //Put Posts
  def putPosts(storePost: post): Boolean = {
    //Add the post data to Posts
    fbPostMap.put(storePost.id, storePost)
    //Add the post id to that user's posts
    var ownerID: Long = 0L
    if (storePost.edgeInfo.map(x => x.owner) != None) {
      ownerID = storePost.edgeInfo.map(x => x.owner).get

    }
    userMap.get(ownerID) match {
      case Some(s) => s.posts+=storePost.id
    }
    true
  }

  //USER ALBUM KEY VALUE PAIRS
  //Get Album
  def getAlbum(userID: Long): ListBuffer[Option[Common.Album]] = {
    val albums : ListBuffer[Option[Common.Album]] = null
    //Get all the album ids of this user and send these ids to Albums to get the actual album data of this User
    userMap.get(userID) match {
      case Some(s) => for(a <- 1 to s.albums.size){
        albums += (userAlbumMap.get(s.albums(a)))
      }
    }
    albums
  }
  //Put Album
  def putAlbum(album: Common.Album): Boolean = {
    //Add the album data to the Albums
    userAlbumMap.put(album.id,album)
    //Add the album id to that User's albums
    userMap.get(album.edgeInfo.map(x=>x.owner).get) match {
      case Some(s) => s.albums+=album.id
    }
    true
  }

  //USER PHOTO KEY VALUE PAIRS
  //Get Photo
  def getPhoto(userID: Long): ListBuffer[Option[Common.Photo]] = {
    val photos : ListBuffer[Option[Common.Photo]] = null
    //Get all the photo ids of this user and send these ids to Photos to get the actual photo data of this User
    userMap.get(userID) match {
      case Some(s) => for(a <- 1 to s.photos.size){
        photos += (userPhotoMap.get(s.photos(a)))
      }
    }
    photos
  }
  //Put Photo
  def putPhoto(photo: Common.Photo): Boolean = {
    //Add the photo data to the Photos
    userPhotoMap.put(photo.id, photo)
    //Add the photo id to that User's photos
    userMap.get(photo.edgeInfo.map(x=>x.owner).get) match {
      case Some(s) => s.photos+=photo.id
    }
    true
  }

  //FRIENDS KEY VALUE PAIRS
  //Get Friends
  def getFriend(userID: Long)  {
    friendsMap.get(userID)
  }

  //Put/Add Friends
  def CreateFriendList(userID:Long, friendUserId:Long): Boolean= {
    userMap.get(userID) match {
      case Some(s) => s.friendsList+=friendUserId
    }
    true
  }

  //PAGE KEY VALUE PAIRS
  //Get PAGE
  def getPage(pageID: Long): Option[Page] = {
    pageMap.get(pageID)
  }
  //Put PAGE
  def putPage(page: Common.Page): Boolean = {
    pageMap.put(page.id, page)
    true
  }
  //Update Page
  def updatePage(page: Common.Page): Boolean = {
    pageMap.update(page.id, page)
    true
  }

  //PAGE POSTS KEY VALUE PAIRS
  //Get Posts
  def getPagePosts(pageID: Long): ListBuffer[Option[Common.post]] = {
    val posts : ListBuffer[Option[Common.post]] = null
    //Get all the post ids of this page and send these ids to Posts to get the actual post data of this Page
    pageMap.get(pageID) match {
      case Some(s) => for(a <- 1 to s.posts.size){
        posts += (pagePostMap.get(s.posts(a)))
      }
    }
    posts
  }
  //Put Posts
  def putPagePosts(storePost: post): Boolean = {
    //Add the post data to Posts
    pagePostMap.put(storePost.id,storePost)
    //Add the post id to that page's posts
    pageMap.get(storePost.edgeInfo.map(x=>x.owner).get) match {
      case Some(s) => s.posts+=storePost.id
    }
    true
  }

  //PAGE ALBUM KEY VALUE PAIRS
  //Get Album
  def getPageAlbum(pageID: Long): ListBuffer[Option[Common.Album]] = {
    val albums : ListBuffer[Option[Common.Album]] = null
    //Get all the album ids of this page and send these ids to Albums to get the actual album data of this Page
    pageMap.get(pageID) match {
      case Some(s) => for(a <- 1 to s.albums.size){
        albums += (pageAlbumMap.get(s.albums(a)))
      }
    }
    albums
  }
  //Put Album
  def putPageAlbum(album: Common.Album): Boolean = {
    //Add the album data to the Albums
    pageAlbumMap.put(album.id,album)
    //Add the album id to that Page's albums
    pageMap.get(album.edgeInfo.map(x=>x.owner).get) match {
      case Some(s) => s.albums+=album.id
    }
    true
  }

  //PHOTO KEY VALUE PAIRS
  //Get Photo
  def getPagePhoto(pageID: Long): ListBuffer[Option[Common.Photo]] = {
    val photos : ListBuffer[Option[Common.Photo]] = null
    //Get all the photo ids of this page and send these ids to Photos to get the actual photo data of this Page
    pageMap.get(pageID) match {
      case Some(s) => for(a <- 1 to s.photos.size){
        photos += (pagePhotoMap.get(s.photos(a)))
      }
    }
    photos
  }
  //Put Photo
  def putPagePhoto(photo: Common.Photo): Boolean = {
    //Add the photo data to the Photos
    userPhotoMap.put(photo.id, photo)
    //Add the photo id to that Page's photos
    pageMap.get(photo.edgeInfo.map(x=>x.owner).get) match {
      case Some(s) => s.photos+=photo.id
    }
    true
  }
}
