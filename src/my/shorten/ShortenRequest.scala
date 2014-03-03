package my.shorten

import java.net.URL
import java.net.URLDecoder
import java.net.URLEncoder
import java.io.InputStreamReader
import java.io.ByteArrayInputStream
import java.io.BufferedReader
import java.nio.ByteBuffer;

object ShortenUrlCodec {
    private val chars = Array('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r',
      			 's','t','u','v','w','x','y','z','A','B','C','D','E','F','G','H','I','J','K',
      			 'L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','0','1','2','3',
      			 '4','5','6','7','8','9')
      			 
    def shortToId(shortUrl:String) : Int = {
    	var id = 0  
    	val len = shortUrl.length() - 1
    	var i = 0 
    	while (i < len){
    		val c = shortUrl.charAt(len - i)
    		var v = 0
    		if(c >= 'a' && c <= 'z')
    			v = c - 'a'
    		else if(c >= 'A' && c <= 'Z')
    			v = c - 'A' + 26
    		else  
    			v = c - '0' + 52
    	
    		val t = scala.math.pow(62, i) * v  
    		id = id + t.toInt
    		i += 1
    	}
    	
    	return magicId(id)
  	}
    
    def magicId(id: Int) = {
        ((id >> 16) << 16) | ((id & 0xF) << 8) | (id >> 8) & 0xF
    }
    
	def idToShort(id:Int) : String = {			 
      	var vId = magicId(id)
      	var cnt = 5
      	val values = Array('a','a','a','a','a','a')

      	while (vId > 0) {  
      		val remainder = vId % 62  
      		values(cnt) = chars(remainder)
      		vId = vId / 62
      		cnt -= 1
      	}  
		return new String(values)
  	}
}
class HttpRequest(buffer: Array[Byte]) {
  val reader = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(buffer)))
  
  var hostName:String = null
  var getContent: String = null

  var line:String = reader.readLine()
  
  while(line != null) {
      if (line.startsWith("GET ")) {
          val longUrlEnd = line.lastIndexOf(" HTTP/")
          getContent = line.substring(4, longUrlEnd).trim()
      }else if (line.startsWith("Host: ")) {
          hostName = line.substring(6).trim()
      }
      line = reader.readLine()
  }
  
  val method : Symbol = 
      getContent != null  && hostName != null match {
          case true =>
              if (getContent.indexOf("url?longUrl=") > 0) {
                  'Long
              }else if (getContent.indexOf("url?shortUrl=") > 0) {
                  'Short
              }else {
                  'None
              }
          case false =>
              'None
      }
  
  def shorturl_prefix() = 
		"http://" + hostName + "/"
    
  // GET /urlshortener/url?longUrl=http://www.ericsson.com/thecompany/company_facts/history HTTP/1.1    [long url]
  // GET /urlshortener/url?shortUrl=http://server-name/f9sA HTTP/1.1									[short url]
  val url : String = 
    method match {
      	case 'Long =>
      		val P = getContent.indexOf("?longUrl=") + "?longUrl=".length()
        	getContent.substring(P)
      	case 'Short =>
      		getContent.substring(getContent.length() - 6);
      	case _ =>
      	    null
    	}
  def toInt() : Int = {
      method match {
          case 'Short =>
          	  url == null match {
          	      case true =>
          	          -1
          	      case false =>
          	          ShortenUrlCodec.shortToId(url)
          	  }
          case 'Long =>
              	url.hashCode().abs
          case _ =>
              -1
      }
  }
}

class ShortenResponse(val methodName:Symbol, val longUrl: String, val shortUrl: String, val shorturl_prefix: String) {
    def toHttp() = {
        methodName match {
            case 'Long =>
                get_s2l(shortUrl, longUrl)
            case 'Short =>
                get_l2s(shortUrl, longUrl)
            case _ =>
                message_404()
        }
    }
    
    def message_1() =
	    "HTTP/1.1 200 Accepted\r\n" + 
		"Content-type: application/json;charset=UTF-8\r\n" +
		"Content-Length: ";
	
	def message_2_s2l() = 
		"\r\n\r\n" +
		"{" +
		"\"kind\": \"expand\"," +
		"\"shortUrl\": ";
	
	def message_3() =
		"," +
		"\"longUrl\": ";
	
	def message_4() = 
		"" +
		"}";
	
	def message_2_l2s() = 
		"\r\n\r\n" +
		"{" +
		"\"kind\": \"shorten\"," +
		"\"shortUrl\": ";
	
	def message_404() = 
		"HTTP/1.1 404 Not found\r\n" + 
		"Content-type: application/json;charset=UTF-8\r\n" +
		"Content-Length: 51\r\n\r\n" +
		"{" +
		"\"error\": true," +
		"\"code\": 404," +
		"\"messsage\": \"Not found\"" +
		"}";
	  
	def get_s2l(shortUrl : String, longUrl : String) = {
	  val body = message_2_s2l() + "\""+ shorturl_prefix + shortUrl + "\"" + message_3() ++ "\"" + URLDecoder.decode(longUrl) + "\"" + message_4()
	  message_1() + (body.length() - 4).toString + body
	}
	
	def get_l2s(shortUrl : String, longUrl : String) = {
	  val body = message_2_l2s() + "\"" + shorturl_prefix + shortUrl + "\"" + message_3() ++ "\"" + URLDecoder.decode(longUrl) + "\"" + message_4()  
	  message_1() + (body.length() - 4).toString + body
	}
}