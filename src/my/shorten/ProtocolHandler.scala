package my.shorten

import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey
import java.util.ArrayList
import java.util.HashMap

object ProtocolHandler {
    def create(context: NioContext) : ProtocolHandler 
    	= new SimpleProtocolHandler(context)
}

trait ProtocolHandler {
	def preHandleData(): Int;
	
	def handleInWorker(workContext: Object): Object;
	
	def handleError(error: Exception){
	    val outHttp = new ShortenResponse('Error, error.getMessage(), null, null).toHttp()
	    val outBuffer = ByteBuffer.allocate(outHttp.length())
		
	    outBuffer.put(outHttp.getBytes())
	    sendData(outBuffer)
	}
	
	def handleData() {
	    val code = preHandleData();
	    code > -1 match {
	        case true =>
	            ProtocolWorkerManager.getWorker(code).addHandler(this)
	        case false =>
	            val outHttp = new ShortenResponse('NotFound, null, null, null).toHttp()
	            val outBuffer = ByteBuffer.allocate(outHttp.length())
	    
	            outBuffer.put(outHttp.getBytes())
	            sendData(outBuffer)
	    }
	    
	}
	
	def context: NioContext
	
	protected def sendData(outBuffer: ByteBuffer) {
		context.outBuffer = outBuffer
		context.outBuffer.flip()
		context.selector.changeOps(context, SelectionKey.OP_WRITE)
	}
	def dataSent() {
		context.close()
	}
}

class SimpleProtocolHandler(val context: NioContext) extends ProtocolHandler {
	var request:HttpRequest = null
	context.protocolHandler = this
	
    def preHandleData() =  {
        val inBuffer = context.inBuffer
    	val size = inBuffer.limit()
        val buffer = new Array[Byte](size)
        inBuffer.get(buffer)
        request = new HttpRequest(buffer)
        request.toInt()
    }
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
    	
    	return id
  	}
	def idToShort(id:Int) : String = {
		val chars = Array('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r',
      			 's','t','u','v','w','x','y','z','A','B','C','D','E','F','G','H','I','J','K',
      			 'L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','0','1','2','3',
      			 '4','5','6','7','8','9')
      			 
      	var vId = id
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
	
	def handleInWorker(wc: Object): Object =  {
	    val (longMap, shortArray, accu, base) = wc.asInstanceOf[(HashMap[String, Int], ArrayList[String], Int, Int)]
	    val (response: ShortenResponse, newAccu: Int) = request.method match {
	        case 'Long =>
	        	longMap.containsKey(request.url) match {
	        	    case false =>
	        	        longMap.put(request.url, accu);
	        	        shortArray.add(request.url)
	        	        (new ShortenResponse(request.method, request.url, idToShort(accu * Config.nThread + base), request.shorturl_prefix()), accu + 1)
	        	    case true =>
	        	      	(new ShortenResponse(request.method, request.url, idToShort(longMap.get(request.url) * Config.nThread + base), request.shorturl_prefix()), accu)
	        	}
	        case 'Short =>
	        	val id = (shortToId(request.url) - base) / 4
	        	id > -1 && id < accu match {
	        	    case true =>
	        	        (new ShortenResponse(request.method, shortArray.get(id), request.url, request.shorturl_prefix()), accu)
	        	    case false =>
	        	        (new ShortenResponse('NotFound, null, null, null), accu)
	        	}
	        case _ =>
	            (new ShortenResponse('NotFound, null, null, null), accu, base)
	            
	    }
	    
	    val outHttp = response.toHttp()
	    
	    val outBuffer = ByteBuffer.allocate(outHttp.length())
	    
	    outBuffer.put(outHttp.getBytes())
	    sendData(outBuffer)
	    (longMap, shortArray, newAccu, base)
	}
}