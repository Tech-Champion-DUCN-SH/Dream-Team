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
	
	def handleInWorker(wc: Object): Object =  {
	    val (longMap, shortArray, accu, base) = wc.asInstanceOf[(HashMap[String, Int], ArrayList[String], Int, Int)]
	    val (response: ShortenResponse, newAccu: Int) = request.method match {
	        case 'Long =>
	        	longMap.containsKey(request.url) match {
	        	    case false =>
	        	        longMap.put(request.url, accu);
	        	        shortArray.add(request.url)
	        	        (new ShortenResponse(request.method, request.url, ShortenUrlCodec.idToShort(accu * Config.nThread + base), request.shorturl_prefix()), accu + 1)
	        	    case true =>
	        	      	(new ShortenResponse(request.method, request.url, ShortenUrlCodec.idToShort(longMap.get(request.url) * Config.nThread + base), request.shorturl_prefix()), accu)
	        	}
	        case 'Short =>
	        	val id = (ShortenUrlCodec.shortToId(request.url) - base) / Config.nThread
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