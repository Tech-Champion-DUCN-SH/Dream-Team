package my.shorten

import java.nio.channels.{SelectableChannel, SelectionKey, Selector, ServerSocketChannel, SocketChannel};
import java.io.IOException
import java.util.LinkedList
import java.net.SocketAddress
import java.net.InetSocketAddress
import scala.collection.JavaConversions._
import java.nio.ByteBuffer
import concurrent.ops._

class NioOpChange(val context:NioContext, val tipe:Symbol, val ops:Int) {
	def this(context:NioContext, tipe: Symbol) { this(context, tipe, -1) }
}

class NioContext(val channel: SelectableChannel, val selector: RequestSelector) {
	var nioHandler:NioHandler = null
	var inBuffer:ByteBuffer = null
	var outBuffer:ByteBuffer = null
	var protocolHandler: ProtocolHandler = null
	
	def close() {
	    selector.close(this)
	}
}

class RequestSelector(private val selector: Selector) {
	private val registerRequests = new LinkedList[NioOpChange]
	
	def register(nioContext: NioContext, ops:Int) {
		registerRequests.synchronized {
			registerRequests.add(new NioOpChange(nioContext, 'REGISTER, ops))
		}

        selector.wakeup();
	}
	
	def changeOps(context: NioContext, ops:Int){
		registerRequests.synchronized {
			registerRequests.add(new NioOpChange(context, 'CHANGE, ops))
		}

        selector.wakeup();
		
    }
	def close(ctx:NioContext) {
	    registerRequests.synchronized {
			registerRequests.add(new NioOpChange(ctx, 'CLOSE))
		}
        selector.wakeup();
    }
	
	def closeContext(ctx:NioContext) {
	    val key = ctx.channel.keyFor(selector);
	    if (key != null) {
	    	key.cancel()
	    }
	    val socket = ctx.channel.asInstanceOf[SocketChannel].socket()
	    ctx.channel.isOpen() match {
	    case true =>
	    	try {
	    	    socket.shutdownInput()
	    	    socket.shutdownOutput()
	    	}catch{
	    		case e: IOException =>
	    	        //don't print
	    	}
	    	
	    case _ =>
	    }
	    try {
	        socket.close()
	    }catch{
	        case e: IOException =>
	            //don't print
	    }
	}
	
	def select() {
	    while(true) {
	        registerRequests.synchronized {
	          registerRequests.foreach((change) => {
	                val ctx:NioContext = change.context
	                try {
	                  change.tipe match {
	                  	case 'REGISTER =>
	                  		if (change.context.channel.isOpen()) {
	                  			change.context.channel.configureBlocking(false);
	                  			change.context.channel.register(selector, change.ops, change.context)
	                  		}else {
	                  		    closeContext(ctx)
	                  		}
	              		case 'CHANGE =>
	              		val key = ctx.channel.keyFor(selector);
	              			if (key != null && key.isValid()) {
	          					key.interestOps(change.ops)
	              			}else {
	              			   closeContext(ctx)
	              			}
	              		case 'CLOSE =>
	              		    closeContext(ctx)
	                  }
	                }catch{
	            		case e: Exception => 
	            		    e.printStackTrace()
	            	}
	          });
	            registerRequests.clear()
	        }
	        
	        selector.select()
	        
	        val selectedKeys = selector.selectedKeys
	        selectedKeys.foreach{ key =>
	            try {
	                if (key.isValid()) {
	                	val ctx:NioContext = key.attachment().asInstanceOf[NioContext]
		                if (!ctx.channel.isOpen()) {
		                    key.cancel();
		                }else {
		                    ctx.nioHandler.handleEvent(key)
		                }
	                }
	            }catch {
	                case e: Exception =>
	                    	e.printStackTrace();
	            }
	        }
	    }
	}
	
}

class Dispatcher {
    
	private val nThread = Config.nThread
  
	private val selectors = for( i <- 1 to nThread) yield {
		val selector = Selector.open()
		new RequestSelector(selector)
	}
  
	private var nextIndex = 0
  
	def nextSelector = {
			nextIndex = (nextIndex + 1) % nThread
			selectors(nextIndex)
		}
  
	def start() {
	    val port = new InetSocketAddress(Config.port);
		val channel = ServerSocketChannel.open()
		channel.configureBlocking(false)
		channel.socket().bind(port, Config.socketBacklog)
		val selector = selectors(0)
    
		val context = new NioContext(channel, selector)
		val handler = new AcceptNioHandler(this, context)
		context.nioHandler = handler
		selector.register(context, SelectionKey.OP_ACCEPT);
		
		val statistics = new Statistics
		statistics.addInterface(handler)
		
		ProtocolWorkerManager.workers.foreach(statistics.addInterface(_))
		
		spawn {statistics.start()}
		selectors.foreach(selector => spawn {selector.select()})
		
		
		
	}
}