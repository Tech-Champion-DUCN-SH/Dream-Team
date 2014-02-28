package my.shorten

import java.nio.channels._
import java.nio.ByteBuffer

trait NioHandler {
	def handleEvent(key: SelectionKey)
	
	def selector: RequestSelector

}

class AcceptNioHandler(private val dispatcher: Dispatcher, private val context: NioContext) extends NioHandler with IStatistics{

	private val serverSocketChannel = context.channel.asInstanceOf[ServerSocketChannel]
	
	var totalAccept = 0
	
	private val acceptNum = Config.nThread * Config.pendingQueue
	
	def statInfo: StatisticsInfo = {
	    new StatisticsInfo(this, 'Transaction, totalAccept)
	}
	def handleEvent(key: SelectionKey) {
	    var num = acceptNum;
	    while(num > 0) {
	        val ch = serverSocketChannel.accept()
	        ch match {
	            case null => 
	                num = 0
	            case _ =>
	                totalAccept += 1
	                val newCtx = new NioContext(ch, dispatcher.nextSelector)
	                newCtx.nioHandler = new ReadWriteNioHandler(newCtx)
	                newCtx.selector.register(newCtx, SelectionKey.OP_READ)
	                num = num - 1
	        }
	    }
	    
	}	
	
	def selector = context.selector
}

class ReadWriteNioHandler(val context: NioContext) extends NioHandler {
	private def buff_size = 4096
	
	def handleEvent(key: SelectionKey) {
		key match {
			case key if key.isReadable() =>
				handleRead()
			case key if key.isValid() && key.isWritable() =>
				handleWrite()
		}
	}
	
	def handleRead() {
		val inBuff = ByteBuffer.allocate(buff_size)

		if (context.inBuffer != null) {
			inBuff.put(context.inBuffer)
		}

        val bytesRead = context.channel.asInstanceOf[SocketChannel].read(inBuff);
        if (bytesRead == 0) {
            return;
        }

        if (bytesRead == -1) { // end of stream
        	context.close()
            return;
        }
        
        inBuff.flip()
        context.inBuffer = inBuff
        
        if (context.protocolHandler != null) {
            context.protocolHandler.handleData()
        }else {
            ProtocolHandler.create(context).handleData()
        }
	}
	
	private def handleWrite() {
        val socketChannel = context.channel.asInstanceOf[SocketChannel]
        val ret = socketChannel.write(context.outBuffer)
        if (!context.outBuffer.hasRemaining()) {
        	selector.changeOps(context, SelectionKey.OP_READ)
        	context.protocolHandler.dataSent()
        }
    }
	
	def selector = context.selector

}