package my.shorten

import java.util.concurrent.LinkedBlockingQueue
import java.util.ArrayList
import java.util.HashMap
import scala.collection.JavaConversions._
import concurrent.ops._

import scala.collection.mutable._

class ProtocolWorker(private var workContext: Object) extends IStatistics{
    
    def statInfo: StatisticsInfo = {
        val (_, _, acc, _) =  workContext.asInstanceOf[(Map[String, Int], ArrayBuffer[String], Int, Int)]
        
        new StatisticsInfo(this, 'Record, acc)
    }
	val workQueue = new LinkedBlockingQueue[ProtocolHandler];
	def start() {
		while(true) {
			val protocolHandler = workQueue.take()
			val all = new ArrayList[ProtocolHandler]
			all.add(protocolHandler)
			workQueue.drainTo(all)
			all.foreach( work => try { 
			    workContext = work.handleInWorker(workContext)}
			catch {
			    case e: Exception => 
			        e.printStackTrace()
			        work.handleError(e)
			    }
			)
		}
	}
	
	def addHandler(protocolHandler:ProtocolHandler) {
	    workQueue.offer(protocolHandler)
	}
}

object ProtocolWorkerManager {
    val manager : ProtocolWorkerManager = new ProtocolWorkerManager
    def getWorker(n: Int) = manager.getWorker(n)
    
    def workers = manager.allWorkers
}

class ProtocolWorkerManager() {
	private val nThread = Config.nThread
  
	private val workers = 
	    for( i <- 0 until nThread) yield {
	    	new ProtocolWorker((new HashMap[String, String](Config.capacity), new ArrayList[String], 0, i))
	    }
	    
	workers.foreach(w => 
	    {
	        spawn {w.start()}
	    })
	def allWorkers = workers
	def getWorker(n: Int) = workers(n % nThread)
  
  
}