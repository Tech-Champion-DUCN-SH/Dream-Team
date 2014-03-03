package my.shorten

import java.util.Properties
import scala.collection.JavaConverters._
import java.io.FileReader

object Config {
    val x = new Properties
    
    try {
        val file = new FileReader("config.properties")
        x.load(file)
    }catch{
        case e : Exception =>
    }
    
    val props = x.asScala

	def port = props.contains("port") match {
        case true => props("port").toInt
        case false => 8848
    }
		
		
	def nThread = props.contains("thread_num") match {
	    case true =>
	        props("thread_num").toInt
	    case false =>
	        Runtime.getRuntime().availableProcessors()
	}
	
	val statTime = props.contains("statistics_timer") match {
	    case true =>
	        props("statistics_timer").toInt
	    case false =>
	        5 * 1000
	} 
	    
	
	val statFileName = props.contains("statistics_file") match {
	    case true =>
	        props("statistics_file")
	    case false =>
	        "statistics.txt"
	} 
	    
	    
	val capacity = props.contains("capacity") match {
	    case true =>
	        (props("capacity").toInt / (1.75)).toInt
	    case false =>
	        5000000
	}
	
	val pendingQueue = props.contains("pending_queue") match {
	    case true =>
	        props("pending_queue").toInt
	    case false =>
	        10
	}
	
	val socketBacklog = props.contains("socket_backlog") match {
	    case true =>
	         props("socket_backlog").toInt
	    case false =>
	        10
	}
	
	val dbgTrace = props.contains("debug") match {
	    case true =>
	         props("debug").toBoolean
	    case false =>
	        true
	}
	
	override def toString = {
	    "port = " + port  + 
	    ", nThread = " + nThread + 
	    ", statTime = " + statTime + 
	    ", statFileName = " + statFileName + 
	    ", capacity = " + capacity + 
	    ", socketBacklog = " + socketBacklog
	}
	
	def dbg(s: Object) {
	     dbgTrace match {
	         case true =>
	            println(s)
	         case false =>
	             
	     }
	        
	}
	
	println(this)
}