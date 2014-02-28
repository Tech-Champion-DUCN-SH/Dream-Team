package my.shorten

import java.io._
import java.text.NumberFormat
import java.util.concurrent._
import java.util.ArrayList
import scala.collection.JavaConversions._
import scala.collection.mutable._
import java.lang.management.ManagementFactory

trait IStatistics {
    def statInfo: StatisticsInfo
}
class StatisticsInfo(val key: Object, val tipe: Symbol, val value: Int)
class Statistics {
    
    private val scheduler = Executors.newScheduledThreadPool(1);
	private val interfaces = new ArrayBuffer[IStatistics]
	
	def addInterface(stat: IStatistics) {
	    interfaces.append(stat)
	    
	}
	
	private val statRunner = new Runnable {
	    def run() {
	        stat()
	    }
	}
	
	def start() {
	    before()
	    writeToFile("Total transactions handled\t\tTotal records stored\t\tCPU load\t\tMemory occupation\n" 
	            	+ "-----------------------------------------------------------------------------------\n")
	    scheduler.scheduleAtFixedRate(statRunner, Config.statTime, Config.statTime, TimeUnit.MILLISECONDS);
	}
	
	def stat() {
	    var totalTransaction: Int = 0;
	    var totalRecord: Int = 0;
	    
	    interfaces.foreach(stat1 => {
	        val statInfo = stat1.statInfo
	        statInfo.tipe match {
	            case 'Record =>
	               totalRecord += statInfo.value
	            case 'Transaction =>
	                totalTransaction += statInfo.value
	        }
	    })
	    val runTime = Runtime.getRuntime();
	    
	    val format = NumberFormat.getInstance();
	    
	    val freeMemory = runTime.freeMemory();
	    val totalMemory = runTime.totalMemory();
	    
	    val mm = ManagementFactory.getOperatingSystemMXBean()
	    
	    val cpu = mm.getSystemLoadAverage()
	   
	    writeToFile(format.format(totalTransaction) + "\t\t\t\t\t\t\t" 
	    	        + format.format(totalRecord) + "\t\t\t"
	    	        + format.format(cpu * 100) + "%\t\t\t"
	    	        + format.format((totalMemory - freeMemory)/ 1024 / 1024)
	    	        + "m\n")
	   
	    
	}
	
	def before() {
	    val fw = new File(Config.statFileName)
	    if (fw.isFile()) {
	        fw.delete()
	    }
	}
	def writeToFile(str: String) {
	    val fw = new FileWriter(Config.statFileName, true)
	    try {
	    	fw.write(str)
	    }finally
	    	fw.close() 
	    
	}
}