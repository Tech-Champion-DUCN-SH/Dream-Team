package my.shorten

import java.net.InetSocketAddress

object App {
  def main(args : Array[String]) : Unit = {
    new Dispatcher().start();
  }
}
