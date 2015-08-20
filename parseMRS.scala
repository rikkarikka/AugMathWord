import scala.io.Source
import scala.collection.mutable.ListBuffer


object parseQ {
	val VERBOSE = true
	class MRS(sent:String,specs:String){

		val word = parseWord
		val rel = specs.split("\\[")(1).split("<")(0).trim
		val lbl = specs.split("LBL: ")(1).split(" ")(0)
		val args = parseArgs

		def parseWord(): String = {
			val wordr = """\<[0-9]+:[0-9]+\>""".r
			val wordbound = wordr findFirstIn specs 
			val wb = wordbound.mkString drop (1) dropRight (1) split (":") map (_.toInt)
			return sent substring(wb(0),wb(1))

		}

		def parseArgs(): List[String] = {
			var i = 0
			var a = new ListBuffer[String]
			while (specs contains "ARG"+i.toString) {
				a += specs.split("ARG"+i.toString+": ")(1).split(" ")(0)
				i+=1
			}
			return a.toList
		}

		def print(): Unit = {
			println(word)
			println(" -- ",rel)
			println(" -- ",lbl)
			println(" -- ",args)
		}
				
		
	}
	def parseS(input:String){
		var sent = input.split("SENT:")(1).split("\\[")(0).trim	
		var spl = input.split("RELS:")(1).split("HCONS")(0)
		val MRS = spl.split("\n")
		var objs = new ListBuffer[MRS]
		for (x<-MRS) {
			objs += new MRS(sent,x)
			/*
			println(x)
			println(w.word,w.rel,w.lbl)
			w.args.foreach(println)
			*/
		}

		// Get Entities and Events
		def entities():List[Int]= (for ((o,i) <- objs.toList.zipWithIndex if o.args(0).startsWith("x")) yield i) 
		def events():List[Int]= (for ((o,i) <- objs.toList.zipWithIndex if o.args(0).startsWith("e")) yield i) 

		if (VERBOSE) entities.foreach(objs(_).print)
		print(entities,events)
	}
	
	def main(args:Array[String]) {
		var fi = Source.fromFile("data/problems/1.mrs")
		var sentences = fi.mkString.split("\n\n") filter (! _.trim.isEmpty)
		fi.close()
		for (x<-sentences) {
			parseS(x)
			println
		}
	}
}
