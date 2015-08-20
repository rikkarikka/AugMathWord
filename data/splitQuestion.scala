import scala.io.Source

object splitQuestion {

def main(args: Array[String]) {
	var fi = Source.fromFile(args{0})
	var fo = fi.mkString.trim.split("\\. ")
	fi.close()
	for(x<-fo){
		if (!x.trim.endsWith("?")) println(x.concat(".")) else println(x)
	}
}
}
