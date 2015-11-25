
import scala.util.Random

	def whileLoopInnderProduct(v1: Vector[Double], v2: Vector[Double]): Double = {
		var i = 0
		var sum = 0.0
		val len = v1.length
		while(i < len) {
			sum = sum + v1(i) * v2(i)
			i += 1
		}	
		sum
	}

	def recusiveInnerProduct(v1: Vector[Double], v2: Vector[Double]): Double = {

		def recInnPro(index: Int): Double = {
			if (index < v1.length)
				v1(index) * v2(index) + recInnPro(index + 1)
			else
				0.0
		}

		recInnPro(0)

	}

	def tailRecursiveInnerProduct(v1: Vector[Double], v2: Vector[Double]): Double = {
		def tailRecInnPro(acc: Double, index: Int): Double = {
			if(index < v1.length)
				tailRecInnPro(v1(index)*v2(index) + acc, index+1)
			else
				0.0
		}
		tailRecInnPro(0.0, 0)
	}

def testFunction(f: (Vector[Double], Vector[Double]) => Double, 
	                 v1: Vector[Double],
	                 v2: Vector[Double]) {

	val startTime = System.nanoTime

	for(i <- 1 to 2000; j <- 1 to 2000)
		f(v1, v2)

	val endTime = System.nanoTime

	println((endTime-startTime)/1000000000d + " seconds")
}

val functionList = List(whileLoopInnderProduct _,
	                    tailRecursiveInnerProduct _,
	                    recusiveInnerProduct _, 
	                    (v1: Vector[Double],v2: Vector[Double]) => 
	                    	v1.view.zip(v2).map{ case (a,b) => a*b }.reduceLeft(_+_),
	                    (v1: Vector[Double],v2: Vector[Double]) => 
	                    	(v1,v2).zipped.map{ case (a,b) => a*b }.reduceLeft(_+_),
	                    (v1: Vector[Double],v2: Vector[Double]) =>
	                    	(v1 zip v2).foldLeft(0.0){ case (a, (b,c)) => a + b*c }, 
	                    (v1: Vector[Double],v2: Vector[Double]) => 
	                    	(v1 zip v2).map{case (a,b) => a * b}.foldLeft(0.0)(_+_)
	                    	 
	                    )

val vector1 = Vector.fill(2000)(Random.nextDouble)
val vector2 = Vector.fill(2000)(Random.nextDouble)

functionList.foreach { testFunction(_, vector1, vector2) }


//1000*1000*1000
	//tailRecursiveInnerProduct(v1, v2) //17.12sec
	//recusiveInnerProduct(v1, v2) //19.85
	//innderProduct(v1, v2) //20.78sec
	//v1.view.zip(v2).map{ case (a,b) => a*b }.reduceLeft(_+_) // 41.95sec
	//(v1,v2).zipped.map{ case (a,b) => a*b }.reduceLeft(_+_) // 48.58sec
	//(v1 zip v2).foldLeft(0.0){ case (a, (b,c)) => a + b*c } //51.52sec
	//(v1 zip v2).map{case (a,b) => a * b}.foldLeft(0.0)(_+_) //61.77sec

//2000*2000*2000
/*
147.423672402 seconds
169.661531128 seconds
142.6726826 seconds
342.784859896 seconds
411.917628386 seconds
448.64638538 seconds
504.065234144 seconds
*/	