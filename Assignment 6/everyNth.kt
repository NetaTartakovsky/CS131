fun everyNth(L: List<Any>, N: Int): List<Any> {
	val s = L.size
	val temp: MutableList<Any> = mutableListOf()
	if (s < 1) {
		val final: List<Any> = temp
		return final
	}
	if (N !in 1..s) {
		val final: List<Any> = temp
		return final
	}
	var i = N-1
	while (i < s) {
		temp.add(L.get(i))
		i += N
	}
	val final: List<Any> = temp
	return final
}

fun main(args: Array<String>) {
	var test = true
	val L = listOf(1,2,3,4,5,6,7,8,9,10)
	val EL: List<Int> = emptyList()
	
	val L1 = listOf(2,4,6,8,10)
	val L2 = everyNth(L,2)
	if (!(L1.equals(L2))) { test = false }
	
	val L3 = listOf(3,6,9)
	val L4 = everyNth(L,3)
	if (!(L3.equals(L4))) { test = false }

	val L5 = everyNth(L,0)
	if (!(L5.equals(EL))) { test = false }

	val L6 = everyNth(L,11)
	if (!(L6.equals(EL))) { test = false }

	val L7 = everyNth(EL,5)
	if (!(L7.equals(EL))) { test = false }

	if (test) {
		println("Passed all tests!")
	} else {
		println("Failed one or more tests!")
	}
}
