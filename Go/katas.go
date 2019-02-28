package kata

func HasUniqueChar(str string) bool {
	m := make(map[rune]bool)
	for _, c := range str {
		if m[c] {
			return false
		}
		m[c] = true
	}
	return true
}

func NbMonths(startPriceOld, startPriceNew, savingperMonth int, percentLossByMonth float64) [2]int {
	m := 0
	priceOld := float64(startPriceOld)
	priceNew := float64(startPriceNew)
	for ; priceOld+float64(m*savingperMonth)-priceNew < 0; m++ {
		if m%2 == 1 {
			percentLossByMonth += 0.5
		}
		priceOld -= percentLossByMonth * 0.01 * priceOld
		priceNew -= percentLossByMonth * 0.01 * priceNew
	}
	a := priceOld + float64(m*savingperMonth) - priceNew + 0.5 // plus 0.5 to get round to int
	return [2]int{m, int(a)}
}

func TwoSum(numbers []int, target int) [2]int {
	for i, x := range numbers {
		for j, y := range numbers[i+1:] {
			if x+y == target {
				return [2]int{i, i + j + 1}
			}
		}
	}
	return [2]int{}
}
