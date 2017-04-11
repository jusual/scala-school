package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  *
  */
object MergeSortImpl extends App {

  def mergeSort(data: Seq[Int]): Seq[Int] = {
    if (data.length == 1) return data
    val data1 = data.dropRight(data.length / 2)
    val data2 = data.takeRight(data.length / 2)
    merge(mergeSort(data1), mergeSort(data2), 0, 0, List()).reverse
  }

  def merge(data1: Seq[Int], data2: Seq[Int], i: Int, j: Int, ans: List[Int]): Seq[Int] = {
    if (i == data1.length || j == data2.length)
      data1.takeRight(data1.length - i).reverse ++ data2.takeRight(data2.length - j).reverse ++ ans
    else if (data1(i) < data2(j))
      merge(data1, data2, i + 1, j, data1(i) :: ans)
    else
      merge(data1, data2, i, j + 1, data2(j) :: ans)
  }
}
