class SimpleCounter {
  private var count: Int = 0

  def reset(): Int = {
    count = 0
    count
  }

  def inc(): Int = {
    count = count + 1
    return count
  }

  def currentCount: Int = count
}
