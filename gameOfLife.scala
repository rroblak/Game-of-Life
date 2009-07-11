class Cell(alive: Boolean ) {
  var isAlive: Boolean = alive

  override def toString = if (this isAlive) "x" else "-"

  def isDead = !this.isAlive
}

object Grid {
  val width: Int = 10
  val height: Int = 10
  var grid = new Array[Array[Cell]](width, height)

  iter((x, y) => {
    if (x == 0 && y == 0) grid(x)(y) = new Cell(true)
    else if (x == 1 && y == 0) grid(x)(y) = new Cell(true)
    else if (x == 0 && y == 1) grid(x)(y) = new Cell(true)
    else grid(x)(y) = new Cell(false)
  }, Unit => Unit)

  private def iter(f: (Int, Int) => Unit, g: Unit => Unit) {
    for (y <- 0 to height - 1) {
      for (x <- 0 to width - 1) {
        f(x, y)
      }

      g()
    }
  }

  def goToNextStep = {
    iter((x, y) => {
      val aCell = grid(x)(y)

      val numberOfAliveNeighbors = numberOfAliveNeighborsOfCellAt(x, y)

      if (aCell isAlive) {
        if (numberOfAliveNeighbors < 2) kill(aCell)
        else if (numberOfAliveNeighbors > 3) kill(aCell)
      }
      else if (numberOfAliveNeighbors == 3) resurrect(aCell)
    }, Unit => Unit)
  }

  def numberOfAliveNeighborsOfCellAt(x: Int, y: Int): Int = {
    var numberOfAliveNeighbors: Int = 0

    iterNeighbors((x, y) => if (grid(x)(y) isAlive) numberOfAliveNeighbors += 1,
                  x, y)
    return 0
  }

  def iterNeighbors(f: (Int, Int) => Unit, x: Int, y: Int) {
    // iterate lower neighbors
    if (y > 0)
    {
      // bottom left
      if (x > 0) f(x-1, y-1)

      // bottom middle
      f(x, y-1)

      // bottom right
      if (x < width-1) f(x+1, y-1)
    }

    // iterate middle neighbors
    if (x > 0) f(x-1, y)
    if (x < width-1) f(x+1, y)

    // iterate top neighbors
    if (y < height-1) {
      // top left
      if (x > 0) f(x-1, y+1)

      // top middle
      f(x, y+1)

      // top right
      if (x < width-1) f(x+1, y+1)
    }
  }

  def kill(aCell: Cell) = aCell isAlive = false
  def resurrect(aCell: Cell) = aCell isAlive = true

  def isNotDead: Boolean = {
    var isDead = true

    iter((x,y) => {
      if (grid(x)(y).isAlive) isDead = false
    }, Unit => Unit)

    return !isDead
  }

  override def toString = {
    var gridStr = ""

    iter((x,y) => {
        gridStr = gridStr + grid(x)(y)
    }, Unit => {gridStr = gridStr + "\n"})

    gridStr
  }
}

object Main {
  def main(args: Array[String]) {
    do
    {
      print(Grid)

      Grid goToNextStep
    }
    while (Grid isNotDead)

    print(Grid)
  }
}
