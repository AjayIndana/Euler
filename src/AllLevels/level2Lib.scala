package AllLevels

import scala.annotation.tailrec

/**
  * Created by aindana on 10/24/2016.
  */
class level2Lib extends BaseTest{


  def largestProductGrid(grid: Array[Array[Int]]): Unit ={

    def getColArray(n: Int, a: Array[Array[Int]]) = a.map{_(n - 1)}
    def getRowArray(n: Int, a: Array[Array[Int]]) = a(n-1)


    def getDiagArray(xss: Array[Array[Int]], length: Int, point1:Int, point2:Int): Array[Int] = {

      val ans = new Array[Int](length)
      @tailrec
      def inner(i: Int,point1:Int, point2:Int) {
        if (i < length) {
          ans(i) = xss(point1-1)(point2-1)
          inner(i+1,point1+1, point2+1)
        }
      }
      inner(0,point1,point2)
      ans
    }


    def maxProd(row:Int, start:Int, end:Int, increment:Int, slice:Int) : Long ={



      def inner(rowBool: Int, row:Int, start:Int, end:Int, increment:Int, prodFour:Long, slice:Int): Long = {
        rowBool match {
          case 0 | 1=> {
            row <= grid.length match {
              case true => {
                start <= end match {
                  case true => {
                     var total = if(rowBool == 0) product(getRowArray(row, grid).toList.slice(start,start+slice)) else product(getColArray(row, grid).toList.slice(start,start+slice))
                    total > prodFour match {
                      case true => inner(rowBool, row, start + increment, end, increment, total,slice)
                      case false => inner(rowBool, row, start + increment, end, increment, prodFour,slice)
                    }
                  }
                  case false => inner(rowBool, row + increment, 0, end, increment, prodFour,slice)
                }
              }
              case false => inner(rowBool + 1, 1, 0, end, increment, prodFour,slice)
            }
          }

          case 2 | 3 =>   {
            row <= grid.length-(slice-1) match {
              case true => {
                start <= end match {
                  case true => {
                   var total = if(rowBool == 2) product(getDiagArray(grid,slice,row,start+1).toList) else product(getDiagArray(grid,slice,start+1,row).toList)

                    total > prodFour match {
                      case true => inner(rowBool, row, start + increment, end, increment, total,slice)
                      case false => inner(rowBool, row, start + increment, end, increment, prodFour,slice)
                    }
                  }
                  case false => inner(rowBool, row + increment, 0, end, increment, prodFour,slice)
                }
              }
              case false => inner(rowBool + 1, 1, 0, end, increment, prodFour,slice)
            }
          }

          case 4 | 5 =>   {
            row <= grid.length-(slice-1) match {
              case true => {
                start <= end match {
                  case true => {
                     var total = if(rowBool == 2) product(getDiagArray(grid.reverse,slice,row,start+1).toList) else product(getDiagArray(grid.reverse,slice,start+1,row).toList)

                    total > prodFour match {
                      case true => inner(rowBool, row, start + increment, end, increment, total,slice)
                      case false => inner(rowBool, row, start + increment, end, increment, prodFour,slice)
                    }
                  }
                  case false => inner(rowBool, row + increment, 0, end, increment, prodFour,slice)
                }
              }
              case false => inner(rowBool + 1, 1, 0, end, increment, prodFour,slice)
            }
          }
          case _ => prodFour
        }
      }
      inner(0,row,start,end,increment,0,slice)
    }

    println(maxProd(1,0,grid.length-4,1,4))


  }

}
