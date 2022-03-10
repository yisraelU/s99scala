object variance {
  class Vehicle
  class Bike extends Vehicle
  class Car extends Vehicle
  abstract class CovarParking[T](var list: List[T]) {
    def park(vehicle: T): Unit ={
      list = vehicle ::  list
    }
    def impound(vehicles: List[T]): Unit ={
      list = list.diff(vehicles)
    }
    def checkvehicles(condition: String): List[T] = {
      list.filter(_.getClass.getName.contains(condition))
    }
  }
  type ContraList[-T]
  abstract class ContraParking[-T](var list: ContraList[T]) {
    def park(vehicle: T): Unit
    def impound(vehicles: List[T]): Unit
    def checkvehicles(condition: String): ContraList[T]
  }
  type IList[T]
  abstract class InvariantParking[T](var list: IList[T]) {
    def park(vehicle: T): InvariantParking[T]
    def impound(vehicles: IList[T]):Unit
    def checkvehicles(condition: String): IList[T]
  }
}