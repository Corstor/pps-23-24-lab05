package ex

import util.Optionals.Optional
import util.Sequences.*

trait Item:
  def code: Int
  def name: String
  def tags: Sequence[String]

object Item:
  private case class ItemImpl(code: Int, name: String, tags: Sequence[String]) extends Item

  import Sequence.*
  def apply(code: Int, name: String, tags: String*): Item =
    ItemImpl(code, name, tags.foldLeft(Nil[String]())(
        (seq, tag) => Cons(tag, seq)
      )
    )

  object sameTag:
    def unapply(items: Sequence[Item]): Option[String] =
      var found: Optional[String] = Optional.Empty()
      items match
        case Cons(h: Item, t) => for tag <- h.tags do
          if containsTag(t, tag)
          then found = Optional.Just(tag)
        case _ =>
      found.toOption


  private def containsTag(items: Sequence[Item], tag: String): Boolean =
    items.allMatch(item => item.tags.contains(tag))

/**
 * A warehouse is a place where items are stored.
 */
trait Warehouse:
  /**
   * Stores an item in the warehouse.
   * @param item the item to store
   */
  def store(item: Item): Unit
  /**
   * Searches for items with the given tag.
   * @param tag the tag to search for
   * @return the list of items with the given tag
   */
  def searchItems(tag: String): Sequence[Item]
  /**
   * Retrieves an item from the warehouse.
   * @param code the code of the item to retrieve
   * @return the item with the given code, if present
   */
  def retrieve(code: Int): Optional[Item]
  /**
   * Removes an item from the warehouse.
   * @param item the item to remove
   */
  def remove(item: Item): Unit
  /**
   * Checks if the warehouse contains an item with the given code.
   * @param itemCode the code of the item to check
   * @return true if the warehouse contains an item with the given code, false otherwise
   */
  def contains(itemCode: Int): Boolean
end Warehouse

object Warehouse:
  private class WarehouseImpl extends Warehouse:
    import Sequence.*

    private var items: Sequence[Item] = Nil()
    override def contains(itemCode: Int): Boolean =
      items.find(i => i.code == itemCode).isNotEmpty

    override def remove(item: Item): Unit =
      items = items.filter(i => i != item)

    override def searchItems(tag: String): Sequence[Item] =
      items.filter(i => i.tags.contains(tag))

    override def retrieve(code: Int): Optional[Item] =
      items.find(i => i.code == code)

    override def store(item: Item): Unit =
      items = Cons(item, items)

  def apply(): Warehouse = WarehouseImpl()

@main def mainWarehouse(): Unit =
  val warehouse = Warehouse()

  val dellXps = Item(33, "Dell XPS 15", "notebook")
  val dellInspiron = Item(34, "Dell Inspiron 13", "notebook")
  val xiaomiMoped = Item(35, "Xiaomi S1", "moped", "mobility")

  println:
    warehouse.contains(dellXps.code) // false
  warehouse.store(dellXps) // side effect, add dell xps to the warehouse
  println:
    warehouse.contains(dellXps.code) // true
  warehouse.store(dellInspiron) // side effect, add dell Inspiron to the warehouse
  warehouse.store(xiaomiMoped) // side effect, add xiaomi moped to the warehouse
  println:
    warehouse.searchItems("mobility") // Sequence(xiaomiMoped)
  println:
    warehouse.searchItems("notebook") // Sequence(dellXps, dell Inspiron)
  println:
    warehouse.retrieve(11) // None
  println:
    warehouse.retrieve(dellXps.code) // Just(dellXps)
  warehouse.remove(dellXps) // side effect, remove dell xps from the warehouse
  println:
    warehouse.retrieve(dellXps.code) // None

  import Sequence.*

  val notebook = Item(36, "note", "notebook")

  val items: Sequence[Item] = Cons(dellXps, Cons(dellInspiron, Cons(notebook, Nil())))

  import Item.*

  items match
    case sameTag(t) => println(s"items have same tag $t")
    case _ => println(s"items have different tags")

/** Hints:
 * - Implement the Item with a simple case class
 * - Implement the Warehouse keeping a private List of items
 * - Start implementing contains and store
 * - Implement searchItems using filter and contains
 * - Implement retrieve using find
 * - Implement remove using filter
 * - Refactor the code of Item accepting a variable number of tags (hint: use _*)
*/