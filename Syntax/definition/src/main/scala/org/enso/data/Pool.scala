package org.enso.data

import scala.reflect.internal.util.WeakHashSet

/** Thread safe pool for objects with 1-1 hashcode-object mapping.
  * Useful for not having lots of duplicate objects in memory.
  */
final class Pool[T <: AnyRef] {

  private val astPool = WeakHashSet[T]()

  def get(t: T): T = synchronized(astPool.findEntryOrUpdate(t))

}
