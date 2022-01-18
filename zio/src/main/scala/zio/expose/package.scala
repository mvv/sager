package zio

package object expose {
  def hadFatalError(): Boolean =
    zio.internal.FiberContext.fatal.get()
}
