package com.github.cyberscouter.udpserver

import java.nio.channels.DatagramChannel
import java.nio.ByteBuffer
import scala.util.Using
import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.Await
import scala.concurrent.Future
import java.{util => ju}
import java.util.concurrent.LinkedBlockingDeque
import java.util.concurrent.BlockingDeque
import scala.concurrent.ExecutionContext
import java.nio.ByteOrder
import java.util.concurrent.Executors
import java.net.SocketAddress
import java.io.IOException

object UDPListener {
  private implicit val DEFAULT_EXECUTION_CONTEXT =
    ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())

  def apply(
      socketAddr: SocketAddress,
      datagramSize: Int,
      byteOrder: ByteOrder
  ): UDPListener = {
    val datagramChannel = DatagramChannel.open();
    datagramChannel.bind(socketAddr)
    new UDPListener(datagramChannel, datagramSize, byteOrder)
  }
}

class UDPListener private (
    datagramChannel: DatagramChannel,
    datagramSize: Int,
    byteOrder: ByteOrder
)(implicit executor: ExecutionContext)
    extends Iterator[(SocketAddress, ByteBuffer)]
    with AutoCloseable {

  @volatile private var closed = false

  private val packageListener = executor.execute(() => {
    do {
      val pack = nextPackage
      packageBuffer.putLast(pack)
    } while (!closed)
    datagramChannel.close()
  })

  private def isOpen: Boolean = datagramChannel.isOpen()

  private val packageBuffer: BlockingDeque[(SocketAddress, ByteBuffer)] =
    new LinkedBlockingDeque()

  private def nextPackage: (SocketAddress, ByteBuffer) = {
    val buffer = ByteBuffer.allocate(datagramSize).order(byteOrder)
    val addr = datagramChannel.receive(buffer)
    (addr, buffer)
  }

  def available = packageBuffer.size()

  override def hasNext: Boolean = true

  @throws(classOf[IOException])
  override def next: (SocketAddress, ByteBuffer) = {
    packageBuffer.takeFirst
  }

  override def close: Unit = {
    closed = true
    // Flush it
    Using(DatagramChannel.open) {
      _.send(ByteBuffer.allocate(0), datagramChannel.getLocalAddress())
    }
  }
}
