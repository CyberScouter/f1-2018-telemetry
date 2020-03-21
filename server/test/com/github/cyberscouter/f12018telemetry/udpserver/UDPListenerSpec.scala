package com.github.cyberscouter.f12018telemetry.udpserver

import java.net.InetSocketAddress
import org.scalatest.Matchers
import scala.util.Random
import java.nio.ByteBuffer
import org.scalatest.FlatSpec
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import java.nio.channels.DatagramChannel
import java.net.SocketAddress
import com.github.cyberscouter.udpserver.UDPListener
import java.nio.ByteOrder
import scala.util.Try
import scala.util.Using
import scala.util.Failure
import scala.util.Success

// Uses port 12345; it has to be free; change it, if it isn't
class UDPListenerSpec extends FlatSpec with Matchers {

  val LISTENING_SOCKET = new InetSocketAddress(12345)

  val BYTE_ORDER = ByteOrder.LITTLE_ENDIAN

  def dataHelper(
      seed: Int,
      widthFunction: Int => Int,
      fillWidthFunction: Int => Int,
      posFunction: Int => Int
  ) = {
    val buffers = (0 until 16)
      .map(x => (x, new Random(seed + x)))
      .map(x => {
        val pos = posFunction(x._1)
        val width = widthFunction(x._1)
        val fillWidth = fillWidthFunction(x._1)
        val bytes = x._2.nextBytes(fillWidth min width)
        val buffer = ByteBuffer.allocate(width)
        buffer.put(bytes)
        buffer.position(pos min width)
        buffer
      })
      .toList
    new Random(seed).shuffle(buffers)
  }

  def data(seed: Int) = dataHelper(seed, identity, identity, _ => 0)

  def dataWithBufferSize(seed: Int, width: Int) =
    dataHelper(seed, _ => width, identity, identity)

  def sendData(
      buffers: Iterable[ByteBuffer],
      socket: SocketAddress
  ): SocketAddress = {
    val channel = DatagramChannel.open()
    for (buffer <- buffers)
      channel.send(buffer, socket)
    channel.getLocalAddress
  }

  "nothing sent" should "receive nothing" in {
    Using(UDPListener(LISTENING_SOCKET, 42, BYTE_ORDER)) { listener =>
      {
        Thread.sleep(100)
        listener.take(listener.available).isEmpty
      }
    } match {
      case Failure(exception) => throw exception
      case Success(value)     => value should be(true)
    }
  }

  "data sent" should "receive data" in {
    val seed = 42
    val data = this.data(seed)
    Using(UDPListener(LISTENING_SOCKET, 8192, BYTE_ORDER)) { listener =>
      {
        sendData(data, LISTENING_SOCKET)
        listener.take(data.length).map(_._2).toList
      }
    } match {
      case Failure(exception) => throw exception
      case Success(value) =>
        value should be(this.dataWithBufferSize(seed, 8192))
    }
  }

  "long data sent" should "be cut off when received" in {
    val data = this.data(42)
    Using(UDPListener(LISTENING_SOCKET, 5, BYTE_ORDER)) { listener =>
      {
        sendData(data, LISTENING_SOCKET)
        listener.take(data.length).toList.map(_._2)
      }
    } match {
      case Failure(exception) => throw exception
      case Success(value) =>
        value should be(this.dataWithBufferSize(42, 5))
    }
  }

  "short data sent" should "return buffers filled with zeroes" in {
    val data = this.data(42)
    Using(UDPListener(LISTENING_SOCKET, 1024, BYTE_ORDER)) { listener =>
      {
        sendData(data, LISTENING_SOCKET)
        listener.take(data.length).toList.map(_._2)
      }
    } match {
      case Failure(exception) => throw exception
      case Success(value) =>
        value should be(this.dataWithBufferSize(42, 1024))
    }
  }

  "data sent from multiple sources" should "be aggregated" in {
    val data1 = this.data(1933)
    val data2 = this.data(1945)
    val expected =
      (dataWithBufferSize(1933, 64) ++ dataWithBufferSize(1945, 64))
        .map(_.array.mkString)
        .sorted
    Using(UDPListener(LISTENING_SOCKET, 64, BYTE_ORDER)) { listener =>
      {
        sendData(data1.take(3), LISTENING_SOCKET)
        sendData(data2.take(3), LISTENING_SOCKET)
        sendData(data1.drop(3), LISTENING_SOCKET)
        sendData(data2.drop(3), LISTENING_SOCKET)
        listener
          .take(data1.length + data2.length)
          .toList
          .map(_._2.array.toList.mkString)
          .sorted
      }
    } match {
      case Failure(exception) => throw exception
      case Success(value) =>
        value should be(expected)
    }
  }
}
