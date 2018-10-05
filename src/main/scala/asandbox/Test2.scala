package asandbox

import java.nio.file.Paths
import java.nio.file.Files
import java.nio.charset.StandardCharsets
import org.web3j.protocol.http.HttpService
import org.web3j.abi.datatypes.Function
import org.web3j.abi.datatypes.Type
import java.util.Arrays
import org.web3j.abi.FunctionEncoder
import org.web3j.protocol.core.methods.request.Transaction
import java.math.BigInteger
import org.web3j.protocol.core.methods.request.EthFilter
import org.web3j.protocol.core.DefaultBlockParameter
import org.web3j.protocol.core.DefaultBlockParameterName
import org.web3j.protocol.core.DefaultBlockParameterNumber
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.collection.Seq
import org.web3j.protocol.core.methods.response.Log
import org.web3j.protocol.core.methods.response.EthLog.LogResult
import org.web3j.ens.Contracts
import org.bouncycastle.jcajce.provider.digest.Keccak
import org.web3j.crypto.Hash
import javax.xml.bind.DatatypeConverter

class EventSignature(
  val name: String,
  val types: Seq[(String, String, Boolean)]) {

  override val toString =
    name + types.map(_._2).mkString("(", ",", ")")

  val sign =
    Hash.sha3(DatatypeConverter.printHexBinary(toString.getBytes("UTF-8")))

  def logMatch(log: Log) =
    log.getTopics.get(0) equals sign

}

class Event(
  val sign: EventSignature,
  val log: Log) {

  val fields =
    (sign.types.filter(_._3) ++: sign.types.filterNot(_._3))
      .zip(log.getTopics.asScala.toSeq.tail ++: log.getData.substring(2).grouped(64).map("0x" + _).toSeq)
      .map(_ match {
        case ((name, "uint256", _), value) => (name, new BigInteger(value.substring(2), 16).toString)
        case ((name, "address", _), value) => (name, "0x" + value.substring(26).toLowerCase)
      })
      .toMap

  override val toString: String =
    sign.name + fields.map { case (name, value) => name + " = " + value }.mkString("(", ",", ")")

}

object TransferEventSignature extends EventSignature(
  "Transfer",
  Seq(
    ("from", "address", true),
    ("to", "address", true),
    ("amount", "uint256", false)))

object TicketPurchasedEventSignature extends EventSignature(
  "TicketPurchased",
  Seq(
    ("lotAddr", "address", false),
    ("lotIndex", "uint256", false),
    ("ticketNumber", "uint256", false),
    ("player", "address", false),
    ("ticketPrice", "uint256", false)))

object TicketWinEventSignature extends EventSignature(
  "TicketWin",
  Seq(
    ("lotAddr", "address", false),
    ("lotIndex", "uint256", false),
    ("ticketNumber", "uint256", false),
    ("player", "address", false),
    ("win", "uint256", false)))

class TransferEvent(override val log: Log)
  extends Event(
    TransferEventSignature,
    log) {

  val to = fields("to")

  val from = fields("to")

  val amount = fields("amount")

}

class TicketWinEvent(override val log: Log)
  extends Event(
    TicketWinEventSignature,
    log) {

  val lotAddr = fields("lotAddr")

  val lotIndex = fields("lotIndex")

  val ticketNumber = fields("ticketNumber")

  val player = fields("player")

  val ticketPrice = fields("ticketPrice")

}

class TicketPurchasedEvent(override val log: Log)
  extends Event(
    TicketPurchasedEventSignature,
    log) {

  val lotAddr = fields("lotAddr")

  val lotIndex = fields("lotIndex")

  val ticketNumber = fields("ticketNumber")

  val player = fields("player")

  val ticketPrice = fields("ticketPrice")

}

object EventFactory {

  def apply(log: Log): Option[Event] =
    if (TransferEventSignature.logMatch(log))
      Some(new TransferEvent(log))
    else if (TicketPurchasedEventSignature.logMatch(log))
      Some(new TicketPurchasedEvent(log))
    else if (TicketWinEventSignature.logMatch(log))
      Some(new TicketWinEvent(log))
    else
      None

}

object Test2 extends App {

  val targetAddr = "0x0a919A4480183B33ED516aF57619808AEdCfdB6E"

  val fromBlock = 4177376

  val service = new HttpService("https://ropsten.infura.io/....")

  val web3 = org.web3j.protocol.Web3j.build(service)

  val web3ClientVersion = web3.web3ClientVersion().send()
  if (!web3ClientVersion.hasError()) {

    val ethFilter = new EthFilter(
      new DefaultBlockParameterNumber(fromBlock),
      DefaultBlockParameterName.LATEST,
      targetAddr)

    val result = web3.ethGetLogs(ethFilter).send()

    val logs = result.getLogs.asScala.toSeq
    println("Count " + logs.length)

    // topcis:
    // 0. Keccak-256 hash of transfer(uint256,uint256)
    // 1. from
    // 2. to
    // 3. data[0] = amount
    //
    //
    //

    logs.foreach { log =>
      val logObject = log.get.asInstanceOf[Log]
      val event = EventFactory.apply(logObject)
      println(event)
    }

  }

}