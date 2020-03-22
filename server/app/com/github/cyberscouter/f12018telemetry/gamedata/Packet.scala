package com.github.cyberscouter.f12018telemetry.gamedata

import java.nio.ByteBuffer
import java.{lang => j}
import scala.io.Codec

sealed abstract class Packet(header: PacketHeader) {}

object Packet {
  import PacketType._

  def apply(parser: PacketParseable): Packet = {
    val header = PacketHeader(parser)
    header.packetId match {
      case MOTION        => PacketMotionData(header, parser)
      case SESSION       => PacketSessionData(header, parser)
      case LAP_DATA      => PacketLapData(header, parser)
      case EVENT         => PacketEventData(header, parser)
      case PARTICIPANTS  => PacketParticipantsData(header, parser)
      case CAR_SETUPS    => PacketCarSetupData(header, parser)
      case CAR_TELEMETRY => PacketCarTelemetryData(header, parser)
      case CAR_STATUS    => PacketCarStatusData(header, parser)
    }
  }

  implicit class ParseableByteBuffer(buffer: ByteBuffer)
      extends PacketParseable {
    def readBytes(n: Int): Array[Byte] = {
      val arr = Array.fill(n)(0.byteValue())
      buffer.get(arr)
      arr
    }
  }

  trait PacketParseable {
    def readBytes(n: Int): Array[Byte]

    def readUIntNB(byteNumber: Int): BigInt = {
      readBytes(byteNumber)
        .map(j.Byte.toUnsignedInt(_))
        .map(BigInt(_))
        .zipWithIndex
        .map(tuple => tuple._1 << (tuple._2 * 8))
        .reduce(_ | _)
    }

    def readIntNB(byteNumber: Int): BigInt = {
      val bytes = readBytes(byteNumber)
      val msByte = bytes.last
      val otherPart = bytes.init
        .map(j.Byte.toUnsignedInt(_))
        .map(BigInt(_))
        .zipWithIndex
        .map(tuple => tuple._1 << (tuple._2 * 8))
        .reduce(_ | _)
      otherPart + (BigInt(msByte) * (BigInt(8) << (bytes.length - 1)))
    }

    def readUInt64: BigInt = readUIntNB(8)
    def readUInt: Long = readUInt32
    def readUInt32: Long = readUIntNB(4).longValue
    def readUInt16: Int = readUIntNB(2).byteValue
    def readUInt8: Int = readUIntNB(1).byteValue

    def readInt64: Long = readIntNB(8).longValue
    def readInt: Long = readInt32
    def readInt32: Long = readIntNB(4).intValue
    def readInt16: Int = readIntNB(2).intValue
    def readInt8: Int = readIntNB(1).intValue

    def readFloat: Double =
      ByteBuffer.wrap(readBytes(4)).getFloat().toDouble
    def readDouble: Double =
      ByteBuffer.wrap(readBytes(8)).getDouble()
    def readBoolean: Boolean = readBytes(1).head != 0
    def read1ByteChar: Char =
      ByteBuffer.wrap(readBytes(1)).getChar()
    def readString(nBytes: Int): String =
      Codec
        .fromUTF8(readBytes(nBytes).takeWhile(_ != 0) ++ Array[Byte](0))
        .mkString
  }
}

case class PacketHeader(
    packetFormat: Int,
    packetVersion: Int,
    packetId: PacketType.PacketType,
    sessionUID: BigInt,
    sessionTime: Double,
    frameIdentifier: Long,
    playerCarIndex: Int
)

object PacketHeader {
  import Packet.PacketParseable
  import PacketType._

  def apply(parseable: PacketParseable): PacketHeader = {
    val format = parseable.readUInt16
    val packetVersion = parseable.readUInt8
    val packetId = PacketType(parseable.readUInt8)
    val sessionUID = parseable.readUInt64
    val sessionTime = parseable.readFloat
    val frameIdentifier = parseable.readUInt
    val playerCarIndex = parseable.readUInt8
    PacketHeader(
      format,
      packetVersion,
      packetId,
      sessionUID,
      sessionTime,
      frameIdentifier,
      playerCarIndex
    )
  }
}

object PacketType {
  sealed trait PacketType
  case object MOTION extends PacketType
  case object SESSION extends PacketType
  case object LAP_DATA extends PacketType
  case object EVENT extends PacketType
  case object PARTICIPANTS extends PacketType
  case object CAR_SETUPS extends PacketType
  case object CAR_TELEMETRY extends PacketType
  case object CAR_STATUS extends PacketType

  def apply(id: Int): PacketType = id match {
    case 0 => MOTION
    case 1 => SESSION
    case 2 => LAP_DATA
    case 3 => EVENT
    case 4 => PARTICIPANTS
    case 5 => CAR_SETUPS
    case 6 => CAR_TELEMETRY
    case 7 => CAR_STATUS
    case _ => throw new IllegalArgumentException
  }
}

object PacketMotionData {
  import Packet._

  def apply(header: PacketHeader, parser: PacketParseable): PacketMotionData = {
    val carMotionData = Vector.fill(20)(CarMotionData(parser))
    val suspensionPosition = WheelData(parser.readFloat)
    val suspensionVelocity = WheelData(parser.readFloat)
    val suspensionAcceleration = WheelData(parser.readFloat)
    val wheelSpeed = WheelData(parser.readFloat)
    val wheelSlip = WheelData(parser.readFloat)
    val localVelocityX = parser.readFloat
    val localVelocityY = parser.readFloat
    val localVelocityZ = parser.readFloat
    val angularVelocityX = parser.readFloat
    val angularVelocityY = parser.readFloat
    val angularVelocityZ = parser.readFloat
    val angularAccelerationX = parser.readFloat
    val angularAccelerationY = parser.readFloat
    val angularAccelerationZ = parser.readFloat
    val frontWheelsAngle = parser.readFloat
    PacketMotionData(
      header,
      carMotionData,
      suspensionPosition,
      suspensionVelocity,
      suspensionAcceleration,
      wheelSpeed,
      wheelSlip,
      localVelocityX,
      localVelocityY,
      localVelocityZ,
      angularVelocityX,
      angularVelocityY,
      angularVelocityZ,
      angularAccelerationX,
      angularAccelerationY,
      angularAccelerationZ,
      frontWheelsAngle
    )
  }
}

case class PacketMotionData(
    header: PacketHeader,
    carMotionData: Vector[CarMotionData],
    suspensionPosition: WheelData[Double],
    suspensionVelocity: WheelData[Double],
    suspensionAcceleration: WheelData[Double],
    wheelSpeed: WheelData[Double],
    wheelSlip: WheelData[Double],
    localVelocityX: Double,
    localVelocityY: Double,
    localVelocityZ: Double,
    angularVelocityX: Double,
    angularVelocityY: Double,
    angularVelocityZ: Double,
    angularAccelerationX: Double,
    angularAccelerationY: Double,
    angularAccelerationZ: Double,
    frontWheelsAngle: Double
) extends Packet(header)

object CarMotionData {
  import Packet._
  def apply(parser: PacketParseable): CarMotionData = {
    val worldPositionX = parser.readDouble
    val worldPositionY = parser.readDouble
    val worldPositionZ = parser.readDouble
    val worldVelocityX = parser.readDouble
    val worldVelocityY = parser.readDouble
    val worldVelocityZ = parser.readDouble
    val worldForwardDirX = parser.readDouble
    val worldForwardDirY = parser.readDouble
    val worldForwardDirZ = parser.readDouble
    val worldRightDirX = parser.readDouble
    val worldRightDirY = parser.readDouble
    val worldRightDirZ = parser.readDouble
    val gForceLateral = parser.readDouble
    val gForceLongitudinal = parser.readDouble
    val gForceVertical = parser.readDouble
    val yaw = parser.readDouble
    val pitch = parser.readDouble
    val roll = parser.readDouble
    CarMotionData(
      worldPositionX,
      worldPositionY,
      worldPositionZ,
      worldVelocityX,
      worldVelocityY,
      worldVelocityZ,
      worldForwardDirX,
      worldForwardDirY,
      worldForwardDirZ,
      worldRightDirX,
      worldRightDirY,
      worldRightDirZ,
      gForceLateral,
      gForceLongitudinal,
      gForceVertical,
      yaw,
      pitch,
      roll
    )
  }
}

case class CarMotionData(
    worldPositionX: Double,
    worldPositionY: Double,
    worldPositionZ: Double,
    worldVelocityX: Double,
    worldVelocityY: Double,
    worldVelocityZ: Double,
    worldForwardDirX: Double,
    worldForwardDirY: Double,
    worldForwardDirZ: Double,
    worldRightDirX: Double,
    worldRightDirY: Double,
    worldRightDirZ: Double,
    gForceLateral: Double,
    gForceLongitudinal: Double,
    gForceVertical: Double,
    yaw: Double,
    pitch: Double,
    roll: Double
)

object PacketSessionData {
  import Packet._
  def apply(
      header: PacketHeader,
      parser: PacketParseable
  ): PacketSessionData = {
    val weather = Weather(parser)
    val trackTemperature = parser.readInt8
    val airTemperature = parser.readInt8
    val totalLaps = parser.readUInt8
    val trackLength = parser.readUInt16
    val sessionType = SessionType(parser)
    val trackId = parser.readInt8
    val era = Era(parser)
    val sessionTimeLeft = parser.readUInt16
    val sessionDuration = parser.readUInt16
    val pitSpeedLimit = parser.readUInt8
    val gamePaused = parser.readBoolean
    val isSpectating = parser.readBoolean
    val spectatorCarIndex = parser.readUInt8
    val sliProNativeSupport = parser.readBoolean
    val numMarshalZones = parser.readUInt8
    val marshallZones = Vector.fill(21)(MarshallZone(parser))
    val safetyCarStatus = SafetyCarStatus(parser)
    val networkGame = parser.readBoolean
    PacketSessionData(
      header: PacketHeader,
      weather: Weather.Weather,
      trackTemperature: Int,
      airTemperature: Int,
      totalLaps: Int,
      trackLength: Int,
      sessionType: SessionType.SessionType,
      trackId: Int,
      era: Era.Era,
      sessionTimeLeft: Int,
      sessionDuration: Int,
      pitSpeedLimit: Int,
      gamePaused: Boolean,
      isSpectating: Boolean,
      spectatorCarIndex: Int,
      sliProNativeSupport: Boolean,
      numMarshalZones: Int,
      marshallZones: Vector[MarshallZone],
      safetyCarStatus: SafetyCarStatus.SafetyCarStatus,
      networkGame: Boolean
    )
  }
}

case class PacketSessionData(
    header: PacketHeader,
    weather: Weather.Weather,
    trackTemperature: Int,
    airTemperature: Int,
    totalLaps: Int,
    trackLength: Int,
    sessionType: SessionType.SessionType,
    trackId: Int,
    era: Era.Era,
    sessionTimeLeft: Int,
    sessionDuration: Int,
    pitSpeedLimit: Int,
    gamePaused: Boolean,
    isSpectating: Boolean,
    spectatorCarIndex: Int,
    sliProNativeSupport: Boolean,
    numMarshalZones: Int,
    marshallZones: Vector[MarshallZone],
    safetyCarStatus: SafetyCarStatus.SafetyCarStatus,
    networkGame: Boolean
) extends Packet(header)

object MarshallZone {
  import Packet.PacketParseable
  def apply(parser: PacketParseable): MarshallZone = {
    val zoneStart = parser.readFloat
    val zoneFlag = ZoneFlag(parser)
    MarshallZone(zoneStart, zoneFlag)
  }
}

case class MarshallZone(
    zoneStart: Double,
    zoneFlag: ZoneFlag.ZoneFlag
)

object ZoneFlag {
  import Packet.PacketParseable
  sealed trait ZoneFlag
  case object UNKNOWN extends ZoneFlag
  case object NONE extends ZoneFlag
  case object GREEN extends ZoneFlag
  case object BLUE extends ZoneFlag
  case object YELLOW extends ZoneFlag
  case object RED extends ZoneFlag

  def apply(parser: PacketParseable): ZoneFlag = apply(parser.readInt8)
  def apply(zoneFlag: Int): ZoneFlag = {
    zoneFlag match {
      case -1 => UNKNOWN
      case 0  => NONE
      case 1  => GREEN
      case 2  => BLUE
      case 3  => YELLOW
      case 4  => RED
      case _  => throw new IllegalArgumentException
    }
  }
}

object Weather {
  import Packet.PacketParseable
  sealed trait Weather
  case object CLEAR extends Weather
  case object LIGHT_CLOUD extends Weather
  case object OVERCAST extends Weather
  case object LIGHT_RAIN extends Weather
  case object HEAVY_RAIN extends Weather
  case object STORM extends Weather

  def apply(parser: PacketParseable): Weather = apply(parser.readUInt8)

  def apply(weather: Int): Weather = {
    weather match {
      case 0 => CLEAR
      case 1 => LIGHT_CLOUD
      case 2 => OVERCAST
      case 3 => LIGHT_RAIN
      case 4 => HEAVY_RAIN
      case 5 => STORM
      case _ => throw new IllegalArgumentException
    }
  }
}

object SessionType {
  import Packet.PacketParseable
  sealed trait SessionType
  case object UNKNOWN extends SessionType
  case object P1 extends SessionType
  case object P2 extends SessionType
  case object P3 extends SessionType
  case object SHORT_P extends SessionType
  case object Q1 extends SessionType
  case object Q2 extends SessionType
  case object Q3 extends SessionType
  case object SHORT_Q extends SessionType
  case object OSQ extends SessionType
  case object R extends SessionType
  case object R2 extends SessionType
  case object TIME_TRIAL extends SessionType

  def apply(parser: PacketParseable): SessionType = apply(parser.readUInt8)
  def apply(sessionType: Int): SessionType = {
    sessionType match {
      case 0  => UNKNOWN
      case 1  => P1
      case 2  => P2
      case 3  => P3
      case 4  => SHORT_P
      case 5  => Q1
      case 6  => Q2
      case 7  => Q3
      case 8  => SHORT_Q
      case 9  => OSQ
      case 10 => R
      case 11 => R2
      case 12 => TIME_TRIAL
      case _  => throw new IllegalArgumentException
    }
  }
}

object Era {
  import Packet.PacketParseable
  sealed trait Era
  case object MODERN extends Era
  case object CLASSIC extends Era

  def apply(parser: PacketParseable): Era = apply(parser.readUInt8)
  def apply(era: Int): Era = {
    era match {
      case 0 => MODERN
      case 1 => CLASSIC
      case _ => throw new IllegalArgumentException
    }
  }
}

object SafetyCarStatus {
  import Packet.PacketParseable
  sealed trait SafetyCarStatus
  case object NO_SAFETY_CAR extends SafetyCarStatus
  case object FULL_SAFETY_CAR extends SafetyCarStatus
  case object VIRTUAL_SAFETY_CAR extends SafetyCarStatus

  def apply(parser: PacketParseable): SafetyCarStatus = apply(parser.readUInt8)
  def apply(safetyCarStatus: Int): SafetyCarStatus = {
    safetyCarStatus match {
      case 0 => NO_SAFETY_CAR
      case 1 => FULL_SAFETY_CAR
      case 2 => VIRTUAL_SAFETY_CAR
      case _ => throw new IllegalArgumentException
    }
  }
}

object PacketLapData {
  import Packet.PacketParseable

  def apply(header: PacketHeader, parser: PacketParseable): PacketLapData = {
    val lapData = Vector.fill(20)(LapData(parser))
    PacketLapData(header, lapData)
  }
}

case class PacketLapData(
    header: PacketHeader,
    lapData: Vector[LapData]
) extends Packet(header)

object LapData {
  import Packet.PacketParseable
  def apply(parser: PacketParseable): LapData = {
    val lastLapTime = parser.readFloat
    val currentLapTime = parser.readFloat
    val bestLapTime = parser.readFloat
    val sector1Time = parser.readFloat
    val sector2Time = parser.readFloat
    val lapDistance = parser.readFloat
    val totalDistance = parser.readFloat
    val safetyCarDelta = parser.readFloat
    val carPosition = parser.readUInt8
    val currentLapNum = parser.readUInt8
    val pitStatus = PitStatus(parser)
    val sector = Sector(parser)
    val currentLapInvalid = parser.readBoolean
    val penalties = parser.readUInt8
    val gridPosition = parser.readUInt8
    val driverStatus = DriverStatus(parser)
    val resultStatus = ResultStatus(parser)
    LapData(
      lastLapTime,
      currentLapTime,
      bestLapTime,
      sector1Time,
      sector2Time,
      lapDistance,
      totalDistance,
      safetyCarDelta,
      carPosition,
      currentLapNum,
      pitStatus,
      sector,
      currentLapInvalid,
      penalties,
      gridPosition,
      driverStatus,
      resultStatus
    )
  }
}

case class LapData(
    lastLapTime: Double,
    currentLapTime: Double,
    bestLapTime: Double,
    sector1Time: Double,
    sector2Time: Double,
    lapDistance: Double,
    totalDistance: Double,
    safetyCarDelta: Double,
    carPosition: Int,
    currentLapNum: Int,
    pitStatus: PitStatus.PitStatus,
    sector: Sector.Sector,
    currentLapInvalid: Boolean,
    penalties: Int,
    gridPosition: Int,
    driverStatus: DriverStatus.DriverStatus,
    resultStatus: ResultStatus.ResultStatus
)

object PitStatus {
  import Packet.PacketParseable
  sealed trait PitStatus
  case object NONE extends PitStatus
  case object PITTING extends PitStatus
  case object IN_PIT_AREA extends PitStatus

  def apply(parser: PacketParseable): PitStatus = apply(parser.readUInt8)
  def apply(pitStatus: Int): PitStatus = {
    pitStatus match {
      case 0 => NONE
      case 1 => PITTING
      case 2 => IN_PIT_AREA
      case _ => throw new IllegalArgumentException
    }
  }
}

object Sector {
  import Packet.PacketParseable
  sealed trait Sector
  case object SECTOR_1 extends Sector
  case object SECTOR_2 extends Sector
  case object SECTOR_3 extends Sector

  def apply(parser: PacketParseable): Sector = apply(parser.readUInt8)
  def apply(sector: Int): Sector = {
    sector match {
      case 0 => SECTOR_1
      case 1 => SECTOR_2
      case 2 => SECTOR_3
      case _ => throw new IllegalArgumentException
    }
  }

  def toInt(sector: Sector): Int = sector match {
    case SECTOR_1 => 1
    case SECTOR_2 => 2
    case SECTOR_3 => 3
  }
}

object DriverStatus {
  import Packet.PacketParseable
  sealed trait DriverStatus
  case object IN_GARAGE extends DriverStatus
  case object FLYING_LAP extends DriverStatus
  case object IN_LAP extends DriverStatus
  case object OUT_LAP extends DriverStatus
  case object ON_TRACK extends DriverStatus

  def apply(parser: PacketParseable): DriverStatus = apply(parser.readUInt8)
  def apply(driverStatus: Int): DriverStatus = {
    driverStatus match {
      case 0 => IN_GARAGE
      case 1 => FLYING_LAP
      case 2 => IN_LAP
      case 3 => OUT_LAP
      case 4 => ON_TRACK
      case _ => throw new IllegalArgumentException
    }
  }
}

object ResultStatus {
  import Packet.PacketParseable
  sealed trait ResultStatus
  case object INVALID extends ResultStatus
  case object INACTIVE extends ResultStatus
  case object ACTIVE extends ResultStatus
  case object FINISHED extends ResultStatus
  case object DISQUALIFIED extends ResultStatus
  case object NOT_CLASSIFIED extends ResultStatus
  case object RETIRED extends ResultStatus

  def apply(parser: PacketParseable): ResultStatus = apply(parser.readUInt8)
  def apply(resultStatus: Int): ResultStatus = {
    resultStatus match {
      case 0 => INVALID
      case 1 => INACTIVE
      case 2 => ACTIVE
      case 3 => FINISHED
      case 4 => DISQUALIFIED
      case 5 => NOT_CLASSIFIED
      case 6 => RETIRED
      case _ => throw new IllegalArgumentException
    }
  }
}

object PacketEventData {
  import Packet.PacketParseable

  def apply(header: PacketHeader, parser: PacketParseable): PacketEventData = {
    val eventStringCode = (1 to 4).map(_ => parser.read1ByteChar).mkString
    PacketEventData(header, Event(parser))
  }
}

case class PacketEventData(
    header: PacketHeader,
    eventStringCode: Event.Event
) extends Packet(header)

object Event {
  import Packet.PacketParseable
  sealed trait Event
  case object SESSION_STARTED extends Event
  case object SESSION_ENDED extends Event

  def apply(parser: PacketParseable): Event = {
    val eventStringCode = (1 to 4).map(_ => parser.read1ByteChar).mkString
    apply(eventStringCode)
  }

  def apply(event: String): Event = {
    event match {
      case "SSTA" => SESSION_STARTED
      case "SEND" => SESSION_ENDED
      case _      => throw new IllegalArgumentException
    }
  }
}

object ParticipantData {
  import Packet.PacketParseable

  def apply(parser: PacketParseable): ParticipantData = {
    val aiControlled = parser.readBoolean
    val driverId = parser.readUInt8
    val teamId = parser.readUInt8
    val raceNumber = parser.readUInt8
    val nationality = parser.readUInt8
    val name = parser.readString(48)
    ParticipantData(
      aiControlled,
      driverId,
      teamId,
      raceNumber,
      nationality,
      name
    )
  }
}

case class ParticipantData(
    aiControlled: Boolean,
    driverId: Int,
    teamId: Int,
    raceNumber: Int,
    nationality: Int,
    name: String
)

object PacketParticipantsData {
  import Packet.PacketParseable

  def apply(
      header: PacketHeader,
      parser: PacketParseable
  ): PacketParticipantsData = {
    val numCars = parser.readUInt8
    val participants = Vector.fill(20)(ParticipantData(parser))
    PacketParticipantsData(
      header,
      numCars,
      participants
    )
  }
}

case class PacketParticipantsData(
    header: PacketHeader,
    numCars: Int,
    participants: Vector[ParticipantData]
) extends Packet(header)

object CarSetupData {
  import Packet.PacketParseable

  def apply(parser: PacketParseable): CarSetupData = {
    val frontWing = parser.readUInt8
    val rearWing = parser.readUInt8
    val onThrottle = parser.readUInt8
    val offThrottle = parser.readUInt8
    val frontCamber = parser.readFloat
    val rearCamber = parser.readFloat
    val frontToe = parser.readFloat
    val rearToe = parser.readFloat
    val frontSuspension = parser.readUInt8
    val rearSuspension = parser.readUInt8
    val frontAntiRollBar = parser.readUInt8
    val rearAntiRollBar = parser.readUInt8
    val frotSuspensionHeight = parser.readUInt8
    val rearSuspensionHeight = parser.readUInt8
    val brakePressure = parser.readUInt8
    val brakeBias = parser.readUInt8
    val frontTyrePressure = parser.readFloat
    val rearTyrePressure = parser.readFloat
    val ballast = parser.readUInt8
    val fuelLoad = parser.readFloat
    CarSetupData(
      frontWing,
      rearWing,
      onThrottle,
      offThrottle,
      frontCamber,
      rearCamber,
      frontToe,
      rearToe,
      frontSuspension,
      rearSuspension,
      frontAntiRollBar,
      rearAntiRollBar,
      frotSuspensionHeight,
      rearSuspensionHeight,
      brakePressure,
      brakeBias,
      frontTyrePressure,
      rearTyrePressure,
      ballast,
      fuelLoad
    )
  }
}

case class CarSetupData(
    frontWing: Int,
    rearWing: Int,
    onThrottle: Int,
    offThrottle: Int,
    frontCamber: Double,
    rearCamber: Double,
    frontToe: Double,
    rearToe: Double,
    frontSuspension: Int,
    rearSuspension: Int,
    frontAntiRollBar: Int,
    rearAntiRollBar: Int,
    frotSuspensionHeight: Int,
    rearSuspensionHeight: Int,
    brakePressure: Int,
    brakeBias: Int,
    frontTyrePressure: Double,
    rearTyrePressure: Double,
    ballast: Int,
    fuelLoad: Double
)

object PacketCarSetupData {
  import Packet.PacketParseable

  def apply(
      header: PacketHeader,
      parser: PacketParseable
  ): PacketCarSetupData = {
    val carSetups = Vector.fill(20)(CarSetupData(parser))
    PacketCarSetupData(header, carSetups)
  }
}

case class PacketCarSetupData(
    header: PacketHeader,
    carSetups: Vector[CarSetupData]
) extends Packet(header)

object CarTelemetryData {
  import Packet.PacketParseable

  def apply(parser: PacketParseable): CarTelemetryData = {
    val speed = parser.readUInt16
    val throttle = parser.readUInt8
    val steer = parser.readInt8
    val brake = parser.readUInt8
    val clutch = parser.readUInt8
    val gear = parser.readInt8
    val engineRPM = parser.readUInt16
    val drs = parser.readBoolean
    val revLightsPercent = parser.readUInt8
    val brakesTemperature = WheelData(parser.readUInt16)
    val tyresSurfaceTemperature = WheelData(parser.readUInt16)
    val tyresInnerTemperature = WheelData(parser.readUInt16)
    val engineTemperature = parser.readUInt16
    val tyresPressure = WheelData(parser.readFloat)
    CarTelemetryData(
      speed,
      throttle,
      steer,
      brake,
      clutch,
      gear,
      engineRPM,
      drs,
      revLightsPercent,
      brakesTemperature,
      tyresSurfaceTemperature,
      tyresInnerTemperature,
      engineTemperature,
      tyresPressure
    )
  }
}

case class CarTelemetryData(
    speed: Int,
    throttle: Int,
    steer: Int,
    brake: Int,
    clutch: Int,
    gear: Int,
    engineRPM: Int,
    drs: Boolean,
    revLightsPercent: Int,
    brakesTemperature: WheelData[Int],
    tyresSurfaceTemperature: WheelData[Int],
    tyresInnerTemperature: WheelData[Int],
    engineTemperature: Int,
    tyresPressure: WheelData[Double]
)

case class WheelData[T](
    rearLeft: T,
    rearRight: T,
    frontLeft: T,
    frontRight: T
)

object WheelData {
  def apply[T](generator: => T): WheelData[T] = {
    val rearLeft = generator
    val rearRight = generator
    val frontLeft = generator
    val frontRight = generator
    WheelData(rearLeft, rearRight, rearLeft, rearRight)
  }
}

case class PacketCarTelemetryData(
    header: PacketHeader,
    carTelemetryData: Vector[CarTelemetryData],
    buttonStatus: ButtonStatus
) extends Packet(header)

object PacketCarTelemetryData {
  import Packet.PacketParseable

  def apply(
      header: PacketHeader,
      parser: PacketParseable
  ): PacketCarTelemetryData = {
    val carTelemetryData = Vector.fill(20)(CarTelemetryData(parser))
    val buttonStatus = ButtonStatus(parser)
    PacketCarTelemetryData(header, carTelemetryData, buttonStatus)
  }
}

case class ButtonStatus(
    crossAPressed: Boolean,
    triangleYPressed: Boolean,
    circleBPressed: Boolean,
    squareXPressed: Boolean,
    dpadLeftPressed: Boolean,
    dpadRightPressed: Boolean,
    dpadUpPressed: Boolean,
    dpadDownPressed: Boolean,
    optionsMenuPressed: Boolean,
    l1LBPressed: Boolean,
    r1RBPressed: Boolean,
    l2LTPressed: Boolean,
    r2RTPressed: Boolean,
    leftStickPressed: Boolean,
    rightStickPressed: Boolean
)

object ButtonStatus {
  import Packet.PacketParseable

  val CROSS_OR_A = 0x0001
  val TRIANGLE_OR_Y = 0x0002
  val CIRCLE_OR_B = 0x0004
  val SQUARE_OR_X = 0x0008
  val DPAD_LEFT = 0x0010
  val DPAD_RIGHT = 0x0020
  val DPAD_UP = 0x0040
  val DPAD_DOWN = 0x0080
  val OPTIONS_OR_MENU = 0x0100
  val L1_OR_LB = 0x0200
  val R1_OR_RB = 0x0400
  val L2_OR_LT = 0x0800
  val R2_OR_RT = 0x1000
  val LEFT_STICK_CLICK = 0x2000
  val RIGHT_STICK_CLICK = 0x4000

  def apply(parser: PacketParseable): ButtonStatus = {
    val data = parser.readUInt32
    val crossAPressed = (data & CROSS_OR_A) != 0
    val triangleYPressed = (data & TRIANGLE_OR_Y) != 0
    val circleBPressed = (data & CIRCLE_OR_B) != 0
    val squareXPressed = (data & SQUARE_OR_X) != 0
    val dpadLeftPressed = (data & DPAD_LEFT) != 0
    val dpadRightPressed = (data & DPAD_RIGHT) != 0
    val dpadUpPressed = (data & DPAD_UP) != 0
    val dpadDownPressed = (data & DPAD_DOWN) != 0
    val optionsMenuPressed = (data & OPTIONS_OR_MENU) != 0
    val l1LBPressed = (data & L1_OR_LB) != 0
    val r1RBPressed = (data & R1_OR_RB) != 0
    val l2LTPressed = (data & L2_OR_LT) != 0
    val r2RTPressed = (data & R2_OR_RT) != 0
    val leftStickPressed = (data & LEFT_STICK_CLICK) != 0
    val rightStickPressed = (data & RIGHT_STICK_CLICK) != 0
    ButtonStatus(
      crossAPressed,
      triangleYPressed,
      circleBPressed,
      squareXPressed,
      dpadLeftPressed,
      dpadRightPressed,
      dpadUpPressed,
      dpadDownPressed,
      optionsMenuPressed,
      l1LBPressed,
      r1RBPressed,
      l2LTPressed,
      r2RTPressed,
      leftStickPressed,
      rightStickPressed
    )
  }
}

case class CarStatusData(
    tractionControl: TractionControl.TractionControl,
    antiLockBrakes: Boolean,
    fuelMix: FuelMix.FuelMix,
    frontBrakeBias: Int,
    pitLimiterStatus: Boolean,
    fuelInTank: Double,
    fuelCapacity: Double,
    maxRPM: Int,
    idleRPM: Int,
    maxGears: Int,
    drsAllowed: DrsAllowed.DrsAllowed,
    tyresWear: WheelData[Int],
    tyreCompound: Int,
    tyresDamage: WheelData[Int],
    frontLeftWingDamage: Int,
    frontRightWingDamage: Int,
    rearWingDamage: Int,
    engineDamage: Int,
    gearBoxDamage: Int,
    exhaustDamage: Int,
    vehicleFiaFlags: VehicleFiaFlags.VehicleFiaFlags,
    ersStoreEnergy: Double,
    ersDeployMode: ErsDeployMode.ErsDeployMode,
    ersHarvestedThisLapMGUK: Double,
    ersHarvestedThisLapMGUH: Double,
    ersDeployedThisLap: Double
)

object CarStatusData {
  import Packet.PacketParseable

  def apply(parser: PacketParseable): CarStatusData = {
    val tractionControl = TractionControl(parser)
    val antiLockBrakes = parser.readBoolean
    val fuelMix = FuelMix(parser.readUInt8)
    val frontBrakeBias = parser.readUInt8
    val pitLimiterStatus = parser.readBoolean
    val fuelInTank = parser.readFloat
    val fuelCapacity = parser.readFloat
    val maxRPM = parser.readUInt16
    val idleRPM = parser.readUInt16
    val maxGears = parser.readUInt8
    val drsAllowed = DrsAllowed(parser)
    val tyresWear = WheelData(parser.readUInt8)
    val tyreCompound = parser.readUInt8
    val tyresDamage = WheelData(parser.readUInt8)
    val frontLeftWingDamage = parser.readUInt8
    val frontRightWingDamage = parser.readUInt8
    val rearWingDamage = parser.readUInt8
    val engineDamage = parser.readUInt8
    val gearBoxDamage = parser.readUInt8
    val exhaustDamage = parser.readUInt8
    val vehicleFiaFlags = VehicleFiaFlags(parser)
    val ersStoreEnergy = parser.readFloat
    val ersDeployMode = ErsDeployMode(parser)
    val ersHarvestedThisLapMGUK = parser.readFloat
    val ersHarvestedThisLapMGUH = parser.readFloat
    val ersDeployedThisLap = parser.readFloat
    CarStatusData(
      tractionControl,
      antiLockBrakes,
      fuelMix,
      frontBrakeBias,
      pitLimiterStatus,
      fuelInTank,
      fuelCapacity,
      maxRPM,
      idleRPM,
      maxGears,
      drsAllowed,
      tyresWear,
      tyreCompound,
      tyresDamage,
      frontLeftWingDamage,
      frontRightWingDamage,
      rearWingDamage,
      engineDamage,
      gearBoxDamage,
      exhaustDamage,
      vehicleFiaFlags,
      ersStoreEnergy,
      ersDeployMode,
      ersHarvestedThisLapMGUK,
      ersHarvestedThisLapMGUH,
      ersDeployedThisLap
    )
  }
}

case class PacketCarStatusData(
    header: PacketHeader,
    carStatusData: Vector[CarStatusData]
) extends Packet(header)

object PacketCarStatusData {
  import Packet.PacketParseable

  def apply(
      header: PacketHeader,
      parser: PacketParseable
  ): PacketCarStatusData = {
    val carStatusData = Vector.fill(20)(CarStatusData(parser))
    PacketCarStatusData(header, carStatusData)
  }
}

object TractionControl {
  import Packet.PacketParseable
  sealed trait TractionControl
  case object OFF extends TractionControl
  case object MEDIUM extends TractionControl
  case object HIGH extends TractionControl

  def apply(parser: PacketParseable): TractionControl = apply(parser.readUInt8)
  def apply(tractionControl: Int): TractionControl = {
    tractionControl match {
      case 0 => OFF
      case 1 => MEDIUM
      case 2 => HIGH
      case _ => throw new IllegalArgumentException
    }
  }
}

object FuelMix {
  import Packet.PacketParseable
  sealed trait FuelMix
  case object LEAN extends FuelMix
  case object STANDARD extends FuelMix
  case object RICH extends FuelMix
  case object MAX extends FuelMix

  def apply(parser: PacketParseable): FuelMix = apply(parser.readUInt8)
  def apply(fuelMix: Int): FuelMix = {
    fuelMix match {
      case 0 => LEAN
      case 1 => STANDARD
      case 2 => RICH
      case 3 => MAX
      case _ => throw new IllegalArgumentException
    }
  }
}

object DrsAllowed {
  import Packet.PacketParseable
  sealed trait DrsAllowed
  case object NOT_ALLOWED extends DrsAllowed
  case object ALLOWED extends DrsAllowed
  case object UNKNOWN extends DrsAllowed

  def apply(parser: PacketParseable): DrsAllowed = apply(parser.readUInt8)
  def apply(drsAllowed: Int): DrsAllowed = {
    drsAllowed match {
      case 0  => NOT_ALLOWED
      case 1  => ALLOWED
      case -1 => UNKNOWN
      case _  => throw new IllegalArgumentException
    }
  }
}

object VehicleFiaFlags {
  import Packet.PacketParseable
  sealed trait VehicleFiaFlags
  case object UNKNOWN extends VehicleFiaFlags
  case object NONE extends VehicleFiaFlags
  case object GREEN extends VehicleFiaFlags
  case object BLUE extends VehicleFiaFlags
  case object YELLOW extends VehicleFiaFlags
  case object RED extends VehicleFiaFlags

  def apply(parser: PacketParseable): VehicleFiaFlags = apply(parser.readInt8)
  def apply(vehicleFiaFlags: Int): VehicleFiaFlags = {
    vehicleFiaFlags match {
      case -1 => UNKNOWN
      case 0  => NONE
      case 1  => GREEN
      case 2  => BLUE
      case 3  => YELLOW
      case 4  => RED
      case _  => throw new IllegalArgumentException
    }
  }
}

object ErsDeployMode {
  import Packet.PacketParseable
  sealed trait ErsDeployMode
  case object NONE extends ErsDeployMode
  case object LOW extends ErsDeployMode
  case object MEDIUM extends ErsDeployMode
  case object HIGH extends ErsDeployMode
  case object OVERTAKE extends ErsDeployMode
  case object HOTLAP extends ErsDeployMode

  def apply(parser: PacketParseable): ErsDeployMode = apply(parser.readUInt8)
  def apply(ersDeployMode: Int): ErsDeployMode = {
    ersDeployMode match {
      case 0 => NONE
      case 1 => LOW
      case 2 => MEDIUM
      case 3 => HIGH
      case 4 => OVERTAKE
      case 5 => HOTLAP
      case _ => throw new IllegalArgumentException
    }
  }
}
