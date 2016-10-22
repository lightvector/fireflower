package fireflower

/** Rand
  * A simple class for random number generation, independent of the scala or java built-in random. High-period and high-quality.
  */
object Rand {
  def apply(): Rand = new Rand(RandUtils.makeSeedsFromTime("Rand",2))
  def apply(seed: Long) = new Rand(Array(seed))
  def apply(seeds: Array[Long]) = new Rand(seeds)
}

class Rand private (seeds: Array[Long]) {
  val seed = seeds.map(_.toString).mkString("|")

  val xoro = new Xoroshiro128plus(seed)
  val xors = new XorShift1024Mult(seed)
  val pcg = new PCG32(seed)

  def nextLong(): Long = (xoro.next() ^ xors.next()) + pcg.next()
  def nextInt(): Int = nextLong.toInt
  def nextInt(n: Int): Int = {
    if(n <= 0) throw new Exception("Rand.nextInt(n): non-positive n: " + n)
    var x = 0
    var y = 0
    do {
      x = nextInt() & 0x7FFFFFFF
      y = x % n
    } while (x - y + (n - 1) < 0);
    y
  }

  def nextDouble(): Double = (nextLong() & 0x1FFFFFFFFFFFFFL) / (1L << 53).toDouble

  def shuffle[T](arr: Array[T]): Unit = {
    for(i <- 1 to (arr.length-1)) {
      val j = nextInt(i+1)
      val tmp = arr(i)
      arr(i) = arr(j)
      arr(j)= tmp
    }
  }
}


//--------------------------------------------------------------------------------------
//Implementation of underlying random number generators
//--------------------------------------------------------------------------------------

trait LongGen {
  def next(): Long
}

//xoroshiro128plus from http://xoroshiro.di.unimi.it/
//Initial values must be not both zero
//Period = 2^128 - 1
class Xoroshiro128plus(initX0: Long, initX1: Long) extends LongGen {

  private def this(longs: Array[Long]) = this(longs(0),longs(1))
  def this(seed: String) = this(RandUtils.makeNonzeroSeedsFromSeed("Xoroshiro128plus",seed,2))

  private var x0: Long = initX0
  private var x1: Long = initX1

  def rotateLeft(x: Long, i: Int): Long = {
    (x << i) | (x >>> (64 - i));
  }

  def next(): Long = {
    var s0 = x0
    var s1 = x1

    s1 ^= s0
    x0 = rotateLeft(s0,55) ^ s1 ^ (s1 << 14)
    x1 = rotateLeft(s1,36)

    x0 + x1
  }
}

//xorshift1024* from http://xoroshiro.di.unimi.it/
//Not all values should be zero
//Period = 2^1024 - 1
class XorShift1024Mult(initS: Array[Long]) extends LongGen {
  val len: Int = 16
  if(initS.length != len)
    throw new Exception("XorShift1024Mult initialized with array initS of length != 16")

  def this(seed: String) = this(RandUtils.makeNonzeroSeedsFromSeed("XorShift1024Mult",seed,16))

  private var s: Array[Long] = initS.clone
  private var idx: Int = 0

  def next(): Long = {
    val s0 = s(idx)
    idx = (idx + 1) % len
    var s1 = s(idx)
    s1 ^= s1 << 31
    s(idx) = s1 ^ s0 ^ (s1 >>> 11) ^ (s0 >>> 30)

    s(idx) * 1181783497276652981L
  }
}

//PCG Generator from http://www.pcg-random.org/
//Period = 2^64
class PCG32(initS: Long) extends LongGen {
  def this(seed: String) = this(RandUtils.makeNonzeroSeedsFromSeed("PCG32",seed,1)(0))

  private var s: Long = initS

  def nextInt(): Int = {
    s = s * 6364136223846793005L + 1442695040888963407L
    val x: Int = (((s >>> 18) ^ s) >>> 27).toInt
    val rot: Int = (s >>> 59).toInt
    (x >>> rot) | (x << (32-rot))
  }

  def next(): Long = {
    val low = nextInt()
    val high = nextInt()
    (low.toLong & 0xFFFFFFFFL) ^ (high.toLong << 32)
  }
}

object RandUtils {
  def sha256Bytes(s: String): Array[Byte] = {
    java.security.MessageDigest.getInstance("SHA-256").digest(s.getBytes("UTF-8"))
  }
  def sha256(s: String): String = {
    javax.xml.bind.DatatypeConverter.printHexBinary(sha256Bytes(s))
  }

  def sha256Long(s: String): Long = {
    bytesToLongs(sha256Bytes("sha256Long" + sha256(s)))(0)
  }

  def bytesToLongs(bytes: Array[Byte]): Array[Long] = {
    val longs = (0 to (bytes.length / 8 - 1)).map { i =>
      ((bytes(i*8+0).toLong & 0xFFL) <<  0) |
      ((bytes(i*8+1).toLong & 0xFFL) <<  8) |
      ((bytes(i*8+2).toLong & 0xFFL) << 16) |
      ((bytes(i*8+3).toLong & 0xFFL) << 24) |
      ((bytes(i*8+4).toLong & 0xFFL) << 32) |
      ((bytes(i*8+5).toLong & 0xFFL) << 40) |
      ((bytes(i*8+6).toLong & 0xFFL) << 48) |
      ((bytes(i*8+7).toLong & 0xFFL) << 56)
    }
    longs.toArray
  }

  private val counter: java.util.concurrent.atomic.AtomicLong = new java.util.concurrent.atomic.AtomicLong()
  def makeSeedsFromTime(salt: String, num: Int): Array[Long] = {
    val len = (num + 3) / 4 //divide rounded up
    val hashsalt = sha256(salt)
    val bytes: Seq[Byte] = (0 to (len-1)).flatMap { i =>
      val hashStr = "fromtime:" + i + ":" + counter.incrementAndGet() + ":" + System.nanoTime() + ":" + hashsalt
      sha256Bytes(hashStr)
    }
    bytesToLongs(bytes.toArray).slice(0,num)
  }

  def makeNonzeroSeedsFromSeed(salt: String, seed: String, num: Int): Array[Long] = {
    val len = (num + 3) / 4 //divide rounded up
    val hashsalt = sha256(salt)

    def loop(tries: Int): Array[Long] = {
      val bytes: Seq[Byte] = (0 to (len-1)).flatMap { i =>
        val hashStr = "fromseed:" + tries + ":" + i + ":" + hashsalt + ":" + seed
        sha256Bytes(hashStr)
      }
      val result = bytesToLongs(bytes.toArray).slice(0,num)
      if(result.exists { x => x != 0 })
        result
      else
        loop(tries+1)
    }
    loop(0)
  }
}


object RandTest {
  def test(): Unit = {
    val xoro = new Xoroshiro128plus(12345,67890)

    val xorm = new XorShift1024Mult(Array(
      -3298461724703502529L,
      3601266951833665894L,
      -1517299006908105192L,
      -4970805572606481462L,
      -2733606064565797204L,
      4148159782736716337L,
      -2411149239708519475L,
      5555591070439871209L,
      4101130512537511022L,
      -5625196436916664707L,
      9050874162294428797L,
      6187760405891629771L,
      -8393097797189788308L,
      2219782655280501359L,
      3719698449347562208L,
      5421263376768154227L
    ))

    val pcg = new PCG32(123)

    //First value should be 1c9390b04e43f913
    //Last value should be  a50a8874ba8ba1d2
    for(i <- 1 to 150) println("Xoroshiro128plus: " + i + " " + xoro.next().toHexString)

    //First value should be 749746d1c27d2463
    //Last value should be  f9add2e499dabbfc
    for(i <- 1 to 150) println("Xorshift1024mult: " + i + " " + xorm.next().toHexString)

    //First value should be 65fdd305b3766cbd
    //Last value should be  e4aef6a6d6858d66
    for(i <- 1 to 150) println("PCG32: " + i + " " + pcg.next().toHexString)

    //Should be: EF537F25C895BFA782526529A9B63D97AA631564D5D789C2B765448C8635FB6C
    println("SHA256: " + RandUtils.sha256("The quick brown fox jumps over the lazy dog."))
    //Should be: a7bf95c8257f53ef
    println("SHA256Longs(0): " + RandUtils.bytesToLongs(RandUtils.sha256Bytes("The quick brown fox jumps over the lazy dog."))(0).toHexString)

    println("Some random numbers:")
    val rand: Rand = Rand()
    for(i <- 1 to 10)
      println(rand.nextInt() + " " + rand.nextLong() + " " + rand.nextInt(10) + " " + rand.nextDouble())
  }
}
