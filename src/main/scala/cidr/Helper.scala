package cidr


object CidrHelper {
  val ip = raw"([\d]{1,3})\.([\d]{1,3})\.([\d]{1,3})\.([\d]{1,3})".r
  val p = raw"([\d]{1,3}\.[\d]{1,3}\.[\d]{1,3}\.[\d]{1,3})/([\d]{1,2})"
  val pr = p.r

  def isValidCidr(cidr: String): Boolean = cidr match {
    case pr(subnet, maskbits) => true
    case _ => false
  }

  def apply(cidr: String) = {
    assert(isValidCidr(cidr))
    cidr match {
      case pr(subnet, bits) => new CidrHelper(subnet, bits.toInt)
    }
  }

  def asNum(s: String): Long = s match {
    case ip(a, b, c, d) => ((a.toLong << 24) + (b.toLong << 16) + (c.toLong << 8) + d.toLong)
  }

  def format(ip: Long): String = {
    val a = ip >> 24
    val b = ip >> 16 & 0x000000ff
    val c = ip >> 8 & 0x000000ff
    val d = ip & 0x000000ff
    s"$a.$b.$c.$d"
  }

}

class CidrHelper(val address: String, val maskbits: Int) {

  override def toString() = s"n=${network}, mask=$maskbits, nm=${subnetMask}"


  def numHosts: Long = math.pow(2, 32 - maskbits).toLong

  def toHexString: String = CidrHelper.asNum(address).toHexString

  def asNum: Long = CidrHelper.asNum(address)

  def network: String = CidrHelper.format(asNum & ~hostBits)
  def networkNum: Long = asNum & ~hostBits

  def hostBits: Long = (math.pow(2, 32 - maskbits) - 1).toLong

  def subnetMask: String = CidrHelper.format(networkBits)

  def networkBits: Long = (math.pow(2, 32) - 1).toLong ^ hostBits

  def lowest: String = CidrHelper.format(networkNum + 1)

  def highest: String = CidrHelper.format(networkNum + hostBits)

  def report: Unit = {
    println(s"cidr\t${address}/${maskbits}")
    println(s"hosts\t$numHosts")
    println(s"network\t$network")
    println(s"submask\t$subnetMask")
    println(s"lowest\t$lowest")
    println(s"highest\t$highest")
  }


}
