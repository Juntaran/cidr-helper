package cidr

import org.scalatest.FlatSpec

class IpTests extends FlatSpec {

  "num hosts" should "be calculated from the subnet bits" in {
    assert(CidrHelper("10.10.2.0/31").numHosts == 2)
    assert(CidrHelper("10.10.2.0/28").numHosts == 16)
    assert(CidrHelper("10.10.2.0/24").numHosts == 256)
  }

  "a Cidr" should "be ctor'd by helper" in {
    val c1 = CidrHelper("192.168.100.14/22")
    assert(c1.maskbits == 22)
    assert(c1.address == "192.168.100.14")
    assert(c1.numHosts == 1024)
  }

  it should "compute correct subnet mask" in {
    assert(CidrHelper("192.168.100.14/22").subnetMask == "255.255.252.0")
    assert(CidrHelper("192.168.100.14/28").subnetMask == "255.255.255.240")
    assert(CidrHelper("192.168.100.14/16").subnetMask == "255.255.0.0")
    assert(CidrHelper("10.10.2.0/15").subnetMask == "255.254.0.0")
  }

  it should "use the mask to find the network" in {
    assert(CidrHelper("192.168.100.14/24").network == "192.168.100.0")
    assert(CidrHelper("192.168.100.14/22").network == "192.168.100.0")
    assert(CidrHelper("192.168.100.14/28").network == "192.168.100.0")
    assert(CidrHelper("192.168.100.14/16").network == "192.168.0.0")
  }

}
