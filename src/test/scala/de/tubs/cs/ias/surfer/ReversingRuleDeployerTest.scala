package de.tubs.cs.ias.surfer

import de.tubs.cs.ias.surfer.reversing.ReversingRuleDeployer
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ReversingRuleDeployerTest extends AnyWordSpec with Matchers {


  "ReversingRuleDeployer" should {
    "have at least 1 rule" in {
      //println(ReversingRuleDeployer.getRules)
      ReversingRuleDeployer.getRules.keySet.nonEmpty shouldBe true
    }

  }

}
