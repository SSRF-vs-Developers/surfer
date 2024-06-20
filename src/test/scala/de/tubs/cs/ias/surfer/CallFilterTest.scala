package de.tubs.cs.ias.surfer

import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CallFilterTest extends AnyWordSpec with Matchers with PHPVersions{

    implicit val version : PHPVersion.Value = PHPVersion.V8
    s"getGlobals with PHP $version" should {
      "give me one _GET use" in new CpgFromCodeTestFixture(
        """$x = $_GET['test'];
          |echo $x;
          |""".stripMargin
      ) {
        CallFilter.getGlobalReads(cpg).length shouldBe 1
      }
      "give me two _GET use" in new CpgFromCodeTestFixture(
        """$x = $_GET['test'];
          |echo $x;
          |echo $_GET['other'];
          |""".stripMargin
      ) {
        CallFilter.getGlobalReads(cpg).length shouldBe 2
      }
      "give me one _GET one _POST use" in new CpgFromCodeTestFixture(
        """$x = $_GET['test'];
          |echo $x;
          |$y = $_POST['test'];
          |""".stripMargin
      ) {
        CallFilter.getGlobalReads(cpg).length shouldBe 2
      }
    }
}
