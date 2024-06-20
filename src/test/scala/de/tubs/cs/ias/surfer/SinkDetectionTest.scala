package de.tubs.cs.ias.surfer

import de.tubs.cs.ias.surfer.implicits.OneableIterableOnce
import de.tubs.cs.ias.surfer.reversing.util.bytecodeFunctionCalls
import de.tubs.cs.ias.surfer.sinks.{CurlSetOptSinkDefinition, SinkDefinition}
import io.joern.bytecode.parser.PHPVersion
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.annotation.nowarn

/**
  * Some of these tests are near identical to the SSRFDetectorTests.
  * However, I want to test the SinkDetection independently from the rest of the pipeline.
  */
@nowarn("msg=possible missing interpolator")
class SinkDetectionTest extends AnyWordSpec with Matchers {
  implicit val version: PHPVersion.Value = PHPVersion.V8
  "handle SEND_ARRAY" in new CpgFixture(
    """$args = [$_GET["x"]];
      |call_user_func_array('file_get_contents', $args);
      |""".stripMargin) {
    val sink: Call = cpg.call.nameExact("file_get_contents").one
    SinkDefinition.getDefinitions.filter(_.matches(sink)).one.getSinkName shouldBe "file_get_contents"
  }
  "no false positive if there is a namespaced collision" in new CpgFixture(
    """namespace namesp;
      |
      |function file_get_contents($x)
      |{
      |    echo $x;
      |}
      |echo file_get_contents($_GET["a"]);
      |""".stripMargin) {
    val calls: List[Call] = cpg.call.l
    //val call_names: List[(String, String)] = calls.map(c =>(c.name, c.code))
    val sinks: List[Call] = calls.filter(call => SinkDefinition.getDefinitions.exists(_.matches(call)))
    sinks shouldBe empty
  }
  "no false positive if there is an object-scope collision with sink" in new CpgFixture(
    """class A {
      |
      |static function file_get_contents($x)
      |{
      |    echo $x;
      |}
      |}
      |echo A::file_get_contents(($_GET["a"]));
      |""".stripMargin) {
    val calls: List[Call] = cpg.call.l
    val sinks: List[Call] = calls.filter(call => SinkDefinition.getDefinitions.exists(_.matches(call)))
    sinks shouldBe empty
  }
  "detect curl related ssrf" when {
    "curl_init" in new CpgFixture("""curl_exec(curl_init($_GET["a"]));""") {
      //new CpgDotFileCreator(cpg.graph).show()
      val sinks: List[Call] = cpg.call.filter(call => SinkDefinition.getDefinitions.exists(_.matches(call))).l
      sinks.length shouldBe 1
    }
    "curl_setopt with correct opt" in new CpgFixture(
      """$ch = curl_init();
        |curl_setopt($ch, CURLOPT_URL, $_GET["a"]);
        |curl_exec($ch);
        |curl_close($ch);
        |""".stripMargin) {
      val sinks: List[Call] = cpg.call.filter(call => SinkDefinition.getDefinitions.exists(_.matches(call))).l
      sinks.length shouldBe 1
    }
    "curl_setopt false-positive" in new CpgFixture(
      """$ch = curl_init("http://example.org");
        |curl_setopt($ch, CURLOPT_USERAGENT, $_GET["a"]);
        |curl_exec($ch);
        |curl_close($ch);
        |""".stripMargin) {
      val sinks: List[Call] = cpg.call.filter(call => SinkDefinition.getDefinitions.exists(_.matches(call))).l
      sinks shouldBe empty
    }
    "curl_set_opt dynamic false-positive" in new CpgFixture(
      """$conf = CURLOPT_USERAGENT;
        |$ch = curl_init("http://example.org");
        |curl_setopt($ch, $conf, $_GET["a"]);
        |curl_exec($ch);
        |curl_close($ch);
        |""".stripMargin) {
      val sinks: List[Call] = cpg.call.filter(call => SinkDefinition.getDefinitions.exists(_.matches(call))).l
      sinks should have length 1 // we overapprox this atm.
    }
    "curl_setopt dynamic" in new CpgFixture(
      """$conf = CURLOPT_URL;
        |$ch = curl_init("http://example.org");
        |curl_setopt($ch, $conf, $_GET["a"]);
        |curl_exec($ch);
        |curl_close($ch);
        |""".stripMargin) {
      val sinkdefs: Set[SinkDefinition] = SinkDefinition.getDefinitions
      val sinks: List[Call] = cpg.call.filter(call => sinkdefs.exists(_.matches(call))).l
      sinks.length shouldBe 1
      sinkdefs.collectFirst { case x: CurlSetOptSinkDefinition => x }.get.overapprox_counter shouldBe 1
    }
  }
  "sink with multiple params" when {
    "no false positive" in new CpgFixture("file_get_contents(\"safe\", $_GET[\"i\"]);") {
      val sinks: List[Call] = cpg.call.filter(call => SinkDefinition.getDefinitions.exists(_.matches(call))).l
      sinks shouldBe empty
    }
    "no false negative" in new CpgFixture("file_get_contents($_GET[\"i\"], \"save\");") {
      val sinks: List[Call] = cpg.call.filter(call => SinkDefinition.getDefinitions.exists(_.matches(call))).l
      sinks.length shouldBe 1
    }
  }

  "detect sink even if there is a namespace" when {
    "file_get_contents" in new CpgFixture(
      """
        |namespace test;
        |$a = $_GET["a"];
        |file_get_contents($a);
        |""".stripMargin) {
      val calls: List[Call] = cpg.call.l
      val sinks: List[Call] = calls.filter(call => SinkDefinition.getDefinitions.exists(_.matches(call)))
      sinks.length shouldBe 1
      sinks.head.name.endsWith("file_get_contents")
    }
    "curl_setopt" in new CpgFixture(
      """
        |namespace test;
        |
        |$a = $_GET["a"];
        |$ch = curl_init();
        |curl_setopt($ch, CURLOPT_URL, $a);
        |echo curl_exec($ch);
        |""".stripMargin) {
      val calls: List[Call] = cpg.call.l.filter(x => bytecodeFunctionCalls.contains(x.code))
      val sinks: List[Call] = calls.filter(call => SinkDefinition.getDefinitions.exists(_.matches(call)))
      sinks.length shouldBe 1
    }
  }
}
