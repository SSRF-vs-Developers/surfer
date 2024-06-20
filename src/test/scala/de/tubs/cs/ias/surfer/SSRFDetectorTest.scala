package de.tubs.cs.ias.surfer

import de.tubs.cs.ias.cpg.dotFileExporter.DotFileCreatorExpansion.CpgDotFileCreator
import de.tubs.cs.ias.cpg.dotFileExporter.conf.ExportConfig
import de.tubs.cs.ias.surfer.implicits.UnwrappableCandidate
import io.joern.bytecode.Defines
import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.File
import scala.annotation.nowarn
import scala.io.Source

@nowarn("msg=possible missing interpolator")
class SSRFDetectorTest extends AnyWordSpec with Matchers with PHPVersions {


  implicit val config: ExportConfig = ExportConfig.apply(new File("export.json"))

  //for (v <- getPhpVersions) {

  implicit val version: PHPVersion.Value = PHPVersion.V8

    "not find anything only with print" in new CpgFromCodeTestFixture(
      """print("Hello World")""") {
      SSRFDetector.getCandidates(debug = false).unwrap shouldBe empty
    }
    "not find anything if there is no source" in new CpgFromCodeTestFixture(
      """print(file_get_contents("https://example.org"))""") {
      SSRFDetector.getCandidates(debug = false).unwrap shouldBe empty
    }
    "detect SSRF in trivial example" in new CpgFromCodeTestFixture(
      """$url = $_POST['url'];
        |$content;
        |if ($url) {
        |    print("Getting " . $url. 'for you..');
        |    $content = file_get_contents($url);
        |} else{
        |    $content = "Nothing to show right now.";
        |}
        |print($content."</body>");
        |""".stripMargin) {
      //new CpgDotFileCreator(cpg.graph).show()
      val res: List[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      // we only have a single candidate
      res.length shouldBe 1
      res.head.getSink.name shouldBe "file_get_contents"
      // we only have a single end node
      res.head.getSources.size shouldBe 1
      res.head.getSources.map(_.code) shouldBe Set("""FETCH_R (global) string("_POST")""")
    }
    "detect ssrf even if get is used twice" in new CpgFromCodeTestFixture(
      """$a = $_GET;
        |$b = $a["a"];
        |$c = $a[1];
        |file_get_contents($c);
        |""".stripMargin) {
      val res: List[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      res.length shouldBe 1
      res.head.getSink.name shouldBe "file_get_contents"
      res.head.getSources.size shouldBe 1
      res.head.getSources.map(_.code) shouldBe Set("""FETCH_R (global) string("_GET")""")
    }
    "work if there is an internal function call within" in new CpgFromCodeTestFixture(
      """$a = $_GET["a"];
        |file_get_contents(explode(",",$a)[0]);
        |""".stripMargin) {
      //new CpgDotFileCreator(cpg.graph).show()
      val res: List[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      res.length shouldBe 1
      res.head.getSink.name shouldBe "file_get_contents"
      // we have one SEND_VAR for a constant and one global access
      res.head.getSources.size shouldBe 2
      //res.head.showGraph()
      res.head.getSources.map(_.code) shouldBe Set("""FETCH_R (global) string("_GET")""", """SEND_VAL string(",") int(1)""")
    }
    """not provide a false negative for nested arrays""" in new CpgFromCodeTestFixture(
      """$a = array(
        |    array("https://example.org"),
        |    array($_GET["x"])
        |);
        |foreach ($a as $elem) {
        |    echo($elem);
        |}
        |file_get_contents($a[1]);
        |""".stripMargin) {
      val res: List[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      res.length shouldBe 1
      //todo: expand unit test
    }
    "work with foreach and if/else inside" in new CpgFromCodeTestFixture(
      """function r($s) {
        |   echo $s;
        |}
        |$a = array(
        |   array("https://example.com"),
        |   array($_GET['z'])
        |);
        |if(is_array($a)) {
        |   foreach($a as $elem) {
        |       r($elem);
        |   }
        |} else {
        |  file_get_contents($a);
        |}
        |""".stripMargin) {
      SSRFDetector.getCandidates(debug = false)._1 match {
        case Left(_) :: Nil => succeed
        case Right(x) :: Nil => fail(x.getMessage + "\n" + x.getStackTrace.mkString("\n"))
        case x => fail(s"unexpected number of results ${x.length}")
      }
    }
  "work through array merge" in new CpgFromCodeTestFixture(
    """$a = array('hi','harmless');
      |$b = array($_GET["x"]);
      |$c = array_merge($a,$b);
      |   file_get_contents($c[2]);
      |""".stripMargin) {
    val res: List[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
    res.length shouldBe 1
    res.head.getSinkStrings shouldBe Set("<G:_GET>[x]")
  }
  "work through array merge and for" in new CpgFromCodeTestFixture(
      """$a = array('hi','harmless');
        |$b = array($_GET["array_merge"]);
        |$c = array_merge($a,$b);
        |for ($i = 0; $i < count($c); $i += 1) {
        |   file_get_contents($c[$i]);
        |}
        |""".stripMargin) {
      val res: List[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      res.length shouldBe 1
    }
    "work through QM_ASSIGN" in new CpgFromCodeTestFixture(
      """$a = rand(0,1) == 1 ? $_GET['a'] : 's';
        |file_get_contents($a);
        |""".stripMargin) {
      val res: List[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      res.length shouldBe 1
    }
    "work through ternary operator" in new CpgFromCodeTestFixture(
      """$a = $_GET['a'];
        |rand(0,1) == 1 ? file_get_contents($a) : print('s');
        |""".stripMargin) {
      val res: List[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      res.length shouldBe 1
    }
    "work through COALESCE" in new CpgFromCodeTestFixture(
      """$a = $_GET["a"];
        |file_get_contents($a ?? 'as');
        |""".stripMargin) {
      val res: List[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      res.length shouldBe 1
    }
    "detect SSRF in try block with loop in catch" in new CpgFromCodeTestFixture(
      """try {
        |   file_get_contents($_GET['a']);
        |} catch (Exception $e) {
        |   for($i = 1; $i <= 10; $i++) {
        |     echo $i;
        |   }
        |}
        |""".stripMargin) {
      val res: List[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      res.length shouldBe 1
    }
    "detect SSRF in try block with loop" in new CpgFromCodeTestFixture(
      """try {
        |   for($i = 1; i <= 10; $i++) {
        |     file_get_contents($_GET['a']);
        |     echo $i;
        |   }
        |} catch (Exception $e) {
        |}
        |""".stripMargin) {
      val res: List[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      res.length shouldBe 1
    }
    "detect SSRF in try block that is in a loop" in new CpgFromCodeTestFixture(
      """for($i = 1; $i <= 10; $i++) {
        |try {
        |   file_get_contents($_GET["a"]);
        |   echo $i;
        |} catch (Exception $e) { }
        |}
        |""".stripMargin) {
      val res: List[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      res.length shouldBe 1
    }
  "detect curl related ssrf" when {
    "curl_init" in new CpgFromCodeTestFixture("""curl_exec(curl_init($_GET["a"]));""") {
      val res: List[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      res.length shouldBe 1
    }
    "curl_setopt with correct opt" in new CpgFromCodeTestFixture(
      """$ch = curl_init();
        |curl_setopt($ch, CURLOPT_URL, $_GET["a"]);
        |curl_exec($ch);
        |curl_close($ch);
        |""".stripMargin) {
      val res: List[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      res.length shouldBe 1
    }
    "curl_setopt false-positive" in new CpgFixture(
      """$ch = curl_init("http://example.org");
        |curl_setopt($ch, CURLOPT_USERAGENT, $_GET["a"]);
        |curl_exec($ch);
        |curl_close($ch);
        |""".stripMargin) {
      val res: List[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      res shouldBe empty
    }
    "curl_setopt dynamic" in new CpgFixture(
      """$conf = CURLOPT_URL;
        |$ch = curl_init("http://example.org");
        |curl_setopt($ch, $conf, $_GET["a"]);
        |curl_exec($ch);
        |curl_close($ch);
        |""".stripMargin) {
      val res: List[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      res.length shouldBe 1
    }
  }
  "don't get confused by namespaces" in new CpgFromCodeTestFixture(
    """namespace test;
      |$a = $_GET["a"];
      |file_get_contents($a);
      |""".stripMargin) {
    cpg.call.out(EdgeTypes.CALL).collectAll[Method].filter(_.code == Defines.INTERNAL_FUNCTION)
      .name("file_get_contents").size shouldBe 1
    val res: List[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
    res.length shouldBe 1
  }
  "sink with multiple params" when {
    "no false positive" in new CpgFixture("file_get_contents(\"safe\", $_GET[\"i\"]);") {
      val res: List[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      res shouldBe empty
    }
    "no false negative" in new CpgFixture("file_get_contents($_GET[\"i\"], \"save\");") {
      val res: List[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      res.length shouldBe 1
    }
  }


    "recursion" in new CpgFromCodeTestFixture(
      """function f($u, $i){
        |  if($i < 1) {f($u, $i + 1);}
        |  file_get_contents($u);
        |}
        |f($_GET["a"], 0);
        |""".stripMargin) {
      try {
        SSRFDetector.getCandidates(debug = true).unwrap.length shouldBe 1
      } catch {
        case _: StackOverflowError => fail("StackOverFlowError")
      }
    }
    "arrays, loops, and recursion" in new CpgFromCodeTestFixture(
      """$a = array(
        |    array("https://example.org"),
        |    array($_GET["x"])
        |);
        |
        |function p($m)
        |{
        |    if (is_array($m)) {
        |        foreach ($m as $elem) {
        |            p($elem);
        |        }
        |    } else {
        |        echo $m;
        |    }
        |}
        |
        |function r($m)
        |{
        |    if (is_array($m)) {
        |        foreach ($m as $elem) {
        |            r($elem);
        |        }
        |    } else {
        |        file_get_contents($m);
        |    }
        |}
        |p($a);
        |r($a);
        |""".stripMargin) {
      try {
        SSRFDetector.getCandidates(debug = true).unwrap.sizeIs > 0
      } catch {
        case _: StackOverflowError => fail("StackOverFlowError")
      }
    }
  "composer setup poc" in new CpgFromCodeTestFixture(
    """//class Installer
      |//{
      |    function downloadToTmp()
      |    {
      |       $url = $_SERVER['http_proxy'];
      |       if(get($url)){
      |       } elseif(get($url)){
      |       }
      |    }
      |
      |    function get($url)
      |    {
      |        return file_get_contents($url, false, $context);
      |    }
      |//}
      |""".stripMargin
  ) {
    SSRFDetector.getCandidates(false)._1 match {
      case Left(result) :: Nil => succeed
      case Right(error) :: Nil => fail(error.getMessage)
      case _ => fail("too much or not enough results")
    }
  }
}
