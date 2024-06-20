package de.tubs.cs.ias.surfer

import de.tubs.cs.ias.cpg.dotFileExporter.conf.ExportConfig
import de.tubs.cs.ias.surfer.implicits.{OneableIterableOnce, UnwrappableCandidate}
import io.joern.bytecode.parser.PHPVersion
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.File
import scala.annotation.nowarn

class CpgFixture(code: String) extends CpgFromCodeTestFixture(code)(PHPVersion.V8) {
  implicit val pcpg: Cpg = cpg
}

//noinspection HttpUrlsUsage
@nowarn("msg=possible missing interpolator")
class SSRFCandidateReversingTest extends AnyWordSpec with Matchers {


  implicit val config: ExportConfig = ExportConfig.apply(new File("export.json"))
  implicit val version: PHPVersion.Value = PHPVersion.V8

  "SSRFCandidate.getSinkBlob" should {
    "give me the proper string for a simple $_GET flow" in new CpgFromCodeTestFixture(
      """$x = $_GET["test"];
        |file_get_contents($x);
        |""".stripMargin) {
      val candidate: Seq[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      candidate.length shouldBe 1
      //candidate.head.showGraph()
      val strings: Set[String] = candidate.head.getSinkStrings
      strings shouldBe Set("<G:_GET>[test]")
      //candidate.head.getStringTrees shouldBe Set("(INDEX (GLOBAL _GET) \"test\")")
    }
    "give me the proper string for a concatenation of $_GET and only constant" in new CpgFromCodeTestFixture(
      """$x = $_GET["test"];
        |$y = "meesa string";
        |file_get_contents($x . $y);
        |""".stripMargin) {
      val candidate: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
      candidate.getSinkStrings shouldBe Set("<G:_GET>[test]meesa string")
      // candidate.getStringTrees shouldBe Set("(CONCAT (INDEX (GLOBAL _GET) \"test\") \"meesa string\")")
    }
    "give me the proper string for a concatenation of $_GET and constant string" in new CpgFromCodeTestFixture(
      """$x = $_GET["test"];
        |file_get_contents($x . "meesa string");
        |""".stripMargin) {
      val candidate: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
      candidate.getSinkStrings shouldBe Set("<G:_GET>[test]meesa string")
      //      candidate.getStringTrees shouldBe Set("(CONCAT (INDEX (GLOBAL _GET) \"test\") \"meesa string\")")
    }

    "give me the proper string for a concatenation of constant string and $_GET" in new CpgFromCodeTestFixture(
      """$x = $_GET["test"];
        |file_get_contents("meesa string".$x);
        |""".stripMargin) {
      val candidate: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
      candidate.getSinkStrings shouldBe Set("meesa string<G:_GET>[test]")
      //candidate.getStringTrees shouldBe Set("(CONCAT \"meesa string\" (INDEX (GLOBAL _GET) \"test\"))")
    }

    "give me the proper string for a concatenation of constant and $_GET" in new CpgFromCodeTestFixture(
      """$x = $_GET["test"];
        |$y = "meesa string";
        |file_get_contents($y . $x);
        |""".stripMargin) {
      val candidate: Seq[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      candidate.length shouldBe 1
      val strings: Set[String] = candidate.head.getSinkStrings
      strings shouldBe Set("meesa string<G:_GET>[test]")
    }
    "give me the proper string for a fast concatenation of $_GET and constant" in new CpgFromCodeTestFixture(
      """$x = $_GET["test"];
        |$y = "meesa string";
        |file_get_contents("$y$x");
        |""".stripMargin) {
      val candidate: Seq[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      candidate.length shouldBe 1
      val strings: Set[String] = candidate.head.getSinkStrings
      strings shouldBe Set("meesa string<G:_GET>[test]")
    }
    "give me the proper string for a fast concatenation of constant and $_GET" in new CpgFromCodeTestFixture(
      """$x = $_GET["test"];
        |$y = "meesa string";
        |file_get_contents("$x$y");
        |""".stripMargin) {
      val candidate: Seq[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      candidate.length shouldBe 1
      val strings: Set[String] = candidate.head.getSinkStrings
      strings shouldBe Set("<G:_GET>[test]meesa string")
    }
  }
  "give me the proper string for a fast concatenation of constant in string and $_GET" in new CpgFromCodeTestFixture(
    """$x = $_GET["test"];
      |file_get_contents("meesa string$x");
      |""".stripMargin) {
    val candidate: Seq[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
    candidate.length shouldBe 1
    val strings: Set[String] = candidate.head.getSinkStrings
    strings shouldBe Set("meesa string<G:_GET>[test]")
  }

  "give me the proper string for a fast concatenation of constant in string and $_GET II" in new CpgFromCodeTestFixture(
    """$x = $_GET["test"];
      |file_get_contents("$x meesa string");
      |""".stripMargin) {
    val candidate: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
    candidate.getSinkStrings shouldBe Set("<G:_GET>[test] meesa string")
    //candidate.getStringTrees shouldBe Set("(CONCAT (INDEX (GLOBAL _GET) \"test\") \" meesa string\")")
  }
  "deal with const of GET" in new CpgFromCodeTestFixture(
    """
      |$parameter = 'p';
      |if (isset($_GET[$parameter])) {
      |    file_get_contents($_GET[$parameter]);
      |}
      |""".stripMargin) {
    val c: List[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
    val res: List[Set[String]] = c.map(_.getSinkStrings)
    res shouldBe List(Set("<G:_GET>[p]"))
  }
  "rope" when {
    "rope const - x - var" in new CpgFixture(
      """$bar = $_GET["a"];
        |$a = "/test";
        |echo file_get_contents("https://${bar}$a");""".stripMargin) {
      val candidate: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
      candidate.getSinkStrings shouldBe Set("https://<G:_GET>[a]/test")
      // candidate.getStringTrees shouldBe Set("""(CONCAT (CONCAT "https://" (INDEX (GLOBAL _GET) "a")) "/test")""")
    }
    "rope const - var - x " in new CpgFixture(
      """$bar = $_GET["a"];
        |$a = "/test";
        |echo file_get_contents("https://$a${bar}");""".stripMargin) {
      try {
        //wvlet.log.Logger.rootLogger.setLogLevel(LogLevel.DEBUG)
        val candidate: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
        candidate.getSinkStrings shouldBe Set("https:///test<G:_GET>[a]")
      } finally {
        //wvlet.log.Logger.rootLogger.setLogLevel(LogLevel.INFO)
      }
    }
    "rope x - const - var" in new CpgFixture(
      """$bar = $_GET["a"];
        |$a = "/test";
        |echo file_get_contents("${bar}https://$a");""".stripMargin) {
      val candidate: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
      candidate.getSinkStrings shouldBe Set("<G:_GET>[a]https:///test")
    }
    "rope const - x - const" in new CpgFixture(
      """$bar = $_GET["a"];
        |file_get_contents("https://${bar}sd");
        |""".stripMargin) {
      val candidate: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
      candidate.getSinkStrings shouldBe Set("https://<G:_GET>[a]sd")
    }
  }

  "inter" when {
    "decorator function" in new CpgFromCodeTestFixture(
      """
        |function query($q) {
        |   return "https://example.org?q=".$q;
        |}
        |file_get_contents(query($_GET["x"]));
        |""".stripMargin) {
      val candidate: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
      candidate.getSinkStrings shouldBe Set("https://example.org?q=<G:_GET>[x]")
    }
    "nested decorator function with same function" in new CpgFromCodeTestFixture(
      """
        |function query($q) {
        |   return "https://example.org?q=".$q;
        |}
        |file_get_contents(query(query($_GET["x"])));
        |""".stripMargin) {
      val candidate: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
      //candidate.showGraph()
      candidate.getSinkStrings shouldBe Set("https://example.org?q=https://example.org?q=<G:_GET>[x]")
    }
    "nested decorator function with different functions" in new CpgFromCodeTestFixture(
      """
        |function query($q) {
        |   return "?q=".$q;
        |}
        |function pd($q) {
        |   return "https://example.org".$q;
        |}
        |file_get_contents(pd(query($_GET["x"])));
        |""".stripMargin) {
      val candidate: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
      //candidate.showGraph()
      candidate.getSinkStrings shouldBe Set("https://example.org?q=<G:_GET>[x]")
    }
    "use function twice" in new CpgFromCodeTestFixture(
      """
        |function f($q) {
        |   return $q;
        |}
        |file_get_contents(f("http://").f($_GET["x"]));
        |""".stripMargin) {
      val candidate: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
      // candidate.showGraph()
      candidate.getSinkStrings shouldBe Set("http://<G:_GET>[x]")
    }
    "source in func" in new CpgFromCodeTestFixture(
      """
        |function s(){return $_GET["x"];}
        |file_get_contents(s());
        |""".stripMargin) {
      SSRFDetector.getCandidates(debug = false).unwrap.one.getSinkStrings shouldBe Set("<G:_GET>[x]")
    }
    "sink in func" when {
      "most basic version" in new CpgFromCodeTestFixture(
        """
          |function s(){file_get_contents($_GET["x"]);}
          |s();
          |""".stripMargin) {
        SSRFDetector.getCandidates(debug = false).unwrap.one.getSinkStrings shouldBe Set("<G:_GET>[x]")
      }
      "with parameter as prefix" in new CpgFromCodeTestFixture(
        """
          |function s($prefix){file_get_contents($prefix.$_GET["x"]);}
          |s("pre");
          |""".stripMargin) {
        val candidate: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
        //candidate.getSlice.showGraph()
        candidate.getSinkStrings shouldBe
          Set("pre<G:_GET>[x]")
      }
      "with parameter with default value" in new CpgFromCodeTestFixture(
        """
          |function s($prefix="def"){
          |    file_get_contents($prefix.$_GET["x"]);
          |}
          |s();
          |""".stripMargin) {
        val candidate: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
        candidate.getSinkStrings shouldBe
          Set("def<G:_GET>[x]")
      }
      "with multiple source calls no default" in new CpgFromCodeTestFixture(
        """function s($prefix) {
          |   file_get_contents($prefix.$_GET['x']);
          |}
          |s("a");
          |s("b");
          |""".stripMargin
      ) {
        val candidate: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
        //candidate.getSlice.showGraph()
        candidate.getSinkStrings shouldBe
          Set("a<G:_GET>[x]","b<G:_GET>[x]")
      }
      "with multiple source calls with used default" in new CpgFromCodeTestFixture(
        """function s($prefix="default") {
          |   file_get_contents($prefix.$_GET['x']);
          |}
          |s("a");
          |s();
          |""".stripMargin
      ) {
        val candidate: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
        //candidate.getSlice.showGraph()
        candidate.getSinkStrings shouldBe
          Set("a<G:_GET>[x]", "default<G:_GET>[x]")
      }
      "with multiple source calls with unused default" in new CpgFromCodeTestFixture(
        """function s($prefix="default") {
          |   file_get_contents($prefix.$_GET['x']);
          |}
          |s("a");
          |s("b");
          |""".stripMargin
      ) {
        val candidate: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
        //candidate.getSlice.showGraph()
        candidate.getSinkStrings shouldBe
          Set("a<G:_GET>[x]", "b<G:_GET>[x]")
      }
    }
  }

  "named params" when {
    "two params, but safe via named" in new CpgFromCodeTestFixture(
      """
        |function url($a, $b) {
        |   echo $a;
        |   return $b;
        |}
        |file_get_contents(url(a:$_GET["x"],b:"https://example.org"));
        |""".stripMargin) {
      val c: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
      c.getSinkStrings shouldBe Set("https://example.org")
    }

    "two params, but unsafe via named -- switched order" in new CpgFromCodeTestFixture(
      """
        |function url($a, $b) {
        |   echo $a;
        |   return $b;
        |}
        |file_get_contents(url(b:$_GET["x"],a:"https://example.org"));
        |""".stripMargin) {
      val c: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
      // TODO this one fails
      c.getSinkStrings shouldBe Set("<G:_GET>[x]")
    }
  }

  "RECV_INIT" when {
    "unused default" in new CpgFromCodeTestFixture(
      """
        |function url($u = "example.org") {
        |   return "https://".$u;
        |}
        |file_get_contents(url($_GET["x"]));
        |""".stripMargin) {
      val c: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
      c.getSinkStrings shouldBe Set("https://<G:_GET>[x]")
    }
    "default used" in new CpgFromCodeTestFixture(
      """
        |function url($u = "example.org") {
        |   return "https://".$u;
        |}
        |$uninvolved = $_GET['test']; // required as without source no slice is calculated
        |file_get_contents(url());
        |""".stripMargin) {
      val c: Seq[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      c.length shouldBe 1
      c.head.getSinkStrings shouldBe Set("https://example.org")
    }
    "fallback to default with concat" in new CpgFromCodeTestFixture(
      """
        |function url($u = "example.org") {
        |   return "https://".$u;
        |}
        |file_get_contents(url().$_GET["q"]);
        |""".stripMargin) {
      val c: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
      c.getSinkStrings shouldBe Set("https://example.org<G:_GET>[q]")
    }
    "two params" when {
      "positional usage" when {
        "unsafe" in new CpgFromCodeTestFixture(
          """
            |function url($a, $b = "https://example.org") {
            |echo $a;
            |return $b;
            |}
            |file_get_contents(url("foo", $_GET["x"]));
            |""".stripMargin) {
          val c: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
          //c.showGraph()
          c.getSinkStrings shouldBe  Set("<G:_GET>[x]")
        }
        "safe" in new CpgFromCodeTestFixture(
          """
            |function url($a, $b = "https://example.org") {
            |echo $a;
            |return $b;
            |}
            |file_get_contents(url($_GET["x"]));
            |""".stripMargin) {
          val c: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
          // c.showGraph()
          c.getSinkStrings shouldBe Set("https://example.org")
        }
      }
      "named usage" when {
        "unsafe" in new CpgFromCodeTestFixture(
          """
            |function url($a, $b = "https://example.org") {
            |echo $a;
            |return $b;
            |}
            |file_get_contents(url(b: $_GET["x"], a: "foo"));
            |""".stripMargin) {
          val c: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
          //c.showGraph()
          c.getSinkStrings shouldBe  Set("<G:_GET>[x]")
        }
        "safe" in new CpgFromCodeTestFixture(
          """
            |function url($a, $b = "https://example.org") {
            |echo $a;
            |return $b;
            |}
            |file_get_contents(url(a: $_GET["x"]));
            |""".stripMargin) {
          val c: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
          // c.showGraph()
          c.getSinkStrings shouldBe Set("https://example.org")
        }
      }
      "mixed usage" when {
        "unsafe via second" in new CpgFromCodeTestFixture(
          """
            |function url($a, $b = "https://example.org") {
            |echo $a;
            |return $b;
            |}
            |file_get_contents(url("foo", b: $_GET["x"]));
            |""".stripMargin) {
          val c: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
          //c.showGraph()
          c.getSinkStrings shouldBe Set("<G:_GET>[x]")
        }
        "unsafe via first" in new CpgFromCodeTestFixture(
          """
            |function url($a = "https://example.org", $b) {
            |echo $b;
            |return $a;
            |}
            |file_get_contents(url($_GET["x"], b: "foo"));
            |""".stripMargin) {
          val c: SSRFCandidate = SSRFDetector.getCandidates(debug = false).unwrap.one
          c.getSinkStrings shouldBe Set("<G:_GET>[x]")
        }
      }
    }
  }

  "multiple alternatives" when {
    "ignored if" in new CpgFromCodeTestFixture(
      """
        |$x = $_GET['a'];
        |if($x == "zz")
        |   $y = "43";
        |if ($y == "42")
        |    $x = "test";
        |file_get_contents($x);
        |""".stripMargin) {
      val candidates: Seq[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      //candidates.head.showGraph()
      candidates.head.getSinkStrings shouldBe Set("<G:_GET>[a]", "test")
    }
    "defining if" in new CpgFromCodeTestFixture(
      """
        |if(rand(0,3) == 2){
        |   $x = "43";
        |   }else {
        |   $x = $_GET['a'];
        |   }
        |file_get_contents($x);
        |""".stripMargin) {
      val candidates: Seq[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
      //candidates.head.showGraph()
      candidates.head.getSinkStrings shouldBe Set("<G:_GET>[a]", "43")
    }
    "multiple return edges" when {
      "one source, one const" in new CpgFixture(
        """
          |function f() {
          |  if($x) {
          |    return $_GET["x"];
          |  } else {
          |    return "foo";
          |  }
          |}
          |file_get_contents(f());
          |""".stripMargin) {
        val candidates: List[SSRFCandidate] = SSRFDetector.getCandidates(debug = true).unwrap
        candidates.flatMap(_.getSinkStrings).toSet shouldBe Set("foo", "<G:_GET>[x]")
      }
      "two sources" in new CpgFixture(
        """
          |function f() {
          |  if($x) {
          |    return $_GET["x"];
          |  } else {
          |    return $_GET["y"];
          |  }
          |}
          |file_get_contents(f());
          |""".stripMargin) {
        val candidates: List[SSRFCandidate] = SSRFDetector.getCandidates(debug = false).unwrap
        candidates.flatMap(_.getSinkStrings).toSet shouldBe Set("<G:_GET>[y]", "<G:_GET>[x]")
      }
    }
  }
  "account for CPG restrictions" when {
    "array assignments are involved" in new CpgFromCodeTestFixture(
      """$x["a"] = $_GET["stuff"];
        |$b = $x["a"];
        |file_get_contents($b);
        |""".stripMargin
    ) {
      //new CpgDotFileCreator(cpg.graph).show()
      val candidate :: Nil = SSRFDetector.getCandidates(debug = false).unwrap
      //candidate.showGraph()
      candidate.getSinkStrings shouldBe Set("<G:_GET>[stuff]")
    }
  }
  "handle objs better" in new CpgFromCodeTestFixture(
    """
      |$uri = Uri::createFromString($_GET["x"]);
      |if($uri->getHost() !== "localhost") {
      | file_get_contents($_GET["x"]);
      |}
      |""".stripMargin) {
    val candidate :: Nil = SSRFDetector.getCandidates(debug = false).unwrap
    //candidate.showGraph()
    //new CpgDotFileCreator(cpg.graph).show()
    val condition: String =
    """(NOT
      |  (EQUALP
      |    (.*::GETHOST (URI::CREATEFROMSTRING (INDEX (GLOBAL _GET) 'x')) )
      |    'localhost'))
      |""".stripMargin.stripMargin.filter(_ >= ' ').replaceAll(" +", " ")
    candidate.getSinkStrings shouldBe Set("<G:_GET>[x]")
  }
  "handle array creation and static access" in new CpgFromCodeTestFixture(
    """$x = [
      |   'a' => $_GET[1],
      |   'b' => 'test'
      |];
      |file_get_contents($x['a']);
      |""".stripMargin) {
      SSRFDetector.getCandidates(debug = false)._1.head match {
        case Left(value) =>
          //value.showGraph()
          value.getSinkStrings shouldBe Set("<G:_GET>[1]")
        case Right(value) => fail(value.getMessage)
      }
  }
  "handle constant arrays" in new CpgFromCodeTestFixture(
    """
      |$x = [
      |   'a' => 'https://',
      |   'b' => 'test'
      |];
      |file_get_contents($x['a'].$_GET["x"]);
      |""".stripMargin) {
    // todo adjust ASSIGN
    SSRFDetector.getCandidates(false).unwrap.one.getSinkStrings shouldBe Set("https://<G:_GET>[x]")
  }
  "handle constant arrays with merge" in new CpgFromCodeTestFixture(
    """
      |$x = [
      |   'a' => 'https://',
      |   'b' => 'test'
      |];
      |file_get_contents($x['a'].$_GET["x"]);
      |""".stripMargin) {
    // todo adjust ASSIGN
    SSRFDetector.getCandidates(false).unwrap.one.getSinkStrings shouldBe Set("https://<G:_GET>[x]")
  }

  "handle constants" in new CpgFromCodeTestFixture("""define("FOO", "something");
                                                     |file_get_contents(FOO.$_GET["x"]);
                                                     |""".stripMargin) {
    SSRFDetector.getCandidates(false, false).unwrap match {
      case head :: Nil =>
        head.getSinkStrings shouldBe Set("something<G:_GET>[x]")
      case Nil => fail()
    }
  }
}
