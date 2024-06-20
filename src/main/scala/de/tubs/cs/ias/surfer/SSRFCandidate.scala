package de.tubs.cs.ias.surfer

import de.tubs.cs.ias.cpg.dotFileExporter.conf.ExportConfig
import de.tubs.cs.ias.cpg.slicing.representation.{ProgramSlice, SliceNode}
import de.tubs.cs.ias.surfer.reversing.{ReversedBlob, ReversingCache, ReversingContext, ReversingRuleDeployer}
import de.tubs.cs.ias.surfer.sinks.SinkDefinition
import wvlet.log.LogSupport

class SSRFCandidate(programSlice: ProgramSlice, sink: SinkDefinition)
    extends LogSupport {

  override def toString: String = s"[SSRF Candidate of sink $sink]"

  private var blobs: Option[List[ReversedBlob]] = None
  private var sinkStrings: Option[Set[String]] = None
  def fullReversing(beAnnoying: Boolean = false,
                    context: String = ""): SSRFCandidate = {
    implicit val cache: ReversingCache = new ReversingCache(beAnnoying, context)
    //println(cache)
    try {
      debug("reversing blobs")
      calculateSinkBlobs()
      debug("reversing sink strings")
      calculateSinkStrings()
      debug("reversing done")
    } finally {
      cache.clear()
    }
    this
  }

  def getSinkDefinition: SinkDefinition = sink

  def showGraph(dot: String = "dot", view: String = "xdg-open")(
      implicit config: ExportConfig): Unit = {
    programSlice.showGraph(dot, view)
  }

  def getDot(implicit config: ExportConfig): String = {
    programSlice.toDot
  }

  def getSink: SliceNode = {
    programSlice.getNode(this.programSlice.getStartNode.id())
  }

  def getSources: Set[SliceNode] = {
    programSlice.getStartNodes()
  }

  def getSlice: ProgramSlice = programSlice

  /** Generate the Blob that is passed into the sink
    *
    * @return the blob that is passed into the sink
    */
  private def calculateSinkBlobs()(implicit cache: ReversingCache): Unit = {
    try {
      blobs = Some(
        ReversingRuleDeployer.deploy(getSink,
                                     ReversingContext(Set(), List(), cache)))
    } finally {
      info("clearing reversing cache")
      cache.getReversingCache
        .clear() // we have reversed - we may now clear that cache again
    }
  }

  def getSinkBlobs: List[ReversedBlob] = {
    blobs match {
      case Some(value) => value
      case None =>
        throw new RuntimeException(
          "you need to reverse the SSRF candidate first")
    }
  }

  private def calculateSinkStrings()(implicit cache: ReversingCache): Unit = {
    sinkStrings = Some(getSinkBlobs.map(_.getBlobStringRepresentation).toSet)
  }

  def getSinkStrings: Set[String] = {
    sinkStrings match {
      case Some(value) => value
      case None =>
        throw new RuntimeException(
          "you need to reverse the SSRF candidate first")
    }
  }
}
