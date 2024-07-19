package top

import circt.stage.CLI
import firrtl.AnnotationSeq
import firrtl.annotations.ModuleTarget
import firrtl.ir.{Block, Conditionally, DefInstance, ExtModule, IntModule, Module, Statement}
import firrtl.options.{Dependency, Phase, PhaseManager, Shell, Stage}
import firrtl.renamemap.MutableRenameMap
import firrtl.stage.FirrtlCircuitAnnotation

object PrefixPhase {
  var prefix = ""

  def StatementsWalker(stmt: Statement): Statement = {
    stmt match {
      case s: DefInstance => s.copy(module = prefix + s.module)
      case s: Conditionally => s.copy(conseq = StatementsWalker(s.conseq), alt = StatementsWalker(s.alt))
      case s: Block => {
        val stmts = s.stmts.map(StatementsWalker)
        s.copy(stmts = stmts)
      }
      case other => other
    }
  }
}

class PrefixPhase extends Phase {
  override def prerequisites: Seq[Nothing] = Seq.empty

  override def optionalPrerequisites: Seq[Nothing] = Seq.empty

  override def optionalPrerequisiteOf: Seq[Nothing] = Seq.empty

  override def invalidates(a: Phase) = false

  private val prefix = PrefixPhase.prefix
  private val renameMap = MutableRenameMap()

  def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val prefixedAS = annotations.flatMap {
      case a: FirrtlCircuitAnnotation =>
        val mods = a.circuit.modules.map {
          case mm@Module(_, name, _, body) => {
            renameMap.record(ModuleTarget(a.circuit.main, name), ModuleTarget(prefix + a.circuit.main, prefix + name))
            val nst = PrefixPhase.StatementsWalker(body)
            mm.copy(name = prefix + name, body = nst)
          }
          case em@ExtModule(_, name, _, defname, _) => {
            renameMap.record(ModuleTarget(a.circuit.main, name), ModuleTarget(prefix + a.circuit.main, prefix + name))
            em.copy(name = prefix + name, defname = prefix + defname)
          }
          case im@IntModule(_, name, _, _, _) => {
            renameMap.record(ModuleTarget(a.circuit.main, name), ModuleTarget(prefix + a.circuit.main, prefix + name))
            im.copy(name = prefix + name)
          }
          case other => other
        }
        val nc = a.circuit.copy(modules = mods, main = prefix + a.circuit.main)
        Some(FirrtlCircuitAnnotation(nc))
      case a => Some(a)
    }
    val redirectedAS = prefixedAS.flatMap {
      a => a.update(renameMap)
    }
    redirectedAS
  }
}

class XsStage extends Stage {
  override def prerequisites = Seq.empty

  override def optionalPrerequisites = Seq.empty

  override def optionalPrerequisiteOf = Seq.empty

  override def invalidates(a: Phase) = false

  override val shell = new Shell("Xiangshan Utils") with CLI

  override def run(annotations: AnnotationSeq): AnnotationSeq = {
    val pm = new PhaseManager(
      targets = Seq(
        Dependency[chisel3.stage.phases.Checks],
        Dependency[chisel3.stage.phases.AddImplicitOutputFile],
        Dependency[chisel3.stage.phases.AddImplicitOutputAnnotationFile],
        Dependency[chisel3.stage.phases.MaybeAspectPhase],
        Dependency[chisel3.stage.phases.AddSerializationAnnotations],
        Dependency[chisel3.stage.phases.Convert],
        Dependency[PrefixPhase],
        Dependency[chisel3.stage.phases.MaybeInjectingPhase],
        Dependency[circt.stage.phases.AddImplicitOutputFile],
        Dependency[circt.stage.phases.Checks],
        Dependency[circt.stage.phases.CIRCT]
      ),
      currentState = Seq(
        Dependency[firrtl.stage.phases.AddDefaults],
        Dependency[firrtl.stage.phases.Checks]
      )
    )
    pm.transform(annotations)
  }
}
