import os.Path
import mill._
import mill.scalalib._
import coursier.maven.MavenRepository
import scalalib._
import $file.`rocket-chip`.common
import $file.`rocket-chip`.`api-config-chipsalliance`.`build-rules`.mill.build
import $file.`rocket-chip`.hardfloat.build

object ivys {
  val sv = "2.12.13"
  val chisel3 = ivy"edu.berkeley.cs::chisel3:3.5.6"
  val chisel3Plugin = ivy"edu.berkeley.cs:::chisel3-plugin:3.5.6"
  val chiseltest = ivy"edu.berkeley.cs::chiseltest:0.5.6"
  val scalatest = ivy"org.scalatest::scalatest:3.2.2"
  val macroParadise = ivy"org.scalamacros:::paradise:2.1.1"
  val sourcecode = ivy"com.lihaoyi::sourcecode:0.2.7"
}

trait MyCommonModule extends ScalaModule with SbtModule{
  override def scalaVersion = ivys.sv
  override def compileIvyDeps = Agg(ivys.macroParadise)
  override def scalacPluginIvyDeps = Agg(ivys.macroParadise, ivys.chisel3Plugin)
  override def scalacOptions = Seq("-Xsource:2.11", "-language:reflectiveCalls")
  def ivyDeps = Agg(ivys.chisel3, ivys.sourcecode)
}

object rocketchip extends `rocket-chip`.common.CommonRocketChip {

  val rcPath = os.pwd / "rocket-chip"
  override def scalaVersion = ivys.sv
  override def scalacOptions = Seq("-Xsource:2.11")
  override def millSourcePath = rcPath

  object configRocket extends `rocket-chip`.`api-config-chipsalliance`.`build-rules`.mill.build.config with PublishModule {
    override def millSourcePath = rcPath / "api-config-chipsalliance" / "design" / "craft"
    override def scalaVersion = T {
      rocketchip.scalaVersion()
    }
    override def pomSettings = T {
      rocketchip.pomSettings()
    }
    override def publishVersion = T {
      rocketchip.publishVersion()
    }
  }

  object hardfloatRocket extends `rocket-chip`.hardfloat.build.hardfloat {
    override def millSourcePath = rcPath / "hardfloat"
    override def scalaVersion = T {
      rocketchip.scalaVersion()
    }
    def chisel3IvyDeps = if(chisel3Module.isEmpty) Agg(
      common.getVersion("chisel3")
    ) else Agg.empty[Dep]
    
    def chisel3PluginIvyDeps = Agg(common.getVersion("chisel3-plugin", cross=true))
  }
  def hardfloatModule = hardfloatRocket
  def configModule = configRocket
}

object `xs-utils` extends MyCommonModule{
  override def moduleDeps = super.moduleDeps ++ Seq(rocketchip)
  override def millSourcePath = os.pwd
}