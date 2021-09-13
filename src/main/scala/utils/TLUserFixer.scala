package utils

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLAdapterNode, TLIdentityNode}
import freechips.rocketchip.util.{BundleField, BundleFieldBase}
import huancun.{PreferCacheField, PrefetchField}

class TLUserFixer(reqFields: Seq[BundleFieldBase])(implicit p: Parameters) extends LazyModule {

  val node = TLAdapterNode(
    clientFn = {c => c.v1copy(requestFields = reqFields)}
  )

  lazy val module = new LazyModuleImp(this){
    for(((in, edgeIn), (out, edgeOut)) <- node.in.zip(node.out)){
      in <> out
    }
  }
}

object TLUserFixer {
  def apply(reqFields: Seq[BundleFieldBase] = Seq(
    PreferCacheField()
  ))(implicit p: Parameters) = {
    val fixer = LazyModule(new TLUserFixer(reqFields))
    fixer.node
  }
}
