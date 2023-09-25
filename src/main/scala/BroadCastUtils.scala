/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xs.utils
import chisel3._
import chisel3.util.experimental.BoringUtils

import scala.collection.immutable.SeqMap
import scala.collection.mutable

class BroadCastBundle(infos:mutable.HashMap[String, Seq[Data]]) extends Record {
  val elements: SeqMap[String, Data] = SeqMap() ++ infos.map(i => (i._1, chiselTypeOf(i._2.head)))
  def apply(key:String):Option[Data] = {
    if(elements.contains(key)){
      Some(elements(key))
    } else {
      None
    }
  }
}

object BroadCastingUtils {
  private val broadCastMap = new mutable.HashMap[String, Seq[Data]]

  def AddBroadCastSink(key:String, in:Data):Unit = {
    dontTouch(in)
    if(broadCastMap.contains(key)){
      require(broadCastMap(key).head.getWidth == in.getWidth)
      broadCastMap(key) = broadCastMap(key) :+ in
    } else {
      broadCastMap(key) = Seq(in)
    }
  }

  def GenBroadCastSource(): BroadCastBundle = {
    val res = Wire(new BroadCastBundle(broadCastMap))
    res.elements.foreach({case(key, data) =>
      broadCastMap(key).foreach(d => {
        BoringUtils.bore(d) := data
      })
    })
    res
  }
}


