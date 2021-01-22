package fxb

case class AdresarW(winstrom: AdresarW.Inner)

object AdresarW {
  case class Inner(adresar: Seq[Adresar])
}