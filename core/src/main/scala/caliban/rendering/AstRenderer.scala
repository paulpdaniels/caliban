package caliban.rendering

trait AstRenderer[-T] {
  def render(in: T, out: StringBuilder, opt: RenderingOptions): Unit
}

object AstRenderer {

  def render[T](t: T)(renderer: AstRenderer[T]): String = {
    val builder = new StringBuilder
    renderer.render(t, builder, RenderingOptions())
    builder.result()
  }

}
