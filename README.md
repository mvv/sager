# Sager
[![Release Version](https://img.shields.io/nexus/r/https/oss.sonatype.org/com.github.mvv.sager/sager_2.13.svg)](https://oss.sonatype.org/content/repositories/releases/com/github/mvv/sager)
[![Snapshot Version](https://img.shields.io/nexus/s/https/oss.sonatype.org/com.github.mvv.sager/sager_2.13.svg)](https://oss.sonatype.org/content/repositories/snapshots/com/github/mvv/sager)
[![Build Status](https://travis-ci.com/mvv/sager.svg?branch=master)](https://travis-ci.com/mvv/sager)

Generic records for Scala. Record types have the form

```scala
Field[Label1, Value1] with ... with Field[LabelN, ValueN]
```

where `Field` is invariant in its first argument and covariant in the second.

## Using Sager in your project

Add Sager to your dependencies

```scala
libraryDependencies += "com.github.mvv.sager" %% "sager" % "0.2-SNAPSHOT"
```

To construct a record, use

```scala
import com.github.mvv.sager._
// Label types
trait Foo
trait Bar
trait Baz
val r = Field[Foo]("foo").add[Bar](123).add[Baz](Vector.empty[Double])
// val r: Field[Foo,String] with Field[Bar,Int] with Field[Baz,Vector[Double]]
```

Record types are polymorthic in expected ways:

```scala
// All of the following implicit searches succeed
implicitly[r.type <:< Field[Bar,Int] with Field[Foo,String]]
implicitly[r.type <:< Field[Baz,Seq[Double]]]
```

Accessing and modifying fields is simple:

```scala
val bar = r.get[Bar]
// val bar: Int = 123
val newBar = r.add[Bar](false).get[Bar]
// val newBar: Boolean = false
val updatedBar = r.update[Bar]((_: Int) > 100).get[Bar]
// val updatedBar: Boolean = true
// Same thing, but more inference-friendly
val mappedBar = r.field[Bar].map(_ > 100).get[Bar]
// val mappedBar: Boolean = true
val r1 = r.remove[Bar]
// val r1: Field[Foo,String] with Field[Baz,Vector[Double]]
```

## Submodules

* `sager-zio` contains helpers for using records as [ZIO](https://github.com/zio/zio) environments
* `sager-zio-interop-cats` extends ZIO Cats-Effect [interop](https://github.com/zio/interop-cats) with `sager-zio`
   support
