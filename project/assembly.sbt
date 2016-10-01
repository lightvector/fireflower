//See https://github.com/sbt/sbt-assembly
//Adds the 'assembly' command within sbt to package the scala libraries and stuff
//into and executable jar so that it's actually standalone runnable unlike 'package'
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.1")
