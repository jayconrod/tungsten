PATH=$PATH:`pwd`/core/src/main/resources:`pwd`/llvm/src/main/resources
SCALA_HOME=~/.m2/repository/org/scala-lang/scala-library/2.8.1
export CLASSPATH=$SCALA_HOME/scala-library-2.8.1.jar:~/.m2/repository/tungsten/tungsten-core/0.3/tungsten-core-0.3.jar:~/.m2/repository/tungsten/tungsten-llvm/0.3/tungsten-llvm-0.3.jar:`pwd`/core/target/test/classes:`pwd`/llvm/target/test/classes:~/.m2/repository/junit/junit/4.5/junit-4.5.jar
