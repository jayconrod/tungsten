repositories.remote << 'http://www.ibiblio.org/maven2'
require 'buildr/scala'

define 'tungsten' do
  puts Scala.version
  project.version = '0.3'
  project.group = 'tungsten'

  define 'core' do
    package :jar
    test.using :junit
  end

  define 'llvm' do
    package :jar
    test.using :junit
    compile.with project('tungsten:core')
  end
end
