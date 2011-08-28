repositories.remote << 'http://www.ibiblio.org/maven2'
Buildr.settings.build['scala.version'] = '2.8.1'
require 'buildr/scala'

ENV['USE_FSC'] = 'yes'

define 'tungsten' do
  puts Scala.version
  project.version = '0.5'
  project.group = 'tungsten'

  define 'core' do
    package :jar
    test.using :junit, :fork => false, :clonevm => false
  end

  define 'llvm' do
    package :jar
    test.using :junit, :fork => false, :clonevm => false
    compile.with project('tungsten:core')
  end
end
