repositories.remote << 'http://www.ibiblio.org/maven2'
require 'buildr/scala'

define 'tungsten' do
  project.version = '0.3'
  package :jar
  test.using :junit
end
