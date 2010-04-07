repositories.remote << 'http://www.ibiblio.org/maven2'
require 'buildr/scala'

layout = Layout.new
layout[:source, :main, :scala] = 'src'
layout[:source, :test, :scala] = 'test'

define 'tungsten', :layout=>layout do
  project.version = '0.3'
  package :jar
  test.using :junit
end
