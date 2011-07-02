{PlaceholderExpander} = require './placeholder'

path = require 'path'

coffee_script_lib_path = path.dirname require.resolve('coffee-script')

parser = require path.join(coffee_script_lib_path, 'parser')

originalParse = parser.parser.parse

parser.parser.parse = (input) ->
  (new PlaceholderExpander).expandPlaceholder(originalParse.call(parser.parser, input))
