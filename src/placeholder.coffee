# Scala-like placeholder syntax for CoffeeScript.
# ===============================================
#
# Expression contains unbound variables named “_” are converted to
# anonymous bound function.
#
# For example, `array.foldLeft(_ + _)` is converted to
# `array.foldLeft((a, b) => a + b)`.
#
# Note that bound variables are not placeholders,
# so that `(_) -> _` remains `(_) -> _`.
# Your existing code using `_` as a variable should not be affected.
#
# Scopes of Placeholders
# ----------------------
#
# Is `foo(_ + _, _)` converted to `(a, b, c) => foo(a + b, c)` or
# `(a) => foo(((b, c) => b + c), a)`?
# The answer is `(a) => foo(((b, c) => b + c), a)`,
# according to the definition of scopes described in this section.
#
# Scopes are expressions converted to the body of the generated
# anonymous functions.
# A scope of a placeholder is defined as the smallest expression
# which satisfies all of the following conditions:
#
# * It is not a placeholder itself.
# * It contains placeholders.
# * It is one of followings:
#   * An argument of a function call.
#   * A parenthetical expression.
#
# Examples:
#
#     console.log(_) == (a) => console.log a
#     array.foldLeft(_ + _) == array.foldLeft((a, b) => a + b)
#     (x = _ + _) == ((a, b) => x = a + b)
#     (x = (_ + _)) == (x = ((a, b) => a + b))
#
# Note the rules are much looser than the rules adopted in Scala.
#
# Special treatment for method definition
# ---------------------------------------
#
# Bound methods can be defined with placeholders.
#
# Example:
#
#     class Foo
#       constructor: (@foo) ->
#       bar: (@foo + _)
#       # is equivalent to
#       #
#       #   bar: (a) => @foo + a
#       #
#       # Note @ referes to an instance of Foo, not Foo.
#
# Notes on postfix form
# ---------------------
#
# Since current CoffeeScript compiler does not distinguish postfix form from
# normal form, parameter order is not intuitive:
#
#     (_ if _) == ((a, b) => b if a)
#     (_ while _) == ((a, b) => b while a)
#     (_ for x in _) == ((a, b) => b for x in a)
#
# This should be fixed on some future occasion.
#
# Notes on scope of bound variables
# ---------------------------------
#
# Since variable scope in CoffeeScript (and JavaScript) is entire function,
# all variables in following code are bound, so that they are not placeholders:
#
#     ->
#       x = (not _)
#       y = [_, _ = 1, _]
#       z = (_ + _)
#
#       # x is true, y is [1, 1, 1], z is 2
#
# Notes on interpolations
# -----------------------
#
# `"abc#{_ + _}def"` is converted to `"abc" + (_ + _) + "def"` by lexer,
# so that this is converted to `"abc" + ((a, b) -> a + b) + "def"`.
# This should be fixed on some future occasion.

PLACEHOLDER = '_'

path = require 'path'

coffee_script_lib_path = path.dirname require.resolve('coffee-script')

CoffeeScript = require 'coffee-script'
nodes = require path.join(coffee_script_lib_path, 'nodes')
{Scope} = require path.join(coffee_script_lib_path, 'scope')
{flatten, extend} = CoffeeScript.helpers

isPlaceholderValue = (node, o) ->
  node instanceof nodes.Value and
    not node.hasProperties() and
    isPlaceholderLiteral(node.base, o)

isPlaceholderLiteral = (node, o) ->
  node instanceof nodes.Literal and
    node.value is PLACEHOLDER and not o.scope.check(PLACEHOLDER)

isLiteralVariable = (node, o) ->
  node instanceof nodes.Value and
    not node.hasProperties() and
    node.base instanceof nodes.Literal

isPlaceholderSplat = (node, o) ->
  node instanceof nodes.Splat and
    isPlaceholderValue(node.name, o)

shallowCopy = (object) ->
  extend {}, object

shallowCopyArray = (array) ->
  extend [], array

# Populates scope with variables from left hand side of assignment,
# including function parameters, for variables, and catch clause variables.
#
# If the LHS is an array or an object,
# recursively traverse it to find all variables.
populateScopeWithVariablesFromLHS = (lhs, o) ->
  if not lhs
    return
  else if lhs.isAssignable()
    if lhs instanceof nodes.Splat
      populateScopeWithVariablesFromLHS(lhs.name, o)
    else if lhs instanceof nodes.Literal
      # We don't care its 'var' or 'param'
      # since only existence is significant.
      o.scope.add lhs.value, 'var'
    else if lhs.this or lhs.hasProperties()
      # this, @foo, or x.y.z.
      # not introducing variables. do nothing.
    else
      o.scope.add lhs.base.value, 'var'
  else if lhs instanceof nodes.Obj
    for propertyDefinition in lhs.objects
      if propertyDefinition instanceof nodes.Assign
        populateScopeWithVariablesFromLHS(propertyDefinition.value, o)
      else
        populateScopeWithVariablesFromLHS(propertyDefinition, o)
  else if lhs instanceof nodes.Arr
    populateScopeWithVariablesFromLHS(object, o) for object in lhs.objects
  else if lhs instanceof nodes.Value and lhs.isObject() or lhs.isArray()
    populateScopeWithVariablesFromLHS(lhs.base, o)
  else
    throw SyntaxError "not assignable"

cloneScope = (scope) ->
  if not scope
    return scope

  newParent = cloneScope(scope.parent)
  newScope = new Scope(newParent, scope.expressions, scope.method)

  newScope.shared = scope.shared
  newScope.variables = shallowCopyArray(scope.variables)
  newScope.positions = shallowCopy(scope.positions)
  newScope.hasAssignments = scope.hasAssignments
  newScope


# The subclass of **SubExpander** class expands placeholders in nodes of
# a certain type.
class SubExpander
  constructor: (@expander) ->

  # Populates scope with variables defined in the given node.
  populateScopeWithVariables: (node, o) ->
    for subNode in @getSubNodes(node, o)
      @expander.populateScopeWithVariables(subNode, o)

  # Returns number of unbound placeholders the given node contains.
  countUnboundPlaceholders: (node, o) ->
    count = 0
    for annotatedNode in @getAnnotatedSubNodes(node, o) \
        when not annotatedNode.isBindable
      count += @expander.countUnboundPlaceholders(annotatedNode.node, o)
    count

  # Replaces unbound placeholders with variables,
  # then expands sub nodes binding placeholders recursively.
  # The method removes variables from “variables” array.
  replacePlaceholders: (node, variables, o) ->
    newSubNodes = (for annotatedNode in @getAnnotatedSubNodes(node, o)
      if annotatedNode.isBindable
        @expander.doExpandPlaceholder(annotatedNode.node, o)
      else
        @expander.replacePlaceholders(annotatedNode.node, variables, o)
    )

    @createNewNode(node, newSubNodes, o)

  # Expands sub nodes recursively.
  expandSubNodes: (node, o) ->
    newSubNodes = (for annotatedNode in @getAnnotatedSubNodes(node, o)
        @expander.doExpandPlaceholder(annotatedNode.node, o)
    )
    @createNewNode(node, newSubNodes, o)

  # Get annotated sub nodes of the node given.
  #
  # Returns an array of annotated sub nodes.
  # Each element is an object with properties below:
  #   node: sub node
  #   isBindable: can the sub node bind placeholders
  getAnnotatedSubNodes: (node, o) ->
    subNodes = @getSubNodes(node, o)
    bindableFlags = @getBindingFlags(node, o)

    for index in [0...subNodes.length]
      subNode = subNodes[index]
      node: subNode
      isBindable: bindableFlags[index] and not isPlaceholderValue(subNode, o)

  # Get sub nodes of the node given.
  getSubNodes: (node, o) -> throw "abstract method called"

  # Get array of flags indicating each sub nodes can bind placeholders
  getBindingFlags: (node, o) -> throw "abstract method called"

  # Create a new node from the given node and subNodes
  # which does not bind placeholders
  createNewNode: (node, newSubNodes, o) -> throw "abstract method called"

  # Dynamically mixins another object.
  overrideWith: (other) -> extend(this, other)

# The **Case** class represents a case clause of a switch expression.
# The orignal AST holds case clauses as arrays.
# We make a separate class for convenience.
class Case
  constructor: (whens, @body) ->
    @whens = flatten([whens])

# Create sub expanders for each AST type.
createSubExpanders = (expander) ->
  blockExpander: new SubExpander(expander).overrideWith
    getSubNodes: (node, o) -> node.expressions
    getBindingFlags: (node, o) -> false for node in node.expressions
    createNewNode: (node, newSubNodes, o) -> new nodes.Block(newSubNodes)

  literalExpander:
    populateScopeWithVariables: (node, o) -> # do nothing
    countUnboundPlaceholders: (node, o) -> # do nothing
      if isPlaceholderLiteral(node, o) then 1 else 0

    replacePlaceholders: (node, variables, o) ->
      if isPlaceholderLiteral(node, o)
        new nodes.Literal(variables.shift())
      else
        node

    expandSubNodes: (node, o) -> node

  returnExpander: new SubExpander(expander).overrideWith
    getSubNodes: (node, o) -> [node.expression]
    getBindingFlags: (node, o) -> [false]
    createNewNode: (node, newSubNodes, o) -> new nodes.Return(newSubNodes[0])

  valueExpander: new SubExpander(expander).overrideWith
    getSubNodes: (node, o) -> [node.base].concat(node.properties)

    getBindingFlags: (node, o) ->
      [false].concat(false for property in node.properties)
    createNewNode: (node, newSubNodes, o) ->
      [newBase, newProperties...] = newSubNodes

      newValue = new nodes.Value(newBase, newProperties)
      extend(newValue, node) # Value does not hold tag as a value
                             # but as a property name,
                             # so that we copy all properties.
      newValue.base = newBase
      newValue.properties = newProperties
      newValue

  callExpander: new SubExpander(expander).overrideWith
    getSubNodes: (node, o) -> [node.variable].concat(node.args)
    getBindingFlags: (node, o) ->
      [false].concat(not isPlaceholderSplat(arg, o) for arg in node.args)

    createNewNode: (node, newSubNodes, o) ->
      [newVariable, newArgs...] = newSubNodes
      newVariable = (if node.isSuper then 'super' else newVariable)
      newNode = new nodes.Call(newVariable, newArgs, node.soak)
      newNode.newInstance() if node.isNew
      newNode

  extendsExpander: new SubExpander(expander).overrideWith
    getSubNodes: (node, o) -> [node.child, node.parent]
    getBindingFlags: (node, o) -> [false, false]
    createNewNode: (node, newSubNodes, o) ->
      new nodes.Extends(newSubNodes[0], newSubNodes[1])

  accessExpander: new SubExpander(expander).overrideWith
    getSubNodes: (node, o) -> []
    getBindingFlags: (node, o) -> []
    createNewNode: (node, newSubNodes, o) -> node

  indexExpander: new SubExpander(expander).overrideWith
    getSubNodes: (node, o) -> [node.index]
    getBindingFlags: (node, o) -> [false]
    createNewNode: (node, newSubNodes, o) ->
      newIndex = new nodes.Index(newSubNodes[0])
      newIndex.soak = node.soak
      newIndex.proto = node.proto
      newIndex

  rangeExpander: new SubExpander(expander).overrideWith
    getSubNodes: (node, o) -> [node.from, node.to]
    getBindingFlags: (node, o) -> [false, false]
    createNewNode: (node, newSubNodes, o) ->
      tag = (if node.exclusive then 'exclusive' else 'inclusive')
      new nodes.Range(newSubNodes[0], newSubNodes[1], tag)

  sliceExpander: new SubExpander(expander).overrideWith
    getSubNodes: (node, o) -> [node.range]
    getBindingFlags: (node, o) -> [false]
    createNewNode: (node, newSubNodes, o) -> new nodes.Slice(newSubNodes[0])

  objExpander: new SubExpander(expander).overrideWith
    getSubNodes: (node, o) -> node.objects
    getBindingFlags: (node, o) -> false for obj in node.objects
    createNewNode: (node, newSubNodes, o) ->
      new nodes.Obj(newSubNodes, node.generated)

  arrExpander: new SubExpander(expander).overrideWith
    getSubNodes: (node, o) -> node.objects
    getBindingFlags: (node, o) -> false for object in node.objects

    createNewNode: (node, newSubNodes, o) -> new nodes.Arr(newSubNodes)

  classExpander: new SubExpander(expander).overrideWith
    populateScopeWithVariables: (node, o) ->
      # class name is bound to outer scope
      populateScopeWithVariablesFromLHS(node.variable, o)
      # class has own scope.  stop recursion.

    # Since the scope of variables are entire class body,
    # so that we populate the scope with variables before
    # processing sub nodes.

    updateScope: (node, o) ->
      o.scope = new Scope o.scope, node.body, node

      # class name is bound to inner scope too.
      populateScopeWithVariablesFromLHS(node.variable, o)
      for subNode in @getSubNodes(node, o)
        @expander.populateScopeWithVariables(subNode, o)

    countUnboundPlaceholders: (node, o) ->
      @updateScope(node, o)
      SubExpander.prototype.countUnboundPlaceholders.call(this, node, o)

    replacePlaceholders: (node, variables, o) ->
      @updateScope(node, o)
      SubExpander.prototype.replacePlaceholders.call(this, node, variables, o)

    expandSubNodes: (node, o) ->
      @updateScope(node, o)
      SubExpander.prototype.expandSubNodes.call(this, node, o)

    getSubNodes: (node, o) ->
      if isLiteralVariable(node.variable, o)
        [node.parent, node.body]
      else
        [node.variable, node.parent, node.body]

    getBindingFlags: (node, o) ->
      if isLiteralVariable(node.variable, o)
        [false, false]
      else
        [false, false, false]

    createNewNode: (node, newSubNodes, o) ->
      if isLiteralVariable(node.variable, o)
        new nodes.Class(node.variable, newSubNodes[0], newSubNodes[1])
      else
        new nodes.Class(newSubNodes[0], newSubNodes[1], newSubNodes[2])

  assignExpander: new SubExpander(expander).overrideWith
    populateScopeWithVariables: (node, o) ->
      # only simple assignment (but not compond assignment) declare variables
      if not node.context
        populateScopeWithVariablesFromLHS(node.variable, o)
      SubExpander.prototype.populateScopeWithVariables.call(this, node, o)

    # Returns true if the assignment defines method with placeholder syntax,
    # example:
    #   class Foo
    #     constructor: (@foo) ->
    #     bar: (@foo + _)
    isDefiningBoundMethodWithPlaceholders: (node, o) ->
      node.context is 'object' and
        node.value instanceof nodes.Value and
        node.value.base instanceof nodes.Parens and
        @expander.countUnboundPlaceholders(node.value.base.body, o)

    expandSubNodes: (node, o) ->
      expanded = SubExpander.prototype.expandSubNodes.call(this, node, o)

      if @isDefiningBoundMethodWithPlaceholders(node, o)
        @tweakBoundMethodDefinition(expanded, o)
      else
        expanded

    # Unwraps Parens and Block since compiler treat bound method specially
    # but parenthetical bound function.
    # example:
    #   class Foo
    #     self1: => @ # returns an instance of Foo
    #     self2: (=> @) # returns Foo itself
    tweakBoundMethodDefinition: (expanded, o) ->
      # a block contains placeholders is transformed to a block containing
      # a function again.
      expanded.value = expanded.value.base.body.expressions[0]
      expanded

    getSubNodes: (node, o) ->
      if isLiteralVariable(node.variable, o)
        [node.value]
      else
        [node.variable, node.value]

    getBindingFlags: (node, o) ->
      if isLiteralVariable(node.variable, o)
        [false]
      else
        [false, false]

    createNewNode: (node, newSubNodes, o) ->
      if isLiteralVariable(node.variable, o)
        new nodes.Assign(node.variable, newSubNodes[0], node.context,
                         {param: node.param})
      else
        new nodes.Assign(newSubNodes[0], newSubNodes[1], node.context,
                         {param: node.param})

  codeExpander: new SubExpander(expander).overrideWith
    populateScopeWithVariables: (node, o) ->
      # code has own scope.  stop recursion.

    # Since the scope of variables are entire function body,
    # so that we populate the scope with variables before
    # processing sub nodes.

    updateScope: (node, o) ->
      o.scope = new Scope o.scope, node.body, node
      @populateScopeWithParameters(node, o)
      for subNode in @getSubNodes(node, o)
        @expander.populateScopeWithVariables(subNode, o)

    countUnboundPlaceholders: (node, o) ->
      @updateScope(node, o)
      SubExpander.prototype.countUnboundPlaceholders.call(this, node, o)

    replacePlaceholders: (node, variables, o) ->
      @updateScope(node, o)
      SubExpander.prototype.replacePlaceholders.call(this, node, variables, o)

    expandSubNodes: (node, o) ->
      @updateScope(node, o)
      SubExpander.prototype.expandSubNodes.call(this, node, o)

    populateScopeWithParameters: (node, o) ->
      for param in node.params
        populateScopeWithVariablesFromLHS(param.name, o)

    getSubNodes: (node, o) -> node.params.concat([node.body])

    getBindingFlags: (node, o) ->
      (false for param in node.params).concat([false])

    createNewNode: (node, newSubNodes, o) ->
      [newParams..., newBody] = newSubNodes
      new nodes.Code(newParams,
                     newBody,
                     if node.bound then 'boundfunc' else 'func')

  paramExpander: new SubExpander(expander).overrideWith
    getSubNodes: (node, o) -> [node.value]
    getBindingFlags: (node, o) -> [false]
    createNewNode: (node, newSubNodes, o) ->
      new nodes.Param(node.name, newSubNodes[0], node.splat)

  splatExpander: new SubExpander(expander).overrideWith
    getSubNodes: (node, o) -> [node.name]
    getBindingFlags: (node, o) -> [false]
    createNewNode: (node, newSubNodes, o) ->
      new nodes.Splat(newSubNodes[0])

  whileExpander: new SubExpander(expander).overrideWith
    getSubNodes: (node, o) -> [node.condition, node.body]
    getBindingFlags: (node, o) -> [false, false]
    createNewNode: (node, newSubNodes, o) ->
      newNode = new nodes.While(newSubNodes[0]).addBody(newSubNodes[1])
      newNode.makeReturn if node.returns
      newNode.guard = node.guard
      newNode

  opExpander: new SubExpander(expander).overrideWith
    getSubNodes: (node, o) -> [node.first, node.second]
    getBindingFlags: (node, o) -> [false, false]
    createNewNode: (node, newSubNodes, o) ->
      operator = @CONVERSIONS[node.operator] or node.operator
      newNode =
        new nodes.Op(operator, newSubNodes[0], newSubNodes[1], node.flip)
      newNode.invert = node.invert
      newNode

    CONVERSIONS:
      '===': '=='
      '!==': '!='
      'in': 'of'

  inExpander: new SubExpander(expander).overrideWith
    getSubNodes: (node, o) -> [node.object, node.array]
    getBindingFlags: (node, o) -> [false, false]
    createNewNode: (node, newSubNodes, o) ->
      newNode = new nodes.In(newSubNodes[0], newSubNodes[1])
      newNode.negated = node.negated
      newNode

  tryExpander: new SubExpander(expander).overrideWith
    populateScopeWithVariables: (node, o) ->
      if node.error
        populateScopeWithVariablesFromLHS(node.error, o)

    getSubNodes: (node, o) -> [node.attempt, node.recovery, node.ensure]
    getBindingFlags: (node, o) ->
      [false, false, false]
    createNewNode: (node, newSubNodes, o) ->
      new nodes.Try(newSubNodes[0], node.error, newSubNodes[1], newSubNodes[2])

  throwExpander: new SubExpander(expander).overrideWith
    getSubNodes: (node, o) -> [node.expression]
    getBindingFlags: (node, o) -> [false]
    createNewNode: (node, newSubNodes, o) -> new nodes.Throw(newSubNodes[0])

  existenceExpander: new SubExpander(expander).overrideWith
    getSubNodes: (node, o) -> [node.expression]
    getBindingFlags: (node, o) -> [false]
    createNewNode: (node, newSubNodes, o) ->
      newNode = new nodes.Existence(newSubNodes[0])
      newNode.negated = node.negated
      newNode

  parensExpander: new SubExpander(expander).overrideWith
    getSubNodes: (node, o) -> [node.body]
    getBindingFlags: (node, o) -> [true]
    createNewNode: (node, newSubNodes, o) -> new nodes.Parens(newSubNodes[0])

  forExpander: new SubExpander(expander).overrideWith
    populateScopeWithVariables: (node, o) ->
      populateScopeWithVariablesFromLHS(node.name, o)
      populateScopeWithVariablesFromLHS(node.index, o)
      SubExpander.prototype.populateScopeWithVariables.call(this, node, o)

    getSubNodes: (node, o) -> [node.body, node.source ,node.guard, node.step]
    getBindingFlags: (node, o) -> [false, false, false, false]
    createNewNode: (node, newSubNodes, o) ->
      source =
        source: newSubNodes[1]
        guard: newSubNodes[2]
        step: newSubNodes[3]
        name: if node.object then node.index else node.name
        index: if node.object then node.name else node.index
        own: node.own
        object: node.object
      new nodes.For(newSubNodes[0], source)

  switchExpander: new SubExpander(expander).overrideWith
    getSubNodes: (node, o) ->
      cases = (new Case(caseNode[0], caseNode[1]) for caseNode in node.cases)
      [node.subject].concat(cases).concat([node.otherwise])

    getBindingFlags: (node, o) ->
      [false].concat(false for caseNode in node.cases).concat([false])

    createNewNode: (node, newSubNodes, o) ->
      [newSubject, newCaseNodes..., newOtherwise] = newSubNodes

      newCases =
        ([caseNode.whens, caseNode.body] for caseNode in newCaseNodes)
      new nodes.Switch(newSubject,
                       newCases,
                       newOtherwise)

  caseExpander: new SubExpander(expander).overrideWith
    getSubNodes: (node, o) -> node.whens.concat([node.body])
    getBindingFlags: (node, o) ->
      (false for whenNode in node.whens).concat([false])

    createNewNode: (node, newSubNodes, o) ->
      [newWhens..., newBody] = newSubNodes

      new Case(newWhens, newBody)

  ifExpander: new SubExpander(expander).overrideWith
    getSubNodes: (node, o) -> [node.condition, node.body, node.elseBody]
    getBindingFlags: (node, o) -> [false, false, false]
    createNewNode: (node, newSubNodes, o) ->
      ifNode = new nodes.If(newSubNodes[0], newSubNodes[1], {soak: node.soak})
      ifNode.addElse(newSubNodes[2]) if newSubNodes[2]?
      ifNode.isChain = node.isChain
      ifNode

# The **PlaceholderExpander** expands placeholders in the given node.
# The expander delegates its job to the sub expanders.
class exports.PlaceholderExpander
  constructor: ->
    @subExpanders = createSubExpanders(this)

  expandPlaceholder: (node, o = {}) ->
    o.scope = new Scope null, node, null

    try
      @populateScopeWithVariables(node, o)
      @doExpandPlaceholder(node, o)
    catch error
      if error instanceof SyntaxError
        return node
      else
        throw error

  doExpandPlaceholder: (node, o) ->
    subExpander = @getSubExpander(node, o)

    originalO = o
    o = shallowCopy o
    o.scope = cloneScope(o.scope)

    count = subExpander.countUnboundPlaceholders(node, o)

    o = shallowCopy originalO

    if count
      variables =
        (o.scope.freeVariable('a') for i in [0...count])

      parameters =
        (new nodes.Param(new nodes.Literal(variable)) for variable in variables)

      body =
        new nodes.Block([subExpander.replacePlaceholders(node, variables, o)])

      code = new nodes.Code(parameters, body, 'boundfunc')

      if node instanceof nodes.Block
        new nodes.Block([code])
      else
        code
    else
      subExpander.expandSubNodes(node, o)

  countUnboundPlaceholders: (node, o) ->
    o = shallowCopy o
    subExpander = @getSubExpander(node, o)
    subExpander.countUnboundPlaceholders(node, o)

  replacePlaceholders: (node, variables, o) ->
    o = shallowCopy o
    subExpander = @getSubExpander(node, o)
    subExpander.replacePlaceholders(node, variables, o)

  populateScopeWithVariables: (node, o) ->
    subExpander = @getSubExpander(node, o)
    subExpander.populateScopeWithVariables(node, o)

  # Fallback sub expander.
  NULL_SUB_EXPANDER =
    populateScopeWithVariables: (node, o) -> # do nothing
    countUnboundPlaceholders: (node, o) -> 0
    replacePlaceholders: (node, variables, o) -> node
    expandSubNodes: (node, o) -> node

  getSubExpander: (node, o) ->
    return NULL_SUB_EXPANDER if not node?

    switch node.constructor
      when nodes.Block then @subExpanders.blockExpander
      when nodes.Literal then @subExpanders.literalExpander
      when nodes.Return then @subExpanders.returnExpander
      when nodes.Value then @subExpanders.valueExpander
      when nodes.Call then @subExpanders.callExpander
      when nodes.Extends then @subExpanders.extendsExpander
      when nodes.Access then @subExpanders.accessExpander
      when nodes.Index then @subExpanders.indexExpander
      when nodes.Range then @subExpanders.rangeExpander
      when nodes.Slice then @subExpanders.sliceExpander
      when nodes.Obj then @subExpanders.objExpander
      when nodes.Arr then @subExpanders.arrExpander
      when nodes.Class then @subExpanders.classExpander
      when nodes.Assign then @subExpanders.assignExpander
      when nodes.Code then @subExpanders.codeExpander
      when nodes.Param then @subExpanders.paramExpander
      when nodes.Splat then @subExpanders.splatExpander
      when nodes.While then @subExpanders.whileExpander
      when nodes.Op then @subExpanders.opExpander
      when nodes.In then @subExpanders.inExpander
      when nodes.Try then @subExpanders.tryExpander
      when nodes.Throw then @subExpanders.throwExpander
      when nodes.Existence then @subExpanders.existenceExpander
      when nodes.Parens then @subExpanders.parensExpander
      when nodes.For then @subExpanders.forExpander
      when nodes.Switch then @subExpanders.switchExpander
      when Case then @subExpanders.caseExpander
      when nodes.If then @subExpanders.ifExpander
      else NULL_SUB_EXPANDER
