# encoding: UTF-8

require 'strscan'
require 'pathname'
require 'citrus/version'
require 'set'

# Citrus is a compact and powerful parsing library for Ruby that combines the
# elegance and expressiveness of the language with the simplicity and power of
# parsing expressions.
#
# http://mjijackson.com/citrus
module Citrus
  autoload :File, 'citrus/file'

  # A pattern to match any character, including newline.
  DOT = /./mu

  Infinity = 1.0 / 0

  CLOSE = -1

  # Returns a map of paths of files that have been loaded via #load to the
  # result of #eval on the code in that file.
  #
  # Note: These paths are not absolute unless you pass an absolute path to
  # #load. That means that if you change the working directory and try to
  # #require the same file with a different relative path, it will be loaded
  # twice.
  def self.cache
    @cache ||= {}
  end

  # Evaluates the given Citrus parsing expression grammar +code+ and returns an
  # array of any grammar modules that are created. Accepts the same +options+ as
  # GrammarMethods#parse.
  #
  #     Citrus.eval(<<CITRUS)
  #     grammar MyGrammar
  #       rule abc
  #         "abc"
  #       end
  #     end
  #     CITRUS
  #     # => [MyGrammar]
  #
  def self.eval(code, options={})
    File.parse(code, options).value
  end

  # Evaluates the given expression and creates a new Rule object from it.
  # Accepts the same +options+ as #eval.
  #
  #     Citrus.rule('"a" | "b"')
  #     # => #<Citrus::Rule: ... >
  #
  def self.rule(expr, options={})
    eval(expr, options.merge(:root => :expression))
  end

  # Loads the grammar(s) from the given +file+. Accepts the same +options+ as
  # #eval, plus the following:
  #
  # force::   Normally this method will not reload a file that is already in
  #           the #cache. However, if this option is +true+ the file will be
  #           loaded, regardless of whether or not it is in the cache. Defaults
  #           to +false+.
  #
  #     Citrus.load('mygrammar')
  #     # => [MyGrammar]
  #
  def self.load(file, options={})
    file += '.citrus' unless /\.citrus$/ === file
    force = options.delete(:force)

    if force || !cache[file]
      raise LoadError, "Cannot find file #{file}" unless ::File.file?(file)
      raise LoadError, "Cannot read file #{file}" unless ::File.readable?(file)

      begin
        cache[file] = eval(::File.read(file), options)
      rescue SyntaxError => e
        e.message.replace("#{::File.expand_path(file)}: #{e.message}")
        raise e
      end
    end

    cache[file]
  end

  # Searches the <tt>$LOAD_PATH</tt> for a +file+ with the .citrus suffix and
  # attempts to load it via #load. Returns the path to the file that was loaded
  # on success, +nil+ on failure. Accepts the same +options+ as #load.
  #
  #     path = Citrus.require('mygrammar')
  #     # => "/path/to/mygrammar.citrus"
  #     Citrus.cache[path]
  #     # => [MyGrammar]
  #
  def self.require(file, options={})
    file += '.citrus' unless /\.citrus$/ === file
    found = nil

    paths = ['']
    paths += $LOAD_PATH unless Pathname.new(file).absolute?
    paths.each do |path|
      found = Dir[::File.join(path, file)].first
      break if found
    end

    if found
      Citrus.load(found, options)
    else
      raise LoadError, "Cannot find file #{file}"
    end

    found
  end

  # A base class for all Citrus errors.
  class Error < RuntimeError; end

  # Raised when a parse fails.
  class ParseError < Error
    # The +input+ given here is an instance of Citrus::Input.
    def initialize(input)
      @offset = input.max_offset
      @line_offset = input.line_offset(offset)
      @line_number = input.line_number(offset)
      @line = input.line(offset)

      message = "Failed to parse input on line #{line_number}"
      message << " at offset #{line_offset}\n#{detail}"

      super(message)
    end

    # The 0-based offset at which the error occurred in the input, i.e. the
    # maximum offset in the input that was successfully parsed before the error
    # occurred.
    attr_reader :offset

    # The 0-based offset at which the error occurred on the line on which it
    # occurred in the input.
    attr_reader :line_offset

    # The 1-based number of the line in the input where the error occurred.
    attr_reader :line_number

    # The text of the line in the input where the error occurred.
    attr_reader :line

    # Returns a string that, when printed, gives a visual representation of
    # exactly where the error occurred on its line in the input.
    def detail
      "#{line}\n#{' ' * line_offset}^"
    end
  end

  # Raised when Citrus.load fails to load a file.
  class LoadError < Error; end

  # Raised when Citrus::File.parse fails.
  class SyntaxError < Error
    # The +error+ given here is an instance of Citrus::ParseError.
    def initialize(error)
      message = "Malformed Citrus syntax on line #{error.line_number}"
      message << " at offset #{error.line_offset}\n#{error.detail}"

      super(message)
    end
  end

  # An Input is a scanner that is responsible for executing rules at different
  # positions in the input string and persisting event streams.
  class Input < StringScanner
    def initialize(string)
      super(string)
      @max_offset = 0
    end

    # The maximum offset in the input that was successfully parsed.
    attr_reader :max_offset

    def reset # :nodoc:
      @max_offset = 0
      super
    end

    # Returns an array containing the lines of text in the input.
    def lines
      if string.respond_to?(:lines)
        string.lines.to_a
      else
        string.to_a
      end
    end

    # Returns the 0-based offset of the given +pos+ in the input on the line
    # on which it is found. +pos+ defaults to the current pointer position.
    def line_offset(pos=pos)
      p = 0
      string.each_line do |line|
        len = line.length
        return (pos - p) if p + len >= pos
        p += len
      end
      0
    end

    # Returns the 0-based number of the line that contains the character at the
    # given +pos+. +pos+ defaults to the current pointer position.
    def line_index(pos=pos)
      p = n = 0
      string.each_line do |line|
        p += line.length
        return n if p >= pos
        n += 1
      end
      0
    end

    # Returns the 1-based number of the line that contains the character at the
    # given +pos+. +pos+ defaults to the current pointer position.
    def line_number(pos=pos)
      line_index(pos) + 1
    end

    alias_method :lineno, :line_number

    # Returns the text of the line that contains the character at the given
    # +pos+. +pos+ defaults to the current pointer position.
    def line(pos=pos)
      lines[line_index(pos)]
    end

    # Returns +true+ when using memoization to cache match results.
    def memoized?
      false
    end

    # Returns an array of events for the given +rule+ at the current pointer
    # position. Objects in this array may be one of three types: a Rule,
    # Citrus::CLOSE, or a length (integer).
    def exec(rule)
      position = pos

      # a side effect of calling apply_rule is that the input position (self.pos) may be modified
      parse_tree = apply_rule(rule, position)

      # if the rule matches some input, then update the max_offset
      if parse_tree.size > 0
        @max_offset = pos if pos > @max_offset
      else
        # if the rule does NOT match any input, then reset the StringScanner
        # position back to what it was before the rule was invoked
        self.pos = position
      end

      parse_tree
    end

    # Returns the length of a match for the given +rule+ at the current pointer
    # position, +nil+ if none can be made.
    def test(rule)
      position = pos
      parse_tree = apply_rule(rule, position)
      self.pos = position
      if parse_tree.size > 0
        parse_tree[-1]
      else
        0
      end
    end

  private

    # Appends all events for +rule+ at the given +position+ to +events+.
    def apply_rule(rule, position)
      rule.exec(self)
    end
  end

  # A MemoizedInput is an Input that caches segments of the event stream for
  # particular rules in a parse. This technique (also known as "Packrat"
  # parsing) guarantees parsers will operate in linear time but costs
  # significantly more in terms of time and memory required to perform a parse.
  # For more information, please read the paper on Packrat parsing at
  # http://pdos.csail.mit.edu/~baford/packrat/icfp02/.
  class MemoizedInput < Input
    def initialize(string)
      super(string)
      @cache = {}
      @cache_hits = 0
    end

    # A nested hash of rules to offsets and their respective matches.
    attr_reader :cache

    # The number of times the cache was hit.
    attr_reader :cache_hits

    def reset # :nodoc:
      @cache.clear
      @cache_hits = 0
      super
    end

    # Returns +true+ when using memoization to cache match results.
    def memoized?
      true
    end

  private

    def apply_rule(rule, position) # :nodoc:
      events = []
      
      memo = @cache[rule] ||= {}

      if memo[position]
        @cache_hits += 1
        c = memo[position]
        unless c.empty?
          events = c
          self.pos += events[-1]
        end
      else
        index = events.size
        events = rule.exec(self)

        # Memoize the result so we can use it next time this same rule is executed at this position.
        memo[position] = events
      end

      events
    end
  end
  
  class LeftRecursiveMemoizedInput < MemoizedInput
    def initialize(string)
      super(string)
      @count = 0
      @memoization_enabled = true
      @call_stack = []
      @call_stack_indices = Hash.new {|hash, key| hash[key] = Set.new }
    end
    
    # call_stack is a stack [<R, P>, ...] that stores a list of (rule, position) pairs
    # representing each rule invocation.
    attr_reader :call_stack
    
    # call_stack_indices is a map <[R, P], Set<integer>> from a [rule, position] pair to
    # a set of index positions, each of which is an index at which that [rule, position]
    # pair occurs in the call_stack
    attr_reader :call_stack_indices
    
    attr_reader :memoization_enabled

    def reset # :nodoc:
      @memoization_enabled = true
      @call_stack.clear
      @call_stack_indices.clear
      super
    end
    
    # def exec(rule, events=[])
    #   events.push([rule, pos]) if events.empty?
    #   super
    # end
    
  private

    def this_is_the_only_recursive_call_in_the_call_stack?(rule, position)
      call_stack_indices[ [rule, position] ].size == 1
    end

    def enable_memoization
      @memoization_enabled = true
    end

    def disable_memoization
      @memoization_enabled = false
    end

    def apply_rule(rule, position, events) # :nodoc:
      @count += 1
      count = @count
      
      memo = @cache[rule] ||= {}

      puts "apply_rule(#{rule} - ##{rule.object_id}, #{position}, #{events})"
      # puts '---'
      # puts "count = #{count}"
      # puts "name = #{rule.name}"
      # puts rule.inspect
      # puts rule.class
      # puts position
      # puts events.inspect

      this_is_a_left_recursive_call = call_stack_includes?(rule, position)

      call_stack_push(rule, position)
      call_stack_index = call_stack.size - 1

      if memo[position]
        # puts "count = #{count}"
        puts "cache hit! cache[#{rule}][#{position}] = #{memo[position]}"
        @cache_hits += 1
        c = memo[position]
        unless c.empty?
          events.concat(c)
          self.pos += events[-1]
        end
      else
        # This is Algorithm 1 from 
        # http://tratt.net/laurie/research/publications/papers/tratt__direct_left_recursive_parsing_expression_grammars.pdf
        case
          # left-recursion has not yet occurred but is about to begin - this is triggered when
          #   no input has been consumed, detected when P has not advanced over orig_pos
          # when rule == orig_rule && position = orig_pos
          when this_is_a_left_recursive_call
            # Set up the base case (the non-recursive case):
            # initialize the seed value to nil so that if / when left-recursion happens for 
            #   this rule at this input position, the initial left-recursion will fail.
            # store an empty parse tree - Tratt uses 'null' to represent an empty tree
            memo[position] = [] if memoization_enabled
            puts "null memoizing ##{rule.object_id} #{rule.name} - #{rule.inspect} at position #{position}"  if memoization_enabled

            disable_memoization
            
            # we are now switching parsing modes, changing from top-down mode to Warth-style
            #   iterative bottom-up mode.
            # From Tratt's paper:
            #   In essence, we continually re-evaluate the rule 'rule' at input position 'position' 
            #   (note that 'position' does not advance); each time this re-evaluation is successful,
            #   we update the seed in memo[position]. As expected, this means that each 
            #   update of the seed includes within it the previous seed.
            while true
              puts 'lr rule application'

              # invoke the same rule another recursive time (the current invocation is the first recursive time)
              # this next invocation will be the 2nd or 3rd or 4th or .... or nth recursive invocation
              index = events.size
              rule.exec(self, events)
              result = events.slice(index, events.size)

              seed = memo[position]
              
              # if the invocation of the parent rule was a recursive invocation, then memoization has been turned
              # off, so the seed will be nil for this child rule's invocation, but the child rule is also treated as
              # a recursive invocation because it appears at least once before in the call stack. We want to turn
              # off position backtracking (lines 471 and 472) for all rules except the one original recursive call.
              break if seed.nil?

              # This explains the following 'if' statement:
              # Re-evaluation of 'rule' at input position 'position' can be unsuccessful for two reasons: if the
              # re-evaluation fails completely; OR if the result returned by re-evaluation consumes less
              # of the input than the current seed (if one exists). The former case is trivial (though
              # note that, by definition, it can only trigger on the first attempt at left-recursion). The
              # latter is less obvious, and is not explained in depth by Warth et al. Intuitively, if a left-
              # recursive call returns a 'shorter' result than the previous known one, then by definition
              # it has not used the current seed; in other words, the left-recursion must have exhausted
              # itself.
              puts "result = #{result.inspect}"
              puts "seed = cache[#{rule} - ##{rule.object_id}][#{position}] = #{seed.inspect}"
              if result.empty? ||                   # case 1: evaluation has failed completely
                 result[-1] < (seed[-1] || 0)       # case 2: result consumes less input than the current seed
                 
                enable_memoization if this_is_the_only_recursive_call_in_the_call_stack?(rule, position)
                
                puts 'RETURNING'
                puts "events = #{events.inspect}"
                events.slice!(index, events.size)     # remove whatever result was appended to events
                puts "events = #{events.inspect}"
                puts "position = #{position}"
                puts "pos = #{self.pos}"
                unless seed.empty?
                  events.concat(seed)
                  self.pos = seed[-1]
                end
                puts "events = #{events.inspect}"
                puts "position = #{position}"
                puts "pos = #{self.pos}"

                break
              end

              memo[position] = result
              
              # backtrack and run the same rule again
              self.pos = position                   # we must backtrack to the original position, 'position'
              events.slice!(index, events.size)     # remove whatever result was appended to events
            end

          # left-recursion is not occuring - rule is not calling itself left-recursively
          # do do everything normally - traditional PEG rule application
          else
            puts 'normal rule application'
            
            index = events.size
            rule.exec(self, events)
          
            # if the rule failed, but there is a non-empty memoized AST for this rule or one of it's parents
            # then retry the rule
            if index == events.size &&                        # the rule failed
               memo[position] && !memo[position].empty?
               # call_stack_ancestor_has_memoized_result(call_stack_index)
              # i = call_stack_ancestor_has_memoized_result(call_stack_index)
              # memo_rule, memo_pos = call_stack[i]
              # events.concat(cache[memo_rule][memo_pos])
              # self.pos = events[-1]

              events.concat(memo[position])
              self.pos = events[-1]
            end
            
            if memoization_enabled
              puts "memoizing ##{rule.object_id} #{rule.name} - #{rule.inspect} as #{events.slice(index, events.size)} at position #{position}"
              # Memoize the result so we can use it next time this same rule is
              # executed at this position.
              memo[position] = events.slice(index, events.size)
            end
        end
      end
      
      call_stack_pop

      events
    end
    
    # returns the index into call_stack at which a rule/position pair exists that references a rule
    #   with a memoized result.
    # returns nil if no such index is found.
    def call_stack_ancestor_has_memoized_result(call_stack_index)
      (0..call_stack_index).to_a.reverse.detect do |i|
        rule, pos = call_stack[i]
        seed = cache[rule][pos]
        seed && !seed.empty?          # there is a non-empty memoized AST
      end
    end
    
    def call_stack_includes?(rule, position)
      !call_stack_indices[ [rule, position] ].empty?
    end

    def call_stack_push(rule, position)
      rule_pos_pair = [rule, position]
      
      # push the given rule and position onto the call stack
      call_stack.push rule_pos_pair
      
      # add the call stack index at which the pair was just added to the set of indices at which the pair exists in the call stack
      call_stack_indices[rule_pos_pair] << call_stack.length - 1
      
      call_stack
    end
    
    def call_stack_pop
      pair = call_stack.pop
      call_stack_indices[pair].delete(call_stack.length)
      pair
    end
  end

  # Inclusion of this module into another extends the receiver with the grammar
  # helper methods in GrammarMethods. Although this module does not actually
  # provide any methods, constants, or variables to modules that include it, the
  # mere act of inclusion provides a useful lookup mechanism to determine if a
  # module is in fact a grammar.
  module Grammar
    # Creates a new anonymous module that includes Grammar. If a +block+ is
    # provided, it is +module_eval+'d in the context of the new module. Grammars
    # created with this method may be assigned a name by being assigned to some
    # constant, e.g.:
    #
    #     MyGrammar = Citrus::Grammar.new {}
    #
    def self.new(&block)
      mod = Module.new { include Grammar }
      mod.module_eval(&block) if block
      mod
    end

    # Extends all modules that +include Grammar+ with GrammarMethods and
    # exposes Module#include.
    def self.included(mod)
      mod.extend(GrammarMethods)
      # Expose #include so it can be called publicly.
      class << mod; public :include end
    end
  end

  # Contains methods that are available to Grammar modules at the class level.
  module GrammarMethods
    def self.extend_object(obj)
      raise ArgumentError, "Grammars must be Modules" unless Module === obj
      super
    end

    # Parses the given +string+ using this grammar's root rule. Accepts the same
    # +options+ as Rule#parse, plus the following:
    #
    # root::    The name of the root rule to start parsing at. Defaults to this
    #           grammar's #root.
    def parse(string, options={})
      rule_name = options.delete(:root) || root
      raise Error, "No root rule specified" unless rule_name
      rule = rule(rule_name)
      raise Error, "No rule named \"#{rule_name}\"" unless rule
      rule.parse(string, options)
    end

    # Returns the name of this grammar as a string.
    def name
      super.to_s
    end

    # Returns an array of all grammars that have been included in this grammar
    # in the reverse order they were included.
    def included_grammars
      included_modules.select {|mod| mod.include?(Grammar) }
    end

    # Returns an array of all names of rules in this grammar as symbols ordered
    # in the same way they were declared.
    def rule_names
      @rule_names ||= []
    end

    # Returns a hash of all Rule objects in this grammar, keyed by rule name.
    def rules
      @rules ||= {}
    end

    # Returns +true+ if this grammar has a rule with the given +name+.
    def has_rule?(name)
      rules.key?(name.to_sym)
    end

    # Loops through the rule tree for the given +rule+ looking for any Super
    # rules. When it finds one, it sets that rule's rule name to the given
    # +name+.
    def setup_super(rule, name) # :nodoc:
      if Nonterminal === rule
        rule.rules.each {|r| setup_super(r, name) }
      elsif Super === rule
        rule.rule_name = name
      end
    end
    private :setup_super

    # Searches the inheritance hierarchy of this grammar for a rule named +name+
    # and returns it on success. Returns +nil+ on failure.
    def super_rule(name)
      sym = name.to_sym
      included_grammars.each do |grammar|
        rule = grammar.rule(sym)
        return rule if rule
      end
      nil
    end

    # Gets/sets the rule with the given +name+. If +obj+ is given the rule
    # will be set to the value of +obj+ passed through Rule.for. If a block is
    # given, its return value will be used for the value of +obj+.
    #
    # It is important to note that this method will also check any included
    # grammars for a rule with the given +name+ if one cannot be found in this
    # grammar.
    def rule(name, obj=nil, &block)
      sym = name.to_sym
      obj = block.call if block

      if obj
        rule_names << sym unless has_rule?(sym)

        rule = Rule.for(obj)
        rule.name = name
        setup_super(rule, name)
        rule.grammar = self

        rules[sym] = rule
      end

      rules[sym] || super_rule(sym)
    rescue => e
      e.message.replace("Cannot create rule \"#{name}\": #{e.message}")
      raise e
    end

    # Gets/sets the +name+ of the root rule of this grammar. If no root rule is
    # explicitly specified, the name of this grammar's first rule is returned.
    def root(name=nil)
      @root = name.to_sym if name
      # The first rule in a grammar is the default root.
      @root || rule_names.first
    end

    # Creates a new rule that will match any single character. A block may be
    # provided to specify semantic behavior (via #ext).
    def dot(&block)
      ext(Rule.for(DOT), block)
    end

    # Creates a new Super for the rule currently being defined in the grammar. A
    # block may be provided to specify semantic behavior (via #ext).
    def sup(&block)
      ext(Super.new, block)
    end

    # Creates a new AndPredicate using the given +rule+. A block may be provided
    # to specify semantic behavior (via #ext).
    def andp(rule, &block)
      ext(AndPredicate.new(rule), block)
    end

    # Creates a new NotPredicate using the given +rule+. A block may be provided
    # to specify semantic behavior (via #ext).
    def notp(rule, &block)
      ext(NotPredicate.new(rule), block)
    end

    # Creates a new ButPredicate using the given +rule+. A block may be provided
    # to specify semantic behavior (via #ext).
    def butp(rule, &block)
      ext(ButPredicate.new(rule), block)
    end

    # Creates a new Repeat using the given +rule+. +min+ and +max+ specify the
    # minimum and maximum number of times the rule must match. A block may be
    # provided to specify semantic behavior (via #ext).
    def rep(rule, min=1, max=Infinity, &block)
      ext(Repeat.new(rule, min, max), block)
    end

    # An alias for #rep.
    def one_or_more(rule, &block)
      rep(rule, &block)
    end

    # An alias for #rep with a minimum of 0.
    def zero_or_more(rule, &block)
      rep(rule, 0, &block)
    end

    # An alias for #rep with a minimum of 0 and a maximum of 1.
    def zero_or_one(rule, &block)
      rep(rule, 0, 1, &block)
    end

    # Creates a new Sequence using all arguments. A block may be provided to
    # specify semantic behavior (via #ext).
    def all(*args, &block)
      ext(Sequence.new(args), block)
    end

    # Creates a new Choice using all arguments. A block may be provided to
    # specify semantic behavior (via #ext).
    def any(*args, &block)
      ext(Choice.new(args), block)
    end

    # Adds +label+ to the given +rule+. A block may be provided to specify
    # semantic behavior (via #ext).
    def label(rule, label, &block)
      rule = ext(rule, block)
      rule.label = label
      rule
    end

    # Specifies a Module that will be used to extend all matches created with
    # the given +rule+. A block may also be given that will be used to create
    # an anonymous module. See Rule#extension=.
    def ext(rule, mod=nil, &block)
      rule = Rule.for(rule)
      mod = block if block
      rule.extension = mod if mod
      rule
    end

    # Creates a new Module from the given +block+ and sets it to be the
    # extension of the given +rule+. See Rule#extension=.
    def mod(rule, &block)
      rule.extension = Module.new(&block)
      rule
    end
  end

  # A Rule is an object that is used by a grammar to create matches on an
  # Input during parsing.
  module Rule
    # Returns a new Rule object depending on the type of object given.
    def self.for(obj)
      case obj
      when Rule     then obj
      when Symbol   then Alias.new(obj)
      when String   then StringTerminal.new(obj)
      when Regexp   then Terminal.new(obj)
      when Array    then Sequence.new(obj)
      when Range    then Choice.new(obj.to_a)
      when Numeric  then StringTerminal.new(obj.to_s)
      else
        raise ArgumentError, "Invalid rule object: #{obj.inspect}"
      end
    end

    # The grammar this rule belongs to, if any.
    attr_accessor :grammar

    # Sets the name of this rule.
    def name=(name)
      @name = name.to_sym
    end

    # The name of this rule.
    attr_reader :name

    # Sets the label of this rule.
    def label=(label)
      @label = label.to_sym
    end

    # A label for this rule. If a rule has a label, all matches that it creates
    # will be accessible as named captures from the scope of their parent match
    # using that label.
    attr_reader :label

    # Specifies a module that will be used to extend all Match objects that
    # result from this rule. If +mod+ is a Proc, it is used to create an
    # anonymous module with a +value+ method.
    def extension=(mod)
      if Proc === mod
        mod = Module.new { define_method(:value, &mod) }
      end

      raise ArgumentError, "Extension must be a Module" unless Module === mod

      @extension = mod
    end

    # The module this rule uses to extend new matches.
    attr_reader :extension

    # The default set of options to use when calling #parse.
    def default_options # :nodoc:
      { :consume  => true,
        :memoize  => false,
        :offset   => 0,
        :left_recurse => false
      }
    end

    # Attempts to parse the given +string+ and return a Match if any can be
    # made. +options+ may contain any of the following keys:
    #
    # consume::       If this is +true+ a ParseError will be raised unless the
    #                 entire input string is consumed. Defaults to +true+.
    # left_recurse::  If this is +true+ the matches generated during a parse are
    #                 memoized, and Rules may make use of left-recursion. For
    #                 example: e = e + e | int
    #                 Defaults to +false+.
    # memoize::       If this is +true+ the matches generated during a parse are
    #                 memoized. See MemoizedInput for more information. Defaults to
    #                 +false+.
    # offset::        The offset in +string+ at which to start parsing. Defaults
    #                 to 0.
    def parse(string, options={})
      opts = default_options.merge(options)

      input = case
        when opts[:left_recurse]
          LeftRecursiveMemoizedInput
        when opts[:memoize]
          MemoizedInput
        else
          Input
      end.new(string)
      input.pos = opts[:offset] if opts[:offset] > 0

      events = input.exec(self)
      length = events[-1]

      puts '==='
      puts events
      puts '==='
      puts length
      puts (string.length - opts[:offset])
      
      if !length || (opts[:consume] && length < (string.length - opts[:offset]))
        raise ParseError, input
      end

      Match.new(string.slice(opts[:offset], length), events)
    end

    # Tests whether or not this rule matches on the given +string+. Returns the
    # length of the match if any can be made, +nil+ otherwise. Accepts the same
    # +options+ as #parse.
    def test(string, options={})
      parse(string, options).length
    rescue ParseError
      nil
    end

    # Tests the given +obj+ for case equality with this rule.
    def ===(obj)
      !test(obj).nil?
    end

    # Returns +true+ if this rule is a Terminal.
    def terminal?
      false
    end

    # Returns +true+ if this rule should extend a match but should not appear in
    # its event stream.
    def elide?
      false
    end

    # Returns +true+ if this rule needs to be surrounded by parentheses when
    # using #to_embedded_s.
    def needs_paren? # :nodoc:
      is_a?(Nonterminal) && rules.length > 1
    end

    # Returns the Citrus notation of this rule as a string.
    def to_s
      if label
        "#{label}:" + (needs_paren? ? "(#{to_citrus})" : to_citrus)
      else
        to_citrus
      end
    end

    # This alias allows strings to be compared to the string representation of
    # Rule objects. It is most useful in assertions in unit tests, e.g.:
    #
    #     assert_equal('"a" | "b"', rule)
    #
    alias_method :to_str, :to_s

    # Returns the Citrus notation of this rule as a string that is suitable to
    # be embedded in the string representation of another rule.
    def to_embedded_s # :nodoc:
      if name
        name.to_s
      else
        needs_paren? && label.nil? ? "(#{to_s})" : to_s
      end
    end

    def ==(other)
      case other
      when Rule
        to_s == other.to_s
      else
        super
      end
    end

    alias_method :eql?, :==

    def inspect # :nodoc:
      to_s
    end

    def extend_match(match) # :nodoc:
      match.extend(extension) if extension
    end
  end

  # A Proxy is a Rule that is a placeholder for another rule. It stores the
  # name of some other rule in the grammar internally and resolves it to the
  # actual Rule object at runtime. This lazy evaluation permits creation of
  # Proxy objects for rules that may not yet be defined.
  module Proxy
    include Rule

    def initialize(rule_name='<proxy>')
      self.rule_name = rule_name
    end

    # Sets the name of the rule this rule is proxy for.
    def rule_name=(rule_name)
      @rule_name = rule_name.to_sym
    end

    # The name of this proxy's rule.
    attr_reader :rule_name

    # Returns the underlying Rule for this proxy.
    def rule
      @rule ||= resolve!
    end

    # Returns an array of events for this rule on the given +input+.
    def exec(input)
      puts "Trying ##{self.object_id} #{self.class} - #{self.name} - #{self.inspect} at position #{input.pos}"
      
      parse_tree = input.exec(rule)

      if parse_tree.size > 0
        puts "Matched ##{self.object_id} #{self.class} - #{self.name} - #{self.inspect} ; now at position #{input.pos}"
        # Proxy objects insert themselves into the event stream in place of the rule they are proxy for.
        parse_tree[0] = self
      else
        puts "FAILED ##{self.object_id} #{self.class} - #{self.name} - #{self.inspect} at position #{input.pos}"
      end

      parse_tree
    end

    # Returns +true+ if this rule should extend a match but should not appear in
    # its event stream.
    def elide? # :nodoc:
      rule.elide?
    end

    def extend_match(match) # :nodoc:
      # Proxy objects preserve the extension of the rule they are proxy for, and
      # may also use their own extension.
      rule.extend_match(match)
      super
    end
  end

  # An Alias is a Proxy for a rule in the same grammar. It is used in rule
  # definitions when a rule calls some other rule by name. The Citrus notation
  # is simply the name of another rule without any other punctuation, e.g.:
  #
  #     name
  #
  class Alias
    include Proxy

    # Returns the Citrus notation of this rule as a string.
    def to_citrus # :nodoc:
      rule_name.to_s
    end

  private

    # Searches this proxy's grammar and any included grammars for a rule with
    # this proxy's #rule_name. Raises an error if one cannot be found.
    def resolve!
      rule = grammar.rule(rule_name)

      unless rule
        raise Error, "No rule named \"#{rule_name}\" in grammar #{grammar}"
      end

      rule
    end
  end

  # A Super is a Proxy for a rule of the same name that was defined previously
  # in the grammar's inheritance chain. Thus, Super's work like Ruby's +super+,
  # only for rules in a grammar instead of methods in a module. The Citrus
  # notation is the word +super+ without any other punctuation, e.g.:
  #
  #     super
  #
  class Super
    include Proxy

    # Returns the Citrus notation of this rule as a string.
    def to_citrus # :nodoc:
      'super'
    end

  private

    # Searches this proxy's included grammars for a rule with this proxy's
    # #rule_name. Raises an error if one cannot be found.
    def resolve!
      rule = grammar.super_rule(rule_name)

      unless rule
        raise Error,
          "No rule named \"#{rule_name}\" in hierarchy of grammar #{grammar}"
      end

      rule
    end
  end

  # A Terminal is a Rule that matches directly on the input stream and may not
  # contain any other rule. Terminals are essentially wrappers for regular
  # expressions. As such, the Citrus notation is identical to Ruby's regular
  # expression notation, e.g.:
  #
  #     /expr/
  #
  # Character classes and the dot symbol may also be used in Citrus notation for
  # compatibility with other parsing expression implementations, e.g.:
  #
  #     [a-zA-Z]
  #     .
  #
  # Character classes have the same semantics as character classes inside Ruby
  # regular expressions. The dot matches any character, including newlines.
  class Terminal
    include Rule

    def initialize(regexp=/^/)
      @regexp = regexp
    end

    # The actual Regexp object this rule uses to match.
    attr_reader :regexp

    # Returns an array of events for this rule on the given +input+.
    def exec(input)
      puts "Trying ##{self.object_id} #{self.class} - #{self.name} - #{self.inspect} at position #{input.pos}"
      match = input.scan(@regexp)

      parse_tree = []

      if match
        puts "Matched ##{self.object_id} #{self.class} - #{self.name} - #{self.inspect} ; now at position #{input.pos}"
        parse_tree << self
        parse_tree << CLOSE
        parse_tree << match.length
      else
        puts "FAILED ##{self.object_id} #{self.class} - #{self.name} - #{self.inspect} at position #{input.pos}"
      end

      parse_tree
    end

    # Returns +true+ if this rule is case sensitive.
    def case_sensitive?
      !@regexp.casefold?
    end

    def ==(other)
      case other
      when Regexp
        @regexp == other
      else
        super
      end
    end

    alias_method :eql?, :==

    # Returns +true+ if this rule is a Terminal.
    def terminal? # :nodoc:
      true
    end

    # Returns the Citrus notation of this rule as a string.
    def to_citrus # :nodoc:
      @regexp.inspect
    end
  end

  # A StringTerminal is a Terminal that may be instantiated from a String
  # object. The Citrus notation is any sequence of characters enclosed in either
  # single or double quotes, e.g.:
  #
  #     'expr'
  #     "expr"
  #
  # This notation works the same as it does in Ruby; i.e. strings in double
  # quotes may contain escape sequences while strings in single quotes may not.
  # In order to specify that a string should ignore case when matching, enclose
  # it in backticks instead of single or double quotes, e.g.:
  #
  #     `expr`
  #
  # Besides case sensitivity, case-insensitive strings have the same semantics
  # as double-quoted strings.
  class StringTerminal < Terminal
    # The +flags+ will be passed directly to Regexp#new.
    def initialize(rule='', flags=0)
      super(Regexp.new(Regexp.escape(rule), flags))
      @string = rule
    end

    def ==(other)
      case other
      when String
        @string == other
      else
        super
      end
    end

    alias_method :eql?, :==

    # Returns the Citrus notation of this rule as a string.
    def to_citrus # :nodoc:
      if case_sensitive?
        @string.inspect
      else
        @string.inspect.gsub(/^"|"$/, '`')
      end
    end
  end

  # A Nonterminal is a Rule that augments the matching behavior of one or more
  # other rules. Nonterminals may not match directly on the input, but instead
  # invoke the rule(s) they contain to determine if a match can be made from
  # the collective result.
  module Nonterminal
    include Rule

    def initialize(rules=[])
      @rules = rules.map {|r| Rule.for(r) }
    end

    # An array of the actual Rule objects this rule uses to match.
    attr_reader :rules

    def grammar=(grammar) # :nodoc:
      super
      @rules.each {|r| r.grammar = grammar }
    end
  end

  # An AndPredicate is a Nonterminal that contains a rule that must match. Upon
  # success an empty match is returned and no input is consumed. The Citrus
  # notation is any expression preceded by an ampersand, e.g.:
  #
  #     &expr
  #
  class AndPredicate
    include Nonterminal

    def initialize(rule='')
      super([rule])
    end

    # Returns the Rule object this rule uses to match.
    def rule
      rules[0]
    end

    # Returns an array of events for this rule on the given +input+.
    def exec(input)
      parse_tree = []
      
      if input.test(rule) > 0
        parse_tree << self
        parse_tree << CLOSE
        parse_tree << 0
      end

      parse_tree
    end

    # Returns the Citrus notation of this rule as a string.
    def to_citrus # :nodoc:
      '&' + rule.to_embedded_s
    end
  end

  # A NotPredicate is a Nonterminal that contains a rule that must not match.
  # Upon success an empty match is returned and no input is consumed. The Citrus
  # notation is any expression preceded by an exclamation mark, e.g.:
  #
  #     !expr
  #
  class NotPredicate
    include Nonterminal

    def initialize(rule='')
      super([rule])
    end

    # Returns the Rule object this rule uses to match.
    def rule
      rules[0]
    end

    # Returns an array of events for this rule on the given +input+.
    def exec(input)
      parse_tree = []
      
      unless input.test(rule) > 0
        parse_tree << self
        parse_tree << CLOSE
        parse_tree << 0
      end
      
      parse_tree
    end

    # Returns the Citrus notation of this rule as a string.
    def to_citrus # :nodoc:
      '!' + rule.to_embedded_s
    end
  end

  # A ButPredicate is a Nonterminal that consumes all characters until its rule
  # matches. It must match at least one character in order to succeed. The
  # Citrus notation is any expression preceded by a tilde, e.g.:
  #
  #     ~expr
  #
  class ButPredicate
    include Nonterminal

    DOT_RULE = Rule.for(DOT)

    def initialize(rule='')
      super([rule])
    end

    # Returns the Rule object this rule uses to match.
    def rule
      rules[0]
    end

    # Returns an array of events for this rule on the given +input+.
    def exec(input)
      parse_tree = []
      
      length = 0

      until input.test(rule) > 0
        len = input.exec(DOT_RULE)[-1]
        break if len.nil?
        length += len
      end

      if length > 0
        parse_tree << self
        parse_tree << CLOSE
        parse_tree << length
      end

      parse_tree
    end

    # Returns the Citrus notation of this rule as a string.
    def to_citrus # :nodoc:
      '~' + rule.to_embedded_s
    end
  end

  # A Repeat is a Nonterminal that specifies a minimum and maximum number of
  # times its rule must match. The Citrus notation is an integer, +N+, followed
  # by an asterisk, followed by another integer, +M+, all of which follow any
  # other expression, e.g.:
  #
  #     expr N*M
  #
  # In this notation +N+ specifies the minimum number of times the preceding
  # expression must match and +M+ specifies the maximum. If +N+ is ommitted,
  # it is assumed to be 0. Likewise, if +M+ is omitted, it is assumed to be
  # infinity (no maximum). Thus, an expression followed by only an asterisk may
  # match any number of times, including zero.
  #
  # The shorthand notation <tt>+</tt> and <tt>?</tt> may be used for the common
  # cases of <tt>1*</tt> and <tt>*1</tt> respectively, e.g.:
  #
  #     expr+
  #     expr?
  #
  class Repeat
    include Nonterminal

    def initialize(rule='', min=1, max=Infinity)
      raise ArgumentError, "Min cannot be greater than max" if min > max
      super([rule])
      @min = min
      @max = max
    end

    # Returns the Rule object this rule uses to match.
    def rule
      rules[0]
    end

    # Returns an array of events for this rule on the given +input+.
    def exec(input)
      events = []
      
      partial_parse_tree = []
      
      length = n = 0

      while n < max && (parse_tree = input.exec(rule)).size > 0
        length += parse_tree[-1]
        partial_parse_tree.concat(parse_tree)
        n += 1
      end

      if n >= min
        events << self
        events.concat(partial_parse_tree)
        events << CLOSE
        events << length
      end

      events
    end

    # The minimum number of times this rule must match.
    attr_reader :min

    # The maximum number of times this rule may match.
    attr_reader :max

    # Returns the operator this rule uses as a string. Will be one of
    # <tt>+</tt>, <tt>?</tt>, or <tt>N*M</tt>.
    def operator
      @operator ||= case [min, max]
        when [0, 0] then ''
        when [0, 1] then '?'
        when [1, Infinity] then '+'
        else
          [min, max].map {|n| n == 0 || n == Infinity ? '' : n.to_s }.join('*')
        end
    end

    # Returns the Citrus notation of this rule as a string.
    def to_citrus # :nodoc:
      rule.to_embedded_s + operator
    end
  end

  # A Sequence is a Nonterminal where all rules must match. The Citrus notation
  # is two or more expressions separated by a space, e.g.:
  #
  #     expr expr
  #
  class Sequence
    include Nonterminal

    # Returns an array of events for this rule on the given +input+.
    def exec(input)
      events = []
      
      puts "Trying ##{self.object_id} #{self.class} - #{self.name} - #{self.inspect} at position #{input.pos}"

      partial_parse_tree = []
      length = n = 0
      m = rules.length

      while n < m && (parse_tree = input.exec(rules[n])).size > 0
        length += parse_tree[-1]
        partial_parse_tree.concat(parse_tree)
        n += 1
      end

      if n == m
        events << self
        puts "Matched ##{self.object_id} #{self.class} - #{self.name} - #{self.inspect} ; now at position #{input.pos}"
        events.concat(partial_parse_tree)
        events << CLOSE
        events << length
      else
        puts "FAILED ##{self.object_id} #{self.class} - #{self.name} - #{self.inspect} at position #{input.pos}"
        puts "n = #{n}"
        puts "m = #{m}"
      end

      events
    end

    # Returns the Citrus notation of this rule as a string.
    def to_citrus # :nodoc:
      rules.map {|r| r.to_embedded_s }.join(' ')
    end
  end

  # A Choice is a Nonterminal where only one rule must match. The Citrus
  # notation is two or more expressions separated by a vertical bar, e.g.:
  #
  #     expr | expr
  #
  class Choice
    include Nonterminal

    # Returns an array of events for this rule on the given +input+.
    def exec(input)
      events=[]
      puts "Trying ##{self.object_id} #{self.class} - #{self.name} - #{self.inspect} at position #{input.pos}"

      n = 0
      m = rules.length

      parse_tree = []

      while n < m && (parse_tree = input.exec(rules[n])).size == 0
        n += 1
      end

      if parse_tree.size > 0
        puts "Matched ##{self.object_id} #{self.class} - #{self.name} - #{self.inspect} ; now at position #{input.pos}"
        events << self
        events.concat(parse_tree)
        events << CLOSE
        events << parse_tree[-1]
      else
        puts "FAILED ##{self.object_id} #{self.class} - #{self.name} - #{self.inspect} at position #{input.pos}"
      end

      events
    end

    # Returns +true+ if this rule should extend a match but should not appear in
    # its event stream.
    def elide? # :nodoc:
      true
    end

    # Returns the Citrus notation of this rule as a string.
    def to_citrus # :nodoc:
      rules.map {|r| r.to_embedded_s }.join(' | ')
    end
  end

  # The base class for all matches. Matches are organized into a tree where any
  # match may contain any number of other matches. Nodes of the tree are lazily
  # instantiated as needed. This class provides several convenient tree
  # traversal methods that help when examining and interpreting parse results.
  class Match
    def initialize(string, events=[])
      @string = string

      if events.length > 0
        if events[-1] != string.length
          raise ArgumentError, "Invalid events for length #{string.length}"
        end

        elisions = []

        while events[0].elide?
          elisions.unshift(events.shift)
          events.slice!(-2, events.length)
        end

        events[0].extend_match(self)

        elisions.each do |rule|
          rule.extend_match(self)
        end
      else
        # Create a default stream of events for the given string.
        events = [Rule.for(string), CLOSE, string.length]
      end

      @events = events
    end

    # The array of events for this match.
    attr_reader :events

    # Returns the length of this match.
    def length
      @string.length
    end

    # Returns a hash of capture names to arrays of matches with that name,
    # in the order they appeared in the input.
    def captures
      process_events! unless @captures
      @captures
    end

    # Returns an array of all immediate submatches of this match.
    def matches
      process_events! unless @matches
      @matches
    end

    # A shortcut for retrieving the first immediate submatch of this match.
    def first
      matches.first
    end

    # Allows methods of this match's string to be called directly and provides
    # a convenient interface for retrieving the first match with a given name.
    def method_missing(sym, *args, &block)
      if @string.respond_to?(sym)
        @string.__send__(sym, *args, &block)
      else
        captures[sym].first
      end
    end

    def to_s
      @string
    end

    # This alias allows strings to be compared to the string value of Match
    # objects. It is most useful in assertions in unit tests, e.g.:
    #
    #     assert_equal("a string", match)
    #
    alias_method :to_str, :to_s

    # The default value for a match is its string value. This method is
    # overridden in most cases to be more meaningful according to the desired
    # interpretation.
    alias_method :value, :to_s

    # Returns this match plus all sub #matches in an array.
    def to_a
      [self] + matches
    end

    # Returns the capture at the given +key+. If it is an Integer (and an
    # optional length) or a Range, the result of #to_a with the same arguments
    # is returned. Otherwise, the value at +key+ in #captures is returned.
    def [](key, *args)
      case key
      when Integer, Range
        to_a[key, *args]
      else
        captures[key]
      end
    end

    def ==(other)
      case other
      when String
        @string == other
      when Match
        @string == other.to_s
      else
        super
      end
    end

    alias_method :eql?, :==

    def inspect
      @string.inspect
    end

    # Prints the entire subtree of this match using the given +indent+ to
    # indicate nested match levels. Useful for debugging.
    def dump(indent=' ')
      lines = []
      stack = []
      offset = 0
      close = false
      index = 0
      last_length = nil

      while index < @events.size
        event = @events[index]

        if close
          os = stack.pop
          start = stack.pop
          rule = stack.pop

          space = indent * (stack.size / 3)
          string = @string.slice(os, event)
          lines[start] = "#{space}#{string.inspect} rule=#{rule}, offset=#{os}, length=#{event}"

          last_length = event unless last_length

          close = false
        elsif event == CLOSE
          close = true
        else
          if last_length
            offset += last_length
            last_length = nil
          end

          stack << event
          stack << index
          stack << offset
        end

        index += 1
      end

      puts lines.compact.join("\n")
    end

  private

    # Initializes both the @captures and @matches instance variables.
    def process_events!
      @captures = captures_hash
      @matches = []

      capture!(@events[0], self)
      @captures[0] = self

      stack = []
      offset = 0
      close = false
      index = 0
      last_length = nil
      capture = true

      while index < @events.size
        event = @events[index]

        if close
          start = stack.pop

          if Rule === start
            rule = start
            os = stack.pop
            start = stack.pop

            match = Match.new(@string.slice(os, event), @events[start..index])
            capture!(rule, match)

            if stack.size == 1
              @matches << match
              @captures[@matches.size] = match
            end

            capture = true
          end

          last_length = event unless last_length

          close = false
        elsif event == CLOSE
          close = true
        else
          stack << index

          # We can calculate the offset of this rule event by adding back the
          # last match length.
          if last_length
            offset += last_length
            last_length = nil
          end

          if capture && stack.size != 1
            stack << offset
            stack << event

            # We should not create captures when traversing a portion of the
            # event stream that is masked by a proxy in the original rule
            # definition.
            capture = false if Proxy === event
          end
        end

        index += 1
      end
    end

    def capture!(rule, match)
      # We can lookup matches that were created by proxy by the name of
      # the rule they are proxy for.
      if Proxy === rule
        if @captures.key?(rule.rule_name)
          @captures[rule.rule_name] << match
        else
          @captures[rule.rule_name] = [match]
        end
      end

      # We can lookup matches that were created by rules with labels by
      # that label.
      if rule.label
        if @captures.key?(rule.label)
          @captures[rule.label] << match
        else
          @captures[rule.label] = [match]
        end
      end
    end

    # Returns a new Hash that is to be used for @captures. This hash normalizes
    # String keys to Symbols, returns +nil+ for unknown Numeric keys, and an
    # empty Array for all other unknown keys.
    def captures_hash
      Hash.new do |hash, key|
        case key
        when String
          hash[key.to_sym]
        when Numeric
          nil
        else
          []
        end
      end
    end
  end
end

class Object
  # A sugar method for creating Citrus grammars from any namespace.
  #
  #     grammar :Calc do
  #     end
  #
  #     module MyModule
  #       grammar :Calc do
  #       end
  #     end
  #
  def grammar(name, &block)
    namespace = respond_to?(:const_set) ? self : Object
    namespace.const_set(name, Citrus::Grammar.new(&block))
  rescue NameError
    raise ArgumentError, "Invalid grammar name: #{name}"
  end
end
