require 'open3'

EXE = "./computor"

def define_control(s)
  STDOUT.isatty ? s : ""
end

TEXTCOLOR_OK = define_control "\e[32m"
TEXTCOLOR_NG = define_control "\e[31m"
TEXTCOLOR_RESET = define_control "\e[0m"
TEXTCOLOR_WEAK = define_control "\e[2m"

module Utils
  def detect_parse_error(str)
    m = str.match(/^ParseError: (.*)$/)
    m ? m[1] : nil
  end

  def detect_lex_error(str)
    m = str.match(/^TokenizeError: (.*)$/)
    m ? m[1] : nil
  end

  def detect_reduction_error(str)
    m = str.match(/^ReductionError: (.*)$/)
    m ? m[1] : nil
  end

  def detect_solver_error(str)
    m = str.match(/^SolverError: (.*)$/)
    m ? m[1] : nil
  end

  def detect_dimension(str, dim)
    m = str.match(/^\[Dimension\s*\] (\d+)$/)
    m ? m[1].to_i == dim : false
  end

  def detect_solutions(str)
    m = str.match(/^\[Solutions\s*\] (.*)$/)
    m ? m[1] : nil
  end

  module_function :detect_parse_error, :detect_lex_error, :detect_reduction_error, :detect_solver_error, :detect_dimension, :detect_solutions
end

module Predicate
  def has_usage(str)
    raise "No Usage" unless str.include?("Usage:")
  end

  def no_usage(str)
    raise "Unexpected Usage" if str.include?("Usage:")
  end

  def has_lex_error(str)
    r = Utils.detect_lex_error(str)
    raise "No Lex Error" unless r
    r
  end

  def no_lex_error(str)
    r = Utils.detect_lex_error(str)
    raise "Unexpected Lex Error: #{r}" if r
  end

  def has_parse_error(str)
    r = Utils.detect_parse_error(str)
    raise "No Parse Error" unless r
    r
  end

  def no_parse_error(str)
    r = Utils.detect_parse_error(str)
    raise "Unexpected Parse Error: #{r}" if r
  end

  def has_reduction_error(str)
    r = Utils.detect_reduction_error(str)
    raise "No Reduction Error" unless r
    r
  end

  def no_reduction_error(str)
    r = Utils.detect_reduction_error(str)
    raise "Unexpected Reduction Error: #{r}" if r
  end

  def has_solver_error(str)
    r = Utils.detect_solver_error(str)
    raise "No Solver Error" unless r
    r
  end

  def no_solver_error(str)
    r = Utils.detect_solver_error(str)
    raise "Unexpected Solver Error: #{r}" if r
  end

  def has_dimension(str, dim)
    r = Utils.detect_dimension(str, dim)
    raise "No Dimension" unless r
  end

  def has_arbitrary_solutions(str)
    r = Utils.detect_solutions(str)
    raise "No Solutions" unless r == "ARBITRARY COMPLEX NUMBER"
    r
  end

  def has_none_solutions(str)
    r = Utils.detect_solutions(str)
    raise "No Solutions" unless r == "NONE"
    r
  end

  module_function :has_usage, :no_usage, :has_lex_error, :no_lex_error, :has_parse_error, :no_parse_error, :has_reduction_error, :no_reduction_error, :has_solver_error, :no_solver_error, :has_dimension, :has_arbitrary_solutions, :has_none_solutions
end


def try(arguments, *expectations)
  argspart = arguments.map{ |a| "'#{a}'"} * " "
  command = "#{EXE} #{argspart}"
  t0 = Time.now.to_f
  out, err, status = Open3.capture3(EXE, *arguments)
  t1 = Time.now.to_f
  begin
    expectations.each{ |exp|
      case exp
      when :has_usage
          Predicate.has_usage(out)
      when :no_usage
          Predicate.no_usage(out)
      when :has_lex_error
          Predicate.has_lex_error(out)
      when :no_lex_error
          Predicate.no_lex_error(out)
      when :has_parse_error
          Predicate.has_parse_error(out)
      when :no_parse_error
          Predicate.no_parse_error(out)
      when :has_reduction_error
          Predicate.has_reduction_error(out)
      when :no_reduction_error
          Predicate.no_reduction_error(out)
      when :has_solver_error
          Predicate.has_solver_error(out)
      when :no_solver_error
          Predicate.no_solver_error(out)
      when :has_dimension_0
          Predicate.has_dimension(out, 0)
      when :has_dimension_1
          Predicate.has_dimension(out, 1)
      when :has_dimension_2
          Predicate.has_dimension(out, 2)
      when :has_dimension_3
          Predicate.has_dimension(out, 3)
      when :has_arbitrary_solution
          Predicate.has_arbitrary_solutions(out)
      when :has_none_solution
          Predicate.has_none_solutions(out)

      else
          raise "Unknown expectation: #{exp}"
      end
    }
    puts "#{TEXTCOLOR_OK}[ok]#{TEXTCOLOR_RESET} #{argspart} -> exit #{status.exitstatus} " + sprintf("(%dms)", (t1 - t0)*1000)
    0
  rescue => e
    puts "#{TEXTCOLOR_NG}[NG]: #{e.message}#{TEXTCOLOR_RESET}"
    puts "#{TEXTCOLOR_WEAK}Command:#{TEXTCOLOR_RESET}"
    puts command
    puts "#{TEXTCOLOR_WEAK}Output:#{TEXTCOLOR_RESET}"
    puts out
    puts "#{TEXTCOLOR_WEAK}Exit Status:#{TEXTCOLOR_RESET} #{status.exitstatus}"
    puts
    1
  end
end

CasesUsage = [
  [[], :has_usage],
  [["1 = ", "X"], :has_usage],
  [["X = 2", "X^2 = 2"], :has_usage],
]

CaseLexError = [
  "1. = X",
  ".23 = X",
  "1.23.4 = X",
  "X^2 % 10 = 0",
  "3 * @ = 1",
  "X^.2 = 1",
].map{ |a| [[a], :has_lex_error] }

CasesParseError = [
  "",
  "1 = ",
  "1 + 1 =",
  "1 = 1 = 1",
  "= 1",
  "X - 1",
  "X = 0)",
  "X (= 0)",
  "X (= 0",
  "(X = 0",
  "(X = 0)",
  "X = 0)",
  "X^-1 = 1",
  "X^+1 = 1",
  "X^1.2 = 1",
  "1X = 1",
  "1X2 = 1",
  "X2 = 1",
  "--1 = X", # NOTE: 式の先頭の二重+-は許容されない(二重の単項+-と解釈するしかないため)
  "+-1 = X",
  "-+1 = X",
  "1 = ++X",
].map{ |a| [[a], :has_parse_error] }

CaseReduceError = [
  "1 / 0 = X",
  "1 / (X^2 + 1) = X",
  "X = 0^0",
  "X = (X-X)^0",
].map{ |a| [[a], :has_reduction_error] }

CaseUnsolvable = [
  "X + Y = 0",
  "X^4 + 1 = 0",
  "(X+1)^2*(X-1)^2 = 0",
  "X^3 + Y^4 + Z^2 = X^3 + Z^2",
  "X / X / X = 1",
  "(X^2+1)^2 = 0",
].map{ |a| [[a], :has_solver_error] }

CaseSolvable0 = [
  ["0 = 0", :has_arbitrary_solution],
  ["1 = 1", :has_arbitrary_solution],
  ["X = X", :has_arbitrary_solution],
  ["X + Y - Y = X + 1", :has_none_solution],
  ["(X+1)^4 = X^4 + 4 * X^3 + 6*X^2 + 4*X + 1", :has_arbitrary_solution], # -> 0 = 0
  ["(X+1)^4 = X^4 + 4 * X^3 + 6*X^2 + 4*X", :has_none_solution], # -> 0 = 1
].map{ |a, ex| [[a], *ex, :has_dimension_0, :no_solver_error] }

CaseSolvable1 = [
  "X = 0",
  "X = 1",
  "X = 1 + 1",
  "X = 1 - 1",
  "X = 1 * 1",
  "X = 1 / 1",
  "X = 1 / 1 / 1",
  "X = 1 ++ 2", # NOTE: 式の途中の二重+-は許容される(二項→単項と解釈して矛盾がないため)
  "X = 1 -- 2",
  "X^100 / X^99 = 1",
  "X^10000000 / X^9999999 = 1",
  "X^0/X^2*X^3 = 2 * X",
  "X * Y / Y^2 * X * Y / X = 1",
  "(X+1)^4 = X^4 + 4 * X^3 + 6*X^2", # -> 0 = X + 1
].map{ |a| [[a], :has_dimension_1, :no_solver_error] }

CaseSolvable2 = [
  "X^2 = 0",
  "X^2 = 1",
  "X^2 = 1 + 1",
  "X^2 = 1 - 1",
  "X^2 = 1 * 1",
  "X^2 = 1 / 1",
  "(X + 1) * (X - 1) = 0",
  "((X + 1) ^ 3 - 3 * X - 3 * X^2 - 1) / X = 0",
  "0*X^3 + X^2 = 1",
].map{ |a| [[a], :has_dimension_2, :no_solver_error] }

CaseSolvable3 = [
  "X^3 = 0",
  "X^3 = 1",
  "X^3 = 1 + 1",
  "X^3 = 1 - 1",
  "X^3 = 1 * 1",
  "X^3 = 1 / 1",
  "(X + 1) * (X - 1) * (X + 2) = 0",
  "((X + 1) ^ 3 - 3 * X - 3 * X^2 - 1) = 0",
  "(1 + 3)^100 = X^3",
].map{ |a| [[a], :has_dimension_3, :no_solver_error] }

CaseExtreme = [
  [["(X+1)^100 = (X-1)^100"], :has_solver_error],
  [["(X+1)^100 = (X+1)^100 + X"], :has_dimension_1, :no_solver_error],
  [["あんぱん^3 = (1 + 2)^3 / 100"], :has_dimension_3, :no_solver_error],
  [["(((((((((((X  )^2)^2)^2)^2)^2)^2)^2)^2)^2)^2)^2 = 0"], :has_solver_error],
  [["(((((((((((X+1)^2)^2)^2)^2)^2)^2)^2)^2)^2)^2)^2 = 0"], :has_solver_error],
  [["(X+1)^9 = 0"], :has_solver_error],
  [["(X+1)^99 = 0"], :has_solver_error],
  [["(X+1)^999 = 0"], :has_solver_error],
  # [["(X+1)^9999 = 0"], :has_solver_error], -- 1分弱かかる
]

def evaluate_cases(name, cases)
  fails = 0
  cases.each{ |args, *expectations|
    fails += try(args, *expectations)
  }

  puts "--"
  if fails > 0
    puts "#{TEXTCOLOR_NG}[NG] #{name}: #{fails} / #{cases.size} test(s) failed#{TEXTCOLOR_RESET}"
    false
  else
    puts "#{TEXTCOLOR_OK}[ok] #{name}#{TEXTCOLOR_RESET}"
    true
  end
end


passed = true
[
  ["Usage", CasesUsage],
  ["LexError", CaseLexError],
  ["ParseError", CasesParseError],
  ["ReduceError", CaseReduceError],
  ["Unsolvable", CaseUnsolvable],
  ["Solvable(Constant)", CaseSolvable0],
  ["Solvable(Linear)", CaseSolvable1],
  ["Solvable(Quadratic)", CaseSolvable2],
  ["Solvable(Cubic)", CaseSolvable3],
  ["Extreme", CaseExtreme],
].each{ |name, cases|
  puts "Cases: #{name} (#{cases.size})"
  passed = evaluate_cases(name, cases) && passed
  puts
}

if passed
  puts "#{TEXTCOLOR_OK}All tests passed#{TEXTCOLOR_RESET}"
else
  puts "#{TEXTCOLOR_NG}Some tests failed#{TEXTCOLOR_RESET}"
end

exit passed ? 0 : 1
