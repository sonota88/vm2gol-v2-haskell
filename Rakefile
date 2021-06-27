require "yaml"

libs = [
  "lib/Utils.hs",
  "lib/Types.hs",
  "lib/Json.hs",
]

dep_map = {
  "bin/json_tester" => ["test/json_tester.hs", *libs],
  # "bin/utils_tester" => ["test/utils_tester.hs" , *libs],
  "bin/lexer"   => ["lexer.hs", *libs],
  "bin/parser"  => ["parser.hs", *libs],
  "bin/codegen" => ["codegen.hs", *libs]
}

dep_map.each do |goal, src_files|
  file goal => src_files do |t|
    exe_file = t.name

    src_files.each { |src_file|
      sh %(cp -p #{src_file} z_tmp/)
    }

    tmp_src_file = "z_tmp/" + File.basename(src_files[0])

    cmd = [
      %(./docker_run.sh stack ghc -- -Wall -iz_tmp -o "#{exe_file}" "#{tmp_src_file}")
    ].join(" ")

    sh cmd
  end
end

task :default => :"build-all"
task :"build-all" => [
  "bin/json_tester",
  "bin/lexer",
  "bin/parser",
  "bin/codegen"
]
