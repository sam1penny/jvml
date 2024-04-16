open Linearise.Instruction

let lower_ir program =
  String.concat "\n"
    [
      Lower.external_lib;
      String.concat "\n" (List.map Lower.lower_declaration program.declarations);
      Lower.produce_instruction_bytecode (program.code, program.static_methods);
    ]
