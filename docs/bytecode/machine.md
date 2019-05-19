# ImpLang VM

The ImpLang VM is a fairly basic bytecode VM.

## Structure

Program Memory is an Immutable Vector of ByteCode instructions
Call Stack is a Stack of Integers
Op Stack is a Stack of StackItems
RAM is a Mutable Vector of StackItems
Program Counter is an Integer

## Types

StackItem is a box for data that gets handled, which is either Floats or Strings.

Op is a type that defines operations that can be performed on the OpStack

## Instructions

Push StackItem      - Push the value at the address onto the OpStack
Pop                 - Pop the top value of the OpStack
Operator Op         - Perform an operation
Jump Int            - Set the Program Counter to a given address
RelJump Int         - Change the Program Counter by the given value
Branch Int          - If the top OpStack value is not 0 then Jump to the address
Constant StackItem  - Load the value onto the OpStack
Load Int            - Push the value at the given RAM address onto the OpStack
Save Int            - Save the top value of the OpStack to the given address
Call Int Int        - Call a function using the top X values from the OpStack
BuiltIn Int Int     - Call a native function using the top X values from the OpStack
Return              - Return to the address ontop of the Call Stack
End                 - Stop the execution of the program
