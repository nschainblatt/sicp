# Generic Operations

## Explicit Dispatch
Data objects are typed and given to generic procedures that extract the type and apply the correct procedure.

## Data Directed Style
Procedures are installed into a 'operation-type-table' within a specific types package. Then to use these operations you use the tables 'GET' selector along with a type and operation name to retrieve and then apply the procedure.

## Message Passing
Data object constructors are type specific (one per type) and return a dispatch procedure that accepts a 'message' that then will perform that operation on the enclosed data.


# Adding New Types and Operations

## Explicit Dispatch
To add a new type, all existing generic procedures must be updated to add this new type to the internal type conditionals.
To add a new operation, a new generic procedure must be made and the procedure must account for all existing types and the new type. All other generic procedures can stay the same.

## Data Directed Style
To add a new type, a new installation package must be made, creating constructors and selectors for existing operations from other package types. Other packages, types and procedures are unaffected.
To add a new operation, each package will have to be updated to support the new operation for every type.

## Message Passing
To add a new type, a new data object and internal dispatch procedure must be created to support all existing operations.
To add a new operation, all existing data objects internal dispatch procedures must be updated to handle the new operation.


# Best For Adding Types
When types are added often, data driven type and message passing are suggested because it would require little or no modification to existing code.


# Best For Adding Operations
When operations are added often, explicit dispatch is suggested as they also require the least modification to existing code.
