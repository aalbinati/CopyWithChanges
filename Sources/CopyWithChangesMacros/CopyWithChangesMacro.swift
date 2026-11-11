import SwiftCompilerPlugin
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros
import SwiftDiagnostics

/// Implementation of the `CopyWithChanges` macro.
public struct CopyWithChangesMacro: MemberMacro {
    #if canImport(SwiftSyntax600)
    public static func expansion(
        of attribute: AttributeSyntax,
        providingMembersOf declaration: some DeclGroupSyntax,
        conformingTo protocols: [TypeSyntax],
        in context: some MacroExpansionContext
    ) throws -> [DeclSyntax] {
        try members(of: attribute, providingMembersOf: declaration, in: context)
    }
    #else
    public static func expansion(
        of attribute: AttributeSyntax,
        providingMembersOf declaration: some DeclGroupSyntax,
        in context: some MacroExpansionContext
    ) throws -> [DeclSyntax] {
        try members(of: attribute, providingMembersOf: declaration, in: context)
    }
    #endif

    private static func members(
        of attribute: AttributeSyntax,
        providingMembersOf declaration: some DeclGroupSyntax,
        in context: some MacroExpansionContext
    ) throws -> [DeclSyntax] {
        guard let members = (declaration.as(StructDeclSyntax.self)?.memberBlock.members ?? declaration.as(ClassDeclSyntax.self)?.memberBlock.members) else {
            context.diagnose(Diagnostic(
                node: attribute,
                message: CopyWithChangesDiagnostic.unsupportedTarget
            ))
            return []
        }

        let variableDecls = members
            .compactMap { $0.decl.as(VariableDeclSyntax.self) }
            .filter { decl in
                !decl.modifiers.contains(where: { $0.name.text == "static" })
            }
        
        let bindings = variableDecls
            .flatMap { $0.bindings }
            .sorted {
                $0.positionAfterSkippingLeadingTrivia < $1.positionAfterSkippingLeadingTrivia
            }

        let with: DeclSyntax = {
            var arguments: [String] = []
            var prelets: [String] = []
            var assignments: [String] = []
        
            for binding in bindings {
                guard
                    let pattern = binding.pattern.as(IdentifierPatternSyntax.self)?.identifier.text,
                    let type = binding.typeAnnotation?.type,
                    binding.accessorBlock == nil
                else {
                    continue
                }
        
                let typeString = type.trimmedDescription
                let isOptional = type.is(OptionalTypeSyntax.self)

                arguments.append("\(pattern): \(typeString)? = nil")
        
                if isOptional {
                    prelets.append("let _\(pattern): \(typeString) = \(pattern) ?? self.\(pattern)")
                    assignments.append("\(pattern): _\(pattern)")
                } else {
                    assignments.append("\(pattern): \(pattern) ?? self.\(pattern)")
                }
            }
        
            // Built as one explicitly indented source string so the expansion reads the same
            // under every supported swift-syntax version.
            var body = prelets.map { "    \($0)" }
            body.append("    return Self(")
            body.append(assignments.map { "        \($0)" }.joined(separator: ",\n"))
            body.append("    )")
            return DeclSyntax(stringLiteral: """
                public func with(\(arguments.joined(separator: ", "))) -> Self {
                \(body.joined(separator: "\n"))
                }
                """)
        }()

        return [with]
    }

    enum CopyWithChangesDiagnostic: DiagnosticMessage {
        case unsupportedTarget
        case unsupportedBinding(PatternBindingSyntax)

        var severity: DiagnosticSeverity {
            switch self {
            case .unsupportedTarget:  .error
            case .unsupportedBinding: .warning
            }
        }

        var message: String {
            switch self {
            case .unsupportedTarget:
                "'@CopyWithChanges' can only be applied to a struct or a class"
            case .unsupportedBinding(let binding):
                "'@CopyWithChanges' cannot copy field '\(binding)'"
            }
        }

        var diagnosticID: MessageID {
            MessageID(domain: "CopyWithChangesMacros", id: String(describing: self))
        }
    }
}

@main
struct CopyWithChangesPlugin: CompilerPlugin {
    let providingMacros: [Macro.Type] = [
        CopyWithChangesMacro.self,
    ]
}
