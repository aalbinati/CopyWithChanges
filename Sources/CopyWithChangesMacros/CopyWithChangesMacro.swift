import SwiftCompilerPlugin
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros
import SwiftDiagnostics

/// Implementation of the `CopyWithChanges` macro.
public struct CopyWithChangesMacro: MemberMacro {
    public static func expansion(
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

        let with = try {
            var arguments: [String] = []
            var prelets: [String] = []
            var assignments: [String] = []
        
            for binding in bindings {
                guard
                    let pattern = binding.pattern.as(IdentifierPatternSyntax.self)?.identifier.text,
                    let type = binding.typeAnnotation?.type,
                    binding.accessor == nil
                else {
                    continue
                }
        
                let typeString = type.description.trimmingCharacters(in: .whitespacesAndNewlines)      
                let isOptional = type.is(OptionalTypeSyntax.self)
        
                if isOptional {
                    arguments.append("\(pattern): \(typeString) = nil")
                    prelets.append("let _\(pattern): \(typeString) = \(pattern) ?? self.\(pattern)")
                    assignments.append("\(pattern): _\(pattern)")
                } else {
                    // Non-optional stored property (T)
                    arguments.append("\(pattern): \(typeString)? = nil")
                    assignments.append("\(pattern): \(pattern) ?? self.\(pattern)")
                }
            }
        
            return try FunctionDeclSyntax(
                "public func with(\(raw: arguments.joined(separator: ", "))) -> Self"
            ) {
                """
                \(raw: prelets.joined(separator: "\n"))
                return Self(
                \(raw: assignments.joined(separator: ",\n"))
                )
                """
            }
        }()

        return [
            DeclSyntax(with),
        ]
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
