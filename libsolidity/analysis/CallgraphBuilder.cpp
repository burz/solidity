/*
	This file is part of solidity.

	solidity is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	solidity is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with solidity.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <libsolidity/analysis/CallgraphBuilder.h>

#include <libsolidity/ast/ASTVisitor.h>

using namespace std;
using namespace dev;
using namespace dev::solidity;

class CallgraphBuilderVisitor: public ASTConstVisitor
{
public:
	CallgraphBuilderVisitor(map<ASTNode const*, CallgraphNode>& _nodes):
		m_nodes(_nodes)
	{}
	virtual bool visit(SourceUnit const& _node) override { return true; }
	virtual bool visit(PragmaDirective const& _node) override { return true; }
	virtual bool visit(ImportDirective const& _node) override { return true; }
	virtual bool visit(ContractDefinition const& _node) override { return true; }
	virtual bool visit(InheritanceSpecifier const& _node) override { return true; }
	virtual bool visit(StructDefinition const& _node) override { return true; }
	virtual bool visit(UsingForDirective const& _node) override { return true; }
	virtual bool visit(EnumDefinition const& _node) override { return true; }
	virtual bool visit(EnumValue const& _node) override { return true; }
	virtual bool visit(ParameterList const& _node) override { return true; }
	virtual bool visit(FunctionDefinition const& _node) override { return true; }
	virtual bool visit(VariableDeclaration const& _node) override { return true; }
	virtual bool visit(ModifierDefinition const& _node) override { return true; }
	virtual bool visit(ModifierInvocation const& _node) override { return true; }
	virtual bool visit(EventDefinition const& _node) override { return true; }
	virtual bool visit(TypeName const& _node) override { return true; }
	virtual bool visit(ElementaryTypeName const& _node) override { return true; }
	virtual bool visit(UserDefinedTypeName const& _node) override { return true; }
	virtual bool visit(FunctionTypeName const& _node) override { return true; }
	virtual bool visit(Mapping const& _node) override { return true; }
	virtual bool visit(ArrayTypeName const& _node) override { return true; }
	virtual bool visit(Block const& _node) override { return true; }
	virtual bool visit(PlaceholderStatement const& _node) override { return true; }
	virtual bool visit(IfStatement const& _node) override { return true; }
	virtual bool visit(WhileStatement const& _node) override { return true; }
	virtual bool visit(ForStatement const& _node) override { return true; }
	virtual bool visit(Continue const& _node) override { return true; }
	virtual bool visit(InlineAssembly const& _node) override { return true; }
	virtual bool visit(Break const& _node) override { return true; }
	virtual bool visit(Return const& _node) override { return true; }
	virtual bool visit(Throw const& _node) override { return true; }
	virtual bool visit(VariableDeclarationStatement const& _node) override { return true; }
	virtual bool visit(ExpressionStatement const& _node) override { return true; }
	virtual bool visit(Conditional const& _node) override { return true; }
	virtual bool visit(Assignment const& _node) override { return true; }
	virtual bool visit(TupleExpression const& _node) override { return true; }
	virtual bool visit(UnaryOperation const& _node) override { return true; }
	virtual bool visit(BinaryOperation const& _node) override { return true; }
	virtual bool visit(FunctionCall const& _node) override { return true; }
	virtual bool visit(NewExpression const& _node) override { return true; }
	virtual bool visit(MemberAccess const& _node) override { return true; }
	virtual bool visit(Identifier const& _node) override { return true; }
	virtual bool visit(ElementaryTypeNameExpression const& _node) override { return true; }

	virtual void endVisit(SourceUnit const& _node) override {}
	virtual void endVisit(PragmaDirective const& _node) override {}
	virtual void endVisit(ImportDirective const& _node) override {}
	virtual void endVisit(ContractDefinition const& _node) override {}
	virtual void endVisit(InheritanceSpecifier const& _node) override {}
	virtual void endVisit(UsingForDirective const& _node) override {}
	virtual void endVisit(StructDefinition const& _node) override {}
	virtual void endVisit(EnumDefinition const& _node) override {}
	virtual void endVisit(EnumValue const& _node) override {}
	virtual void endVisit(ParameterList const& _node) override {}
	virtual void endVisit(FunctionDefinition const& _node) override {}
	virtual void endVisit(VariableDeclaration const& _node) override {}
	virtual void endVisit(ModifierDefinition const& _node) override {}
	virtual void endVisit(ModifierInvocation const& _node) override {}
	virtual void endVisit(EventDefinition const& _node) override {}
	virtual void endVisit(TypeName const& _node) override {}
	virtual void endVisit(ElementaryTypeName const& _node) override {}
	virtual void endVisit(UserDefinedTypeName const& _node) override {}
	virtual void endVisit(FunctionTypeName const& _node) override {}
	virtual void endVisit(Mapping const& _node) override {}
	virtual void endVisit(ArrayTypeName const& _node) override {}
	virtual void endVisit(Block const& _node) override {}
	virtual void endVisit(PlaceholderStatement const& _node) override {}
	virtual void endVisit(IfStatement const& _node) override {}
	virtual void endVisit(WhileStatement const& _node) override {}
	virtual void endVisit(ForStatement const& _node) override {}
	virtual void endVisit(Continue const& _node) override {}
	virtual void endVisit(InlineAssembly const& _node) override {}
	virtual void endVisit(Break const& _node) override {}
	virtual void endVisit(Return const& _node) override {}
	virtual void endVisit(Throw const& _node) override
	{
		m_nodes[&_node].hasSideEffects = true;
	}
	virtual void endVisit(VariableDeclarationStatement const& _variableDeclarationStatement) override
	{
		m_nodes[&_variableDeclarationStatement];
		if (Expression const* expression = _variableDeclarationStatement.initialValue())
		{
			CallgraphNode& node = m_nodes[&_variableDeclarationStatement] = m_nodes[expression];
			TypePointers valueTypes;
			if (auto tupleType = dynamic_cast<TupleType const*>(expression->annotation().type.get()))
				valueTypes = tupleType->components();
			else
				valueTypes = TypePointers{expression->annotation().type};
			auto const& assignments = _variableDeclarationStatement.annotation().assignments;
			solAssert(assignments.size() == valueTypes.size(), "");
			for (size_t i = 0; i < assignments.size(); ++i)
			{
				size_t j = assignments.size() - i - 1;
				solAssert(!!valueTypes[j], "");
				VariableDeclaration const* varDecl = assignments[j];
				if (varDecl)
					node = node + processAssignment(
						varDecl,
						*varDecl->annotation().type,
						*valueTypes[j]
					);
			}
		}
	}
	virtual void endVisit(ExpressionStatement const& _node) override
	{
		m_nodes[&_node] = m_nodes[_node.expression()];
	}
	virtual void endVisit(Conditional const& _node) override
	{
		// TODO control flow
		m_nodes[&_node] =
			m_nodes[&_node.condition()] +
			m_nodes[&_node.trueExpression()] +
			m_nodes[&_node.falseExpression()];
	}
	virtual void endVisit(Assignment const& _assignment) override
	{
		// TODO this could be more intelligent with tuples.
		// TODO finding the variable that is assigned to could be made more intelligent
		// (or whether it writes to storage or not)
		m_nodes[&_assignment] =
			m_nodes[&_assignment.leftHandSide()] +
			m_nodes[&_assignment.rightHandSide()] +
			processAssignment(
				referencedVariable(_assignment.leftHandSide()),
				*_assignment.leftHandSide().annotation().type,
				*_assignment.rightHandSide().annotation().type
			);
	}
	CallgraphNode processAssignment(VariableDeclaration const* leftHandSide, Type const& _leftType, Type const& _rightType)
	{
		CallgraphNode node;

		if (_rightType.dataStoredIn(DataLocation::Storage))
			node.readsStorage = true;

		node.hasSideEffects = true;
		node.writes.insert(var);

		if (!_leftType.isValueType() && !_leftType.dataStoredIn(DataLocation::Storage))
		{
			// We know it does not write to storage.
		}
		else if (!var || var->isStateVariable())
			node.writesStorage = true;
		return node;
	}
	virtual void endVisit(TupleExpression const& _tuple) override
	{
		auto& node = m_nodes[&_tuple];
		for (auto const& component: _tuple.components())
			if (component)
			{
				node = node + m_nodes[&component];
				if (component->annotation().type->dataStoredIn(DataLocation::Storage))
					node.readsStorage = true;
			}
	}
	virtual void endVisit(UnaryOperation const& _unaryOperation) override
	{
		m_nodes[&_unaryOperation] = m_nodes[&_unaryOperation.subExpression()];
		if (_unaryOperation.annotation().type->category() == Type::Category::RationalNumber)
			return;

		switch (_unaryOperation.getOperator())
		{
		case Token::Not:
		case Token::BitNot:
		case Token::Add:
		case Token::Sub:
			return;
		case Token::Delete:
		case Token::Inc:
		case Token::Dec:
		{
			m_nodes[&_unaryOperation].hasSideEffects = true;
			// TODO this could be made more intelligent, especially in connecting to
			// e.g. deleting members of storage or memory structs.
			VariableDeclaration const* var = referencedVariable(_unaryOperation.subExpression());
			m_nodes[&_unaryOperation].writes.insert(var);
			auto t = _unaryOperation.subExpression().annotation().type;
			if (!t->isValueType() && !t->dataStoredIn(DataLocation::Storage))
			{
				// We know it does not write to storage.
			}
			else if (!var || var->isStateVariable())
				m_nodes[&_unaryOperation].writesStorage = true;
			break;
		default:
			solAssert(false, "Invalid unary operator: " + string(Token::toString(_unaryOperation.getOperator())));
		}
		}
	}
	virtual void endVisit(BinaryOperation const& _binaryOperation) override
	{
		Token::Value const c_op = _binaryOperation.getOperator();
		if (c_op == Token::And || c_op == Token::Or)
		{
			// TODO short-circuiting!
		}

		m_nodes[&_binaryOperation] =
			m_nodes[&_binaryOperation.leftExpression()] +
			m_nodes[&_binaryOperation.rightExpression()];
	}
	virtual void endVisit(FunctionCall const& _functionCall) override
	{
		CallgraphNode n = m_nodes[&_functionCall.expression()];
		for (auto const& arg: _functionCall.arguments())
			n = n + m_nodes[arg.get()];
		CallgraphNode& node = m_nodes[&_functionCall];
		node = n;

		for (auto const& arg: _functionCall.arguments())
			if (arg->annotation().type->dataStoredIn(DataLocation::Storage))
				node.readsStorage = true;
		if (auto const* ft = dynamic_cast<FunctionType const*>(_functionCall.expression().annotation().type.get()))
			if (ft->bound() && _functionCall.expression().annotation().type->dataStoredIn(DataLocation::Storage))
				node.readsStorage = true;

		if (_functionCall.annotation().kind != FunctionCallKind::FunctionCall)
			return;

		// just about anything might happen due to reentrancy, event "sends value".
		// What cannot happen is that local variables are written to.
		bool affectStateArbitrarily = false;
		bool hasSideEffects = true;
		bool calls = false;

		FunctionType const& function = dynamic_cast<FunctionType const&>(*_functionCall.expression().annotation().type);
		switch (function.kind())
		{
		case FunctionType::Kind::Internal:
		{
			affectStateArbitrarily = !function.isConstant();
			calls = true;
			break;
		}
		case FunctionType::Kind::External:
		case FunctionType::Kind::CallCode:
		case FunctionType::Kind::DelegateCall:
		case FunctionType::Kind::BareCall:
		case FunctionType::Kind::BareCallCode:
		case FunctionType::Kind::BareDelegateCall:
		case FunctionType::Kind::Creation:
		{
			affectStateArbitrarily = true;
			calls = true;
			break;
		}
		case FunctionType::Kind::Send:
		case FunctionType::Kind::Transfer:
		{
			calls = true;
			node.sendsValue = true;
			break;
		}
		case FunctionType::Kind::Selfdestruct:
		{
			calls = true;
			node.sendsValue = true;
			node.selfdestructs = true;
			break;
		}
		case FunctionType::Kind::Revert:
		case FunctionType::Kind::Assert:
		case FunctionType::Kind::Require:
		{
			// TODO model control flow here.
			hasSideEffects = false;
			break;
		}
		case FunctionType::Kind::Log0:
		case FunctionType::Kind::Log1:
		case FunctionType::Kind::Log2:
		case FunctionType::Kind::Log3:
		case FunctionType::Kind::Log4:
		case FunctionType::Kind::Event:
		{
			node.writesLogs = true;
			break;
		}
		case FunctionType::Kind::BlockHash:
		{
			hasSideEffects = false;
			node.readsEnvironment = true;
			break;
		}
		case FunctionType::Kind::ECRecover:
		case FunctionType::Kind::SHA256:
		case FunctionType::Kind::RIPEMD160:
		{
			calls = true;
			hasSideEffects = false;
			break;
		}
		case FunctionType::Kind::SetGas:
		case FunctionType::Kind::SetValue:
		case FunctionType::Kind::SHA3:
		case FunctionType::Kind::AddMod:
		case FunctionType::Kind::MulMod:
		case FunctionType::Kind::ObjectCreation:
		{
			hasSideEffects = false;
			break;
		}
		case FunctionType::Kind::ByteArrayPush:
		case FunctionType::Kind::ArrayPush:
		{
			// TODO determine the exact effects,
			// this might only write to a single variable.
			affectStateArbitrarily = true;
			break;
		}
		default:
			solAssert(false, "Invalid function type.");
		}

		if (hasSideEffects)
			node.hasSideEffects = true;
		if (calls)
			node.calls = true;
		if (affectStateArbitrarily)
		{
			CallgraphNode& node = m_nodes[&_functionCall];
			node.hasSideEffects = true;
			node.readsEnvironment = true;
			// TODO does not read or write local variables
			node.reads.insert(nullptr);
			node.writes.insert(nullptr);
			node.readsStorage = true;
			node.selfdestructs = true;
			node.sendsValue = true;
			node.writesLogs = true;
			node.writesStorage = true;
		}
	}
	virtual void endVisit(NewExpression const& _node) override
	{
		m_nodes[&_node] = m_nodes[&_node.typeName()];
	}
	virtual void endVisit(MemberAccess const& _memberAccess) override
	{
		m_nodes[&_memberAccess] = m_nodes[&_memberAccess.expression()];

		if (auto const& varDecl = dynamic_cast<VariableDeclaration const*>(_memberAccess.annotation().referencedDeclaration))
			m_nodes[&_memberAccess] += variableWasReferenced(*varDecl);

		ASTString const& member = _memberAccess.memberName();
		switch (_memberAccess.expression().annotation().type->category())
		{
		case Type::Category::Contract:
		case Type::Category::Integer:
			if (member == "balance" && !_memberAccess.annotation().referencedDeclaration)
				m_nodes[&_memberAccess].readsEnvironment = true;
			break;
		case Type::Category::Magic:
			// we can ignore the kind of magic and only look at the name of the member
			if (member != "data" && member != "sig")
				m_nodes[&_memberAccess].readsEnvironment = true;
			break;
		case Type::Category::Struct:
		{
			if (
				_memberAccess.expression().annotation().type->dataStoredIn(DataLocation::Storage) &&
				_memberAccess.annotation().type->isValueType()
			)
				m_nodes[&_memberAccess].readsStorage = true;
			break;
		}
		case Type::Category::Array:
		{
			auto const& type = dynamic_cast<ArrayType const&>(*_memberAccess.expression().annotation().type);
			if (member == "length" && type.isDynamicallySized() && type.dataStoredIn(DataLocation::Storage))
				m_nodes[&_memberAccess].readsStorage = true;
			break;
		}
		}
	}
	virtual void endVisit(IndexAccess const& _indexAccess) override
	{
		solAssert(_indexAccess.indexExpression(), "");
		m_nodes[&_indexAccess] = m_nodes[&_indexAccess.baseExpression()] + m_nodes[_indexAccess.indexExpression()];

		Type const& baseType = *_indexAccess.baseExpression().annotation().type;

		if (baseType.dataStoredIn(DataLocation::Storage) && _indexAccess.annotation().type->isValueType())
			m_nodes[&_indexAccess].readsStorage = true;
	}
	virtual void endVisit(Identifier const& _identifier) override
	{
		Declaration const* declaration = _identifier.annotation().referencedDeclaration;
		solAssert(declaration, "");

		if (VariableDeclaration const* varDecl = dynamic_cast<VariableDeclaration const*>(declaration))
		{
			m_nodes[&_identifier] = variableWasReferenced(*varDecl);
		}
		if (MagicVariableDeclaration const* magicVar = dynamic_cast<MagicVariableDeclaration const*>(declaration))
		{
			switch (magicVar->type()->category())
			{
			case Type::Category::Contract:
				// "this" or "super"
				if (!dynamic_cast<ContractType const&>(*magicVar->type()).isSuper())
					// reads the address
					m_nodes[&_identifier].readsEnvironment = true;
				break;
			case Type::Category::Integer:
				// "now"
				m_nodes[&_identifier].readsEnvironment = true;
				break;
			default:
				break;
			}
		}
	}
	virtual void endVisit(ElementaryTypeNameExpression const& _node) override { m_nodes[&_node]; }
	virtual void endVisit(Literal const& _node) override { m_nodes[&_node]; }

private:

	/// @returns the variable referenced in an lvalue expression - if any.
	VariableDeclaration const* referencedVariable(Expression const& _expression)
	{
		Declaration const* declaration = nullptr;
		if (Identifier const* identifier = dynamic_cast<Identifier const*>(&_expression))
			declaration = identifier->annotation().referencedDeclaration;
		else if (MemberAccess const* memberAccess = dynamic_cast<MemberAccess const*>(&_expression))
			declaration = memberAccess->annotation().referencedDeclaration;
		return dynamic_cast<VariableDeclaration const*>(declaration);
	}

	/// Used by identifiers or member access nodes that access a variable.
	CallgraphNode variableWasReferenced(VariableDeclaration const& _varDecl)
	{
		CallgraphNode r;
		r.reads.insert(_varDecl);
		if (_varDecl->isStateVariable() && !_varDecl->isConstant() && _varDecl.type()->isValueType())
			r.readsStorage = true;
		// Reference types do not really read from storage at this point.
		return r;
	}



	std::map<ASTNode const*, CallgraphNode>& m_nodes;
};

bool CallgraphBuilder::build(vector<ASTNode const&> const& _nodes)
{
	CallgraphBuilderVisitor v(m_nodes);

	for (auto const& node: _nodes)
		node.accept(v);
}
