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
	virtual void endVisit(Throw const& _node) override {}
	virtual void endVisit(VariableDeclarationStatement const& _node) override {}
	virtual void endVisit(ExpressionStatement const& _node) override {}
	virtual void endVisit(Conditional const& _node) override {}
	virtual void endVisit(Assignment const& _node) override {}
	virtual void endVisit(TupleExpression const& _node) override {}
	virtual void endVisit(UnaryOperation const& _node) override {}
	virtual void endVisit(BinaryOperation const& _node) override {}
	virtual void endVisit(FunctionCall const& _node) override {}
	virtual void endVisit(NewExpression const& _node) override {}
	virtual void endVisit(MemberAccess const& _node) override {}
	virtual void endVisit(IndexAccess const& _indexAccess) override
	{
		solAssert(_indexAccess.indexExpression(), "");
		m_nodes[&_indexAccess] = m_nodes[_indexAccess.baseExpression()] + m_nodes[_indexAccess.indexExpression()];

		Type const& baseType = *_indexAccess.baseExpression().annotation().type;

		if (baseType.category() == Type::Category::Mapping)
		{
			solAssert(m_nodes[&_indexAccess].readsStorage, "");
		}
		else if (baseType.category() == Type::Category::Array)
		{
			ArrayType const& arrayType = dynamic_cast<ArrayType const&>(baseType);
			solAssert(_indexAccess.indexExpression(), "Index expression expected.");

			_indexAccess.indexExpression()->accept(*this);
			utils().convertType(*_indexAccess.indexExpression()->annotation().type, IntegerType(256), true);
			// stack layout: <base_ref> [<length>] <index>
			ArrayUtils(m_context).accessIndex(arrayType);
			switch (arrayType.location())
			{
			case DataLocation::Storage:
				if (arrayType.isByteArray())
				{
					solAssert(!arrayType.isString(), "Index access to string is not allowed.");
					setLValue<StorageByteArrayElement>(_indexAccess);
				}
				else
					setLValueToStorageItem(_indexAccess);
				break;
			case DataLocation::Memory:
				setLValue<MemoryItem>(_indexAccess, *_indexAccess.annotation().type, !arrayType.isByteArray());
				break;
			case DataLocation::CallData:
				//@todo if we implement this, the value in calldata has to be added to the base offset
				solUnimplementedAssert(!arrayType.baseType()->isDynamicallySized(), "Nested arrays not yet implemented.");
				if (arrayType.baseType()->isValueType())
					CompilerUtils(m_context).loadFromMemoryDynamic(
						*arrayType.baseType(),
						true,
						!arrayType.isByteArray(),
						false
					);
				break;
			}
		}
		else if (baseType.category() == Type::Category::FixedBytes)
		{
			FixedBytesType const& fixedBytesType = dynamic_cast<FixedBytesType const&>(baseType);
			solAssert(_indexAccess.indexExpression(), "Index expression expected.");

			_indexAccess.indexExpression()->accept(*this);
			utils().convertType(*_indexAccess.indexExpression()->annotation().type, IntegerType(256), true);
			// stack layout: <value> <index>
			// check out-of-bounds access
			m_context << u256(fixedBytesType.numBytes());
			m_context << Instruction::DUP2 << Instruction::LT << Instruction::ISZERO;
			// out-of-bounds access throws exception
			m_context.appendConditionalInvalid();

			m_context << Instruction::BYTE;
			utils().leftShiftNumberOnStack(256 - 8);
		}
		else if (baseType.category() == Type::Category::TypeType)
		{
			solAssert(baseType.sizeOnStack() == 0, "");
			solAssert(_indexAccess.annotation().type->sizeOnStack() == 0, "");
			// no-op - this seems to be a lone array type (`structType[];`)
		}
		else
			solAssert(false, "Index access only allowed for mappings or arrays.");

		return false;
	}
	virtual void endVisit(Identifier const& _identifier) override
	{
		Declaration const* declaration = _identifier.annotation().referencedDeclaration;
		solAssert(declaration, "");
		m_nodes[&_identifier].reads.insert(declaration);
		// TODO "lValueRequested" is not strict enough for passing storage or memory pointers.
		if (_identifier.annotation().lValueRequested)
			m_nodes[&_identifier].writes.insert(declaration);
		if (declaration->type()->dataStoredIn(DataLocation::Storage))
		{
			// TODO "lValueRequested" is not strict enough for passing storage or memory pointers.
			m_nodes[&_identifier].writesStorage = _identifier.annotation().lValueRequested;
			m_nodes[&_identifier].readsStorage = true;
		}

		if (MagicVariableDeclaration const* magicVar = dynamic_cast<MagicVariableDeclaration const*>(declaration))
		{
			switch (magicVar->type()->category())
			{
			case Type::Category::Contract:
				// "this" or "super"
				if (!dynamic_cast<ContractType const&>(*magicVar->type()).isSuper())
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
	std::map<ASTNode const*, CallgraphNode>& m_nodes;
};

bool CallgraphBuilder::build(vector<ASTNode const&> const& _nodes)
{
	CallgraphBuilderVisitor v(m_nodes);

	for (auto const& node: _nodes)
		node.accept(v);
}
