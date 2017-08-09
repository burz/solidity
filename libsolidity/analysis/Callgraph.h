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

#pragma once

#include <libdevcore/CommonData.h>

#include <set>

namespace dev
{
namespace solidity
{

class Declaration;
class VariableDeclaration;
class ASTNode;

struct CallgraphNode
{
	/// Variable declarations this node writes to, unknown if it contains a nullptr.
	/// Note that due to aliasing, this might be incorrect for reference types.
	std::set<VariableDeclaration*> writes;
	/// Variable declaration this node reads from, unknown if it contains a nullptr.
	/// Note that due to aliasing, this might be incorrect for reference types.
	std::set<VariableDeclaration*> reads;
	/// Roughly, a node has side effects (at statement level) if it cannot be removed
	/// from the code without changing semantics. In particular, if a node
	/// performs any modifications to any state, it has side effects.
	/// Assignments to local variables count as side-effects,
	/// usage of temporary memory does not count as a side-effect (unless this memory
	/// is used elsewhere).
	/// If any modification to the state is captured only by the value of the
	/// expression, it does not have side effects.
	bool hasSideEffects = false;
	/// Reads from storage (of any account).
	bool readsStorage = false;
	/// Writes to storage (of any account).
	bool writesStorage = false;
	bool writesLogs = false;
	bool readsEnvironment = false;
	/// Whether it might performs an external call or create.
	bool calls = false;
	/// Whether it might send Ether somewhere.
	bool sendsValue = false;
	bool selfdestructs = false;

	bool canBeUsedInPureFunction()
	{
		return canBeUsedInViewFunction() && !readsStorage && !readsEnvironment;
	}

	bool canBeUsedInViewFunction()
	{
		return !writesStorage && !writesLogs && !sendsValue && !selfdestructs;
	}

	/// TODO this is not assigned yet
	/// The outgoing arches of the call graph node.
	/// Can contain a nullptr, which means "unknown".
	std::set<ASTNode const*> nextNodes;

	/// TODO this is not assigned yet
	/// Statements that are visited during execution but are not part of the
	/// syntactic area of the current node.
	/// Can contain a nullptr, which means "unknown".
	std::set<ASTNode const*> calledToNodes;

	CallgraphNode operator+(CallgraphNode const& _other)
	{
		CallgraphNode r = *this;
		r.writes += _other.writes;
		r.reads += _other.reads;
		if (_other.hasSideEffects)
			r.hasSideEffects = true;
		if (_other.readsStorage)
			r.readsStorage = true;
		if (_other.writesStorage)
			r.writesStorage = true;
		if (_other.writesLogs)
			r.writesLogs = true;
		if (_other.readsEnvironment)
			r.readsEnvironment = true;
		if (_other.calls)
			r.calls = true;
		if (_other.sendsValue)
			r.sendsValue = true;
		if (_other.selfdestructs)
			r.selfdestructs = true;
		r.calledToNodes += _other.calledToNodes;
		return r;
	}
};

}
}
