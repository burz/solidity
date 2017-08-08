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
class ASTNode;

struct CallgraphNode
{
	// Declarations this node writes to, unknown if it contains a nullptr.
	std::set<Declaration*> writes;
	// Declaration this node reads from, unknown if it contains a nullptr.
	std::set<Declaration*> reads;
	bool readsStorage = false;
	bool writesStorage = false;
	bool readsEnvironment = false;
	/// Whether it might performs an external call or create.
	bool calls = false;
	/// Whether it might send Ether somewhere.
	bool sendsValue = false;

	/// The outgoing arches of the call graph node.
	/// Can contain a nullptr, which means "unknown".
	std::set<ASTNode const*> nextNodes;

	/// Statements that are visited during execution but are not part of the
	/// syntactic area of the current node.
	/// Can contain a nullptr, which means "unknown".
	std::set<ASTNode const*> calledToNodes;

	CallgraphNode operator+(CallgraphNode const& _other)
	{
		CallgraphNode r = *this;
		r.writes += _other.writes;
		r.reads += _other.reads;
		if (_other.readsStorage)
			r.readsStorage = true;
		if (_other.writesStorage)
			r.writesStorage = true;
		if (_other.readsEnvironment)
			r.readsEnvironment = true;
		if (_other.calls)
			r.calls = true;
		if (_other.sendsValue)
			r.sendsValue = true;
		r.calledToNodes += _other.calledToNodes;
		return r;
	}
};

}
}
