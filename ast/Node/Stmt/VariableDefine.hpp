#pragma once

#include <string>

#include "../Stmt.hpp"

namespace ast {

class Node::Stmt::VariableDefine : public Stmt {

public:
    const std::string name;

    const Node::Type * const type;
    const Node::Expr * const value;

    inline VariableDefine(std::string && name, const Node::Type * type, const Node::Expr * value) noexcept :
        name(std::move(name)), type(type), value(value) {}

protected:
    inline ~VariableDefine() noexcept override = default;
};

}
