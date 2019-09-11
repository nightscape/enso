package org.enso.interpreter.node.callable.argument;

import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import org.enso.interpreter.node.ExpressionNode;

public class SuspensionNode extends ExpressionNode {
  @Child private ExpressionNode exprNode;

  public SuspensionNode(ExpressionNode exprNode) {
    this.exprNode = exprNode;
  }

  @Override
  public Object executeGeneric(VirtualFrame frame) {
    return exprNode.executeGeneric((MaterializedFrame) frame.getArguments()[0]);
  }
}
