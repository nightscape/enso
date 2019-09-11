package org.enso.interpreter.runtime.callable.argument;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.frame.MaterializedFrame;

public class Suspension {
  private final RootCallTarget callTarget;
  private final MaterializedFrame scope;

  public Suspension(RootCallTarget callTarget, MaterializedFrame scope) {
    this.callTarget = callTarget;
    this.scope = scope;
  }

  public RootCallTarget getCallTarget() {
    return callTarget;
  }

  public MaterializedFrame getScope() {
    return scope;
  }
}
