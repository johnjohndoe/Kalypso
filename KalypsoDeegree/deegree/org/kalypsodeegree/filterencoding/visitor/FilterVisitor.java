package org.kalypsodeegree.filterencoding.visitor;

import org.kalypsodeegree.filterencoding.Operation;

/**
 * @author kuepfer
 */
public interface FilterVisitor
{
  /** Do not recurse, visit all features of given type */
  public final static int DEPTH_ZERO = 0;

  /** Also visit features contained in features of given type; doesn't follow Links */
  public final static int DEPTH_INFINITE = 1;

  /**
   * Visit the given operation
   * 
   * @return Return false, if recursion should stop now (overrules DEPTH_INFINITE_... )
   */
  public boolean visit( Operation operation );
}
