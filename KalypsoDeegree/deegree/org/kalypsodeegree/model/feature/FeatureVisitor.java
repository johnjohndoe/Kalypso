package org.deegree.model.feature;

/**
 * @author belger
 */
public interface FeatureVisitor
{
  /** Do not recurse, visit all features of given type */
  public final static int DEPTH_ZERO = 0;
  
  /** Also visit features contained in features of given type; doesn't follow Links */
  public final static int DEPTH_INFINITE = 1;
  
  /** Even follow links (Attention, may cause in infinite loops!) */
  public final static int DEPTH_INFINITE_LINKS = 2;
  
  /** Visit the given feature
   * 
   * @return Return false, if recursion should stop now (overrules DEPTH_INFINITE_... )  
   */
  public boolean visit( final Feature f );
}
