package org.deegree_impl.model.feature.visitors;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureVisitor;

/**
 * unselects all features off the given id
 * 
 * @author belger
 */
public class UnselectFeatureVisitor implements FeatureVisitor
{
  private final int m_selectionID;

  public UnselectFeatureVisitor( final int selectionID )
  {
    m_selectionID = selectionID;
  }

  /**
   * @see org.deegree.model.feature.FeatureVisitor#visit(org.deegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    f.unselect( m_selectionID );
    
    return true;
  }
}
