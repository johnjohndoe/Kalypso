package org.deegree_impl.model.feature.visitors;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureVisitor;

/**
 * selects all features with the given id
 * 
 * @author belger
 */
public class SelectFeatureVisitor implements FeatureVisitor
{
  private final int m_selectionID;

  public SelectFeatureVisitor( final int selectionID )
  {
    m_selectionID = selectionID;
  }

  /**
   * @see org.deegree.model.feature.FeatureVisitor#visit(org.deegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    f.select( m_selectionID );
    
    return true;
  }
}
