package org.kalypsodeegree_impl.model.feature.visitors;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;

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
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    f.select( m_selectionID );
    
    return true;
  }
}
