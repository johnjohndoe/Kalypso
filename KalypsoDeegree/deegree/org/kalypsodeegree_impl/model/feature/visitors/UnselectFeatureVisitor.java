package org.kalypsodeegree_impl.model.feature.visitors;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;

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
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    f.unselect( m_selectionID );

    return true;
  }
}
