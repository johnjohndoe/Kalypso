package org.deegree_impl.model.feature.visitors;

import java.util.Map;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureVisitor;

/**
 * Setzt die Selektion aller Features anhand einer Map: id -> new Integer( selection )
 * 
 * @author belger
 */
public class SetSelectionVisitor implements FeatureVisitor
{
  private final Map m_selectionMap;

  public SetSelectionVisitor( final Map selectionMap )
  {
    m_selectionMap = selectionMap;
  }

  /**
   * @see org.deegree.model.feature.FeatureVisitor#visit(org.deegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    final Object object = m_selectionMap.get( f.getId() );
    if( object != null )
      f.select( ((Integer)object).intValue() );
    
    return true;
  }

}
