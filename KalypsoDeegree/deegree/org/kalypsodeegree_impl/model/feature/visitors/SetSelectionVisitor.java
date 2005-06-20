package org.kalypsodeegree_impl.model.feature.visitors;

import java.util.Map;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;

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
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    final Object object = m_selectionMap.get( f.getId() );
    if( object != null )
    {
      final int intValue = ( (Integer)object ).intValue();
      //      System.out.println( "Selecting feature " + f.getId() + " with " + intValue );
      f.setSelection( intValue );
    }

    return true;
  }

}
