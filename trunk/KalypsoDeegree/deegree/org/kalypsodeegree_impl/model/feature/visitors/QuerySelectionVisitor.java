package org.kalypsodeegree_impl.model.feature.visitors;

import java.util.HashMap;
import java.util.Map;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;

/**
 * Schreibt die Selektionen aller besuchten Features in eine Map: id -> new
 * Integer( selection )
 * 
 * @author belger
 */
public class QuerySelectionVisitor implements FeatureVisitor
{
  private final Map m_selectionMap;

  /**
   * Erzeugt eine eigene Map zum sammeln, kann über {@link #getSelectionMap()}
   * dann abgefragt werden
   */
  public QuerySelectionVisitor()
  {
    this( new HashMap() );
  }

  /** Verwendet die übergebene Map zum sammeln */
  public QuerySelectionVisitor( final Map selectionMap )
  {
    m_selectionMap = selectionMap;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    m_selectionMap.put( f.getId(), new Integer( f.getSelection() ) );

    return true;
  }

  public Map getSelectionMap()
  {
    return m_selectionMap;
  }
}