package org.kalypsodeegree_impl.model.feature.visitors;

import java.util.HashMap;
import java.util.Map;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;

/**
 * Indiziert alle besuchten Features anhand einer ihrer Properties.
 * Die Features werden in eine HashMap id -> feature gesteckt.
 * 
 * @author bce
 */
public class IndexFeaturesVisitor implements FeatureVisitor
{
  private final String m_indexProperty;
  
  private final Map m_index = new HashMap();

  public IndexFeaturesVisitor( final String indexProperty )
  {
    m_indexProperty = indexProperty;

  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    final Object property = f.getProperty( m_indexProperty );
    m_index.put( property, f );
    
    return true;
  }
  
  public Map getIndex()
  {
    return m_index;
  }
}
