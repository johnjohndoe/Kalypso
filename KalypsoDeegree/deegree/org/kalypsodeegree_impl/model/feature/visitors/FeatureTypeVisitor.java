package org.deegree_impl.model.feature.visitors;

import java.util.ArrayList;
import java.util.Collection;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureVisitor;

/**
 * Collects all features of a given type
 * 
 * @author belger
 */
public class FeatureTypeVisitor implements FeatureVisitor
{
  private final Collection m_results = new ArrayList();
  
  private final FeatureType m_ft;

  public FeatureTypeVisitor( final FeatureType ft )
  {
    m_ft = ft;
  }

  /**
   * @see org.deegree.model.feature.FeatureVisitor#visit(org.deegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    if( f.getFeatureType().equals( m_ft ) )
      m_results.add( f );
    
    return true;
  }

  public Collection getResults()
  {
    return m_results;
  }
}
