package org.deegree_impl.model.feature.visitors;

import java.util.ArrayList;
import java.util.Collection;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureVisitor;

/**
 * <p>Collects all features of a given type</p>
 * <p>Comparisaon is by name of the given type</p>
 * 
 * @author belger
 */
public class FeatureTypeVisitor implements FeatureVisitor
{
  private final Collection m_results = new ArrayList();
  
  private final String m_typename;

  public FeatureTypeVisitor( final FeatureType ft )
  {
    this( ft.getName() );
  }

  public FeatureTypeVisitor( final String typename )
  {
    m_typename = typename;
  }

  /**
   * @see org.deegree.model.feature.FeatureVisitor#visit(org.deegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    if( m_typename.equals( f.getFeatureType().getName() ) )
      m_results.add( f );
    
    return true;
  }

  public Collection getResults()
  {
    return m_results;
  }
}
