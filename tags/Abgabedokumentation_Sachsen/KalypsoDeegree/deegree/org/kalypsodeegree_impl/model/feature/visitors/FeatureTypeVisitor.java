package org.kalypsodeegree_impl.model.feature.visitors;

import java.util.ArrayList;
import java.util.Collection;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureVisitor;

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

  /** Falls true, werden auch features acceptiert, welche den angegebenen Typ substituieren */
  private final boolean m_acceptIfSubstituting;

  public FeatureTypeVisitor( final FeatureType ft, final boolean acceptIfSubstituting )
  {
    this( ft.getName(), acceptIfSubstituting );
  }

  public FeatureTypeVisitor( final String typename, final boolean acceptIfSubstituting )
  {
    m_typename = typename;
    m_acceptIfSubstituting = acceptIfSubstituting;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    final FeatureType featureType = f.getFeatureType();

    final String substName = featureType.getNamespace() + ":" + m_typename;
    
    if( m_typename.equals( featureType.getName() ) || ( m_acceptIfSubstituting && substName.equals( featureType.getSubstitutionGroup() ) ) )
      m_results.add( f );
    
    return true;
  }

  public Collection getResults()
  {
    return m_results;
  }
}
