package org.kalypsodeegree_impl.model.feature.visitors;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureVisitor;

/**
 * <p>
 * Decorater over any FeatureVisitor, but only visits features of a given type.
 * </p>
 * <p>
 * Comparisaon is by name of the given type
 * </p>
 * 
 * @author belger
 */
public class FeatureTypeVisitor implements FeatureVisitor
{
  private final String m_typename;

  /** Falls true, werden auch features acceptiert, welche den angegebenen Typ substituieren */
  private final boolean m_acceptIfSubstituting;

  private FeatureVisitor m_visitor;

  public FeatureTypeVisitor( final FeatureVisitor visitor, final FeatureType ft, final boolean acceptIfSubstituting )
  {
    this( visitor, ft.getName(), acceptIfSubstituting );
  }

  public FeatureTypeVisitor( final FeatureVisitor visitor, final String typename, final boolean acceptIfSubstituting )
  {
    m_visitor = visitor;
    m_typename = typename;
    m_acceptIfSubstituting = acceptIfSubstituting;
  }

  public void setVisitor( FeatureVisitor visitor )
  {
    m_visitor = visitor;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    if( matchesType( f ) )
      m_visitor.visit( f );

    return true;
  }

  public boolean matchesType( final Feature f )
  {
    final FeatureType featureType = f.getFeatureType();

    final String substName = featureType.getNamespace() + ":" + m_typename;
    return ( m_typename.equals( featureType.getName() ) || ( m_acceptIfSubstituting && substName.equals( featureType
        .getSubstitutionGroup() ) ) );
  }
}
