package org.kalypsodeegree_impl.model.feature.visitors;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypsodeegree.model.feature.Feature;
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

  public FeatureTypeVisitor( final FeatureVisitor visitor, final IFeatureType ft, final boolean acceptIfSubstituting )
  {
    this( visitor, ft.getQName().getLocalPart(), acceptIfSubstituting );
  }

  public FeatureTypeVisitor( final FeatureVisitor visitor, final String typeLocalPart, final boolean acceptIfSubstituting )
  {
    m_visitor = visitor;
    m_typename = typeLocalPart;
    m_acceptIfSubstituting = acceptIfSubstituting;
  }

  public void setVisitor( final FeatureVisitor visitor )
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
    if( f == null )
      return false;

    final IFeatureType featureType = f.getFeatureType();
    // final FeatureType[] substituts = f.getFeatureType().getSubstituts( m_context, false, true );

    // final String substName = featureType.getNamespace() + ":" + m_typename;
    // return (m_typename.equals( featureType.getQName() ) || (m_acceptIfSubstituting && substName.equals(
    // featureType.getSubstitutionGroup() )));
    if( m_typename.equals( featureType.getName() ) )
      return true;
    if( m_acceptIfSubstituting )
    {
      return m_typename.equals( f.getFeatureType().getSubstitutionGroupFT().getQName().getLocalPart() );
    }
    return false;
  }
}
