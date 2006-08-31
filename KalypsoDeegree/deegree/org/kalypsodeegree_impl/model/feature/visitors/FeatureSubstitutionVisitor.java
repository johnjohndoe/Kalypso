package org.kalypsodeegree_impl.model.feature.visitors;

import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;

/**
 * <p>
 * Decorater over any FeatureVisitor, but only visits features of a given type.
 * </p>
 * <p>
 * Comparison is by name of the given type
 * </p>
 * 
 * @author belger
 */
public class FeatureSubstitutionVisitor implements FeatureVisitor
{
  private FeatureVisitor m_visitor;

  private final IFeatureType m_featureType;

  public FeatureSubstitutionVisitor( final FeatureVisitor visitor, final IFeatureType featureType )
  {
    m_visitor = visitor;
    m_featureType = featureType;
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
    final IFeatureType featureType = f.getFeatureType();

    return GMLSchemaUtilities.substitutes( featureType, m_featureType.getQName() );
  }

  public void setVisitor( final FeatureVisitor visitor )
  {
    m_visitor = visitor;
    
  }
}
