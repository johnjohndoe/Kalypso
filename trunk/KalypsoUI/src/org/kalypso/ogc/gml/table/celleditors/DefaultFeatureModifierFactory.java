package org.kalypso.ogc.gml.table.celleditors;

import org.deegree.model.feature.FeatureTypeProperty;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;
import org.kalypso.ogc.gml.featureview.modfier.BooleanModifier;
import org.kalypso.ogc.gml.featureview.modfier.StringModifier;

/**
 * @author Belger
 */
public class DefaultFeatureModifierFactory implements IFeatureModifierFactory
{
  /**
   * @see org.kalypso.ogc.gml.table.celleditors.IFeatureModifierFactory#createFeatureModifier(org.deegree.model.feature.FeatureTypeProperty)
   */
  public IFeatureModifier createFeatureModifier( final FeatureTypeProperty ftp )
  {
    final String type = ftp.getType();

    if( "java.lang.String".equals( type ) )
      return new StringModifier( ftp );
    if( "java.lang.Integer".equals( type ) )
      return new StringModifier( ftp );
    if( "java.lang.Long".equals( type ) )
      return new StringModifier( ftp );
    if( "java.lang.Float".equals( type ) )
      return new StringModifier( ftp );
    if( "java.lang.Double".equals( type ) )
      return new StringModifier( ftp );
    if( "java.lang.Date".equals( type ) )
      return new StringModifier( ftp );
    if( "java.lang.String".equals( type ) )
      return new StringModifier( ftp );
    if( "java.lang.Boolean".equals( type ) )
      return new BooleanModifier( ftp );

    return new StringModifier( ftp );
  }
}