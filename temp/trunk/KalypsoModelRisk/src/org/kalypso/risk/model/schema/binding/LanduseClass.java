package org.kalypso.risk.model.schema.binding;

import org.eclipse.swt.graphics.RGB;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;

public class LanduseClass extends AbstractFeatureBinder implements ILanduseClass
{
  public LanduseClass( final Feature featureToBind )
  {
    super( featureToBind, QNAME );
  }

  public RGB getColorStyle( )
  {
    return (RGB) getFeature().getProperty( ILanduseClass.PROP_COLOR_STYLE );
  }

  public void setColorStyle( final RGB rgb )
  {
    getFeature().setProperty( ILanduseClass.PROP_COLOR_STYLE, rgb );
  }

  public void setOrdinalNumber( int value )
  {
    getFeature().setProperty( ILanduseClass.PROP_ORDINAL_NUMBER, value );
  }

  public int getOrdinalNumber( )
  {
    final Integer value = (Integer) getFeature().getProperty( ILanduseClass.PROP_ORDINAL_NUMBER );
    return value == null ? 0 : value.intValue();
  }

  public double getMaxDamage( )
  {
    final Double value = (Double) getFeature().getProperty( ILanduseClass.PROP_MAX_DAMAGE );
    return value == null ? 0 : value.intValue();
  }

  public double getMinDamage( )
  {
    final Double value = (Double) getFeature().getProperty( ILanduseClass.PROP_MIN_DAMAGE );
    return value == null ? Double.MAX_VALUE : value.intValue();
  }

  public double getTotalDamage( )
  {
    final Double value = (Double) getFeature().getProperty( ILanduseClass.PROP_TOTAL_DAMAGE );
    return value == null ? 0 : value.intValue();
  }

  public void setMaxDamage( double value )
  {
    getFeature().setProperty( ILanduseClass.PROP_MAX_DAMAGE, value );
  }

  public void setMinDamage( double value )
  {
    getFeature().setProperty( ILanduseClass.PROP_MIN_DAMAGE, value );
  }

  public void setTotalDamage( double value )
  {
    getFeature().setProperty( ILanduseClass.PROP_TOTAL_DAMAGE, value );
  }

}
