package org.kalypso.risk.model.schema.binding;

import org.eclipse.swt.graphics.RGB;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;

public class RiskZoneDefinition extends AbstractFeatureBinder implements IRiskZoneDefinition
{
  public RiskZoneDefinition( final Feature featureToBind )
  {
    super( featureToBind, IRiskZoneDefinition.QNAME );
  }

  public RGB getColorStyle( )
  {
    return (RGB) getFeature().getProperty( IRiskZoneDefinition.PROP_COLOR_STYLE );
  }

  public double getLowerBoundary( )
  {
    final Double property = (Double) getFeature().getProperty( IRiskZoneDefinition.PROP_LOWER_BOUNDARY );
    return property == null ? 0.0 : property.doubleValue();
  }

  public void setLowerBoundary( final double value )
  {
    getFeature().setProperty( IRiskZoneDefinition.PROP_LOWER_BOUNDARY, value );
  }

  public void setColorStyle( final RGB rgb )
  {
    getFeature().setProperty( IRiskZoneDefinition.PROP_COLOR_STYLE, rgb );
  }

  public int getOrdinalNumber( )
  {
    final Integer property = (Integer) getFeature().getProperty( IRiskZoneDefinition.PROP_ORDINAL_NUMBER );
    return property == null ? 0 : property.intValue();
  }

  @Override
  public String getDescription( )
  {
    return (String) getFeature().getProperty( IRiskZoneDefinition.PROP_DESCRIPTION );
  }

  @Override
  public String getName( )
  {
    return (String) getFeature().getProperty( IRiskZoneDefinition.PROP_NAME );
  }

  public Boolean isUrbanLanduseType( )
  {
    return (Boolean) getFeature().getProperty( IRiskZoneDefinition.PROP_ISURBANTYPE );
  }

}
