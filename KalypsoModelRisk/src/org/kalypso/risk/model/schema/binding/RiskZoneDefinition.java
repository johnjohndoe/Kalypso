package org.kalypso.risk.model.schema.binding;

import org.eclipse.swt.graphics.RGB;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

public class RiskZoneDefinition extends Feature_Impl implements IRiskZoneDefinition
{
  public RiskZoneDefinition( Object parent, IRelationType parentRelation, IFeatureType ft, String id, Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public RGB getColorStyle( )
  {
    return (RGB) getProperty( IRiskZoneDefinition.PROP_COLOR_STYLE );
  }

  @Override
  public double getLowerBoundary( )
  {
    final Double property = (Double) getProperty( IRiskZoneDefinition.PROP_LOWER_BOUNDARY );
    return property == null ? 0.0 : property.doubleValue();
  }

  @Override
  public void setLowerBoundary( final double value )
  {
    setProperty( IRiskZoneDefinition.PROP_LOWER_BOUNDARY, value );
  }

  @Override
  public void setColorStyle( final RGB rgb )
  {
    setProperty( IRiskZoneDefinition.PROP_COLOR_STYLE, rgb );
  }

  @Override
  public int getOrdinalNumber( )
  {
    final Integer property = (Integer) getProperty( IRiskZoneDefinition.PROP_ORDINAL_NUMBER );
    return property == null ? 0 : property.intValue();
  }

  @Override
  public String getDescription( )
  {
    return (String) getProperty( IRiskZoneDefinition.PROP_DESCRIPTION );
  }

  @Override
  public String getName( )
  {
    return (String) getProperty( IRiskZoneDefinition.PROP_NAME );
  }

  @Override
  public Boolean isUrbanLanduseType( )
  {
    return (Boolean) getProperty( IRiskZoneDefinition.PROP_ISURBANTYPE );
  }

}
