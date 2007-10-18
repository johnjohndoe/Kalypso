package org.kalypso.risk.model.schema.propertyFunctions;

import java.util.Map;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.risk.model.schema.binding.IWaterdepthCoverage;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;

public class VirtualRasterGeometryProperty extends FeaturePropertyFunction
{

  @Override
  public void init( Map<String, String> properties )
  {
    // TODO Auto-generated method stub
    
  }

  public Object getValue( Feature feature, IPropertyType pt, Object currentValue )
  {
    final RectifiedGridDomain rgDomain = (RectifiedGridDomain) feature.getProperty( IWaterdepthCoverage.PROP_RECTIFIED_GRID_DOMAIN );
    if( rgDomain == null )
      return null;
    try
    {
      // ASK: convert to targetCrs with help of TransformVisitor?!?
      return rgDomain.getGM_Surface( rgDomain.getCoordinateSystem() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  public Object setValue( Feature feature, IPropertyType pt, Object valueToSet )
  {
    // TODO Auto-generated method stub
    return null;
  }

}
