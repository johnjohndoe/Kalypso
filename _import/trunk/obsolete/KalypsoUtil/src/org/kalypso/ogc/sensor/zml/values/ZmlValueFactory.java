package org.kalypso.ogc.sensor.zml.values;

import org.kalypso.ogc.sensor.zml.ZmlAxis;
import org.kalypso.zml.AxisType;


/**
 * ValueFactory um die entsprechende ValuesLoader zu erzeugen.
 * 
 * @author schlienger
 */
public class ZmlValueFactory
{
  public static IZmlValuesLoader createLoader( final String currentPath, final AxisType axisType, final ZmlAxis axis )
  {
    Object va = axisType.getValueArray();
    if( va != null )
      return new ValueArray( (AxisType.ValueArrayType)va, axis );
    
    Object vl = axisType.getValueLink();
    if( vl != null )
      return new ValueLink( currentPath, (AxisType.ValueLinkType)vl, axis );

    throw new IllegalArgumentException( "AxisType is not supported: " + axisType.toString() );
  }
}
