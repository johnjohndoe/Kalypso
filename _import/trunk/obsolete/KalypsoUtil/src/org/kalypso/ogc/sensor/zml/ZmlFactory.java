package org.kalypso.ogc.sensor.zml;

import org.kalypso.zml.AxisType;


/**
 * @author schlienger
 *
 */
public class ZmlFactory
{
  public static IZmlValuesLoader createLoader( AxisType axisType )
  {
    Object va = axisType.getValueArray();
    if( va != null )
      return new ValueArrayLoader( (AxisType.ValueArrayType)va );
    
    Object vl = axisType.getValueLink();
    if( vl != null )
      return new ValueLinkLoader( (AxisType.ValueLinkType)vl );

    throw new IllegalArgumentException( "AxisType is not supported: " + axisType.toString() );
  }
}
