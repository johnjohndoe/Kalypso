package org.kalypso.ogc.sensor.zml.values;

import java.net.MalformedURLException;
import java.net.URL;

import org.kalypso.ogc.sensor.zml.ZmlAxis;
import org.kalypso.zml.AxisType;


/**
 * ValueFactory um die entsprechende ValuesLoader zu erzeugen.
 * 
 * @author schlienger
 */
public class ZmlValueFactory
{
  public static IZmlValuesLoader createLoader( final URL baseUrl, final AxisType axisType, final ZmlAxis axis ) throws MalformedURLException
  {
    // loader for inline values, no need to specify where base location is
    Object va = axisType.getValueArray();
    if( va != null )
      return new ValueArray( (AxisType.ValueArrayType)va, axis );
    
    // loader for linked values, here we specify where base location is
    Object vl = axisType.getValueLink();
    if( vl != null )
      return new ValueLink( baseUrl, (AxisType.ValueLinkType)vl, axis );

    throw new IllegalArgumentException( "AxisType is not supported: " + axisType.toString() );
  }
}
