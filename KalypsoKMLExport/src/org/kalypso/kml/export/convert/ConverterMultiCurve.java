/**
 *
 */
package org.kalypso.kml.export.convert;

import java.util.List;

import javax.xml.bind.JAXBElement;

import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;

import com.google.earth.kml.GeometryType;
import com.google.earth.kml.MultiGeometryType;
import com.google.earth.kml.ObjectFactory;

/**
 * @author Dirk Kuch
 */
public class ConverterMultiCurve
{

  /**
   * @param factory
   * @param string
   * @param multiCurve
   * @return
   * @throws Exception
   */
  public static MultiGeometryType convert( final ObjectFactory factory, final GM_MultiCurve multiCurve ) throws Exception
  {
    final MultiGeometryType multiGeometryType = factory.createMultiGeometryType();
    final List<JAXBElement< ? extends GeometryType>> geometries = multiGeometryType.getGeometry();

    final GM_Curve[] curves = multiCurve.getAllCurves();
    for( final GM_Curve curve : curves )
      geometries.add( factory.createLineString( ConverterCurve.convert( factory, curve ) ) );

    return multiGeometryType;
  }

}
