/**
 *
 */
package org.kalypso.kml.export.convert;

import java.util.List;

import javax.xml.bind.JAXBElement;

import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Surface;

import com.google.earth.kml.GeometryType;
import com.google.earth.kml.MultiGeometryType;
import com.google.earth.kml.ObjectFactory;

/**
 * @author kuch
 */
public class ConverterMultiSurface
{

  /**
   * @param factory
   * @param gmo
   * @return
   * @throws Exception
   */
  public static MultiGeometryType convert( final ObjectFactory factory, final GM_MultiSurface gmo ) throws Exception
  {
    final MultiGeometryType multiGeometryType = factory.createMultiGeometryType();
    final List<JAXBElement< ? extends GeometryType>> geometries = multiGeometryType.getGeometry();

    final GM_Surface< ? >[] surfaces = gmo.getAllSurfaces();
    for( final GM_Surface< ? > surface : surfaces )
      geometries.add( factory.createPolygon( ConverterSurface.convert( factory, surface ) ) );

    return multiGeometryType;
  }

}
