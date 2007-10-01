/**
 *
 */
package org.kalypso.google.earth.export.utils;

import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

import com.google.earth.kml.DocumentType;
import com.google.earth.kml.LatLonAltBoxType;
import com.google.earth.kml.LookAtType;
import com.google.earth.kml.ObjectFactory;
import com.google.earth.kml.RegionType;

/**
 * @author kuch
 */
public class GoogleEarthUtils
{

  /**
   * target coordinate system of google earth
   */
  public static final String GOOGLE_EARTH_CS = "EPSG:4326";

  /**
   * @param boundingBox
   * @param coordinatesSystem
   * @param factory
   * @param documentType
   * @throws Exception
   */
  public static void setLookAt( final GM_Envelope boundingBox, final CS_CoordinateSystem coordinatesSystem, final ObjectFactory factory, final DocumentType documentType ) throws Exception
  {
    // set look at to the middle of bounding box
    final GeoTransformer transformer = new GeoTransformer( GoogleEarthUtils.GOOGLE_EARTH_CS );

    final GM_Position min = boundingBox.getMin();

    // point
    final GM_Position pos = GeometryFactory.createGM_Position( min.getX() + boundingBox.getWidth() / 2, min.getY() + boundingBox.getHeight() / 2 );
    final GM_Point middle = (GM_Point) transformer.transform( GeometryFactory.createGM_Point( pos, coordinatesSystem ) );

    final LookAtType lookAtType = factory.createLookAtType();
    lookAtType.setAltitude( 12000.0 );
    lookAtType.setLatitude( middle.getY() );
    lookAtType.setLongitude( middle.getX() );

    documentType.setLookAt( lookAtType );
  }

  /**
   * @param boundingBox
   * @param srcCRS
   * @param factory
   * @param documentType
   * @throws Exception
   */
  public static void setMapBoundary( final GM_Envelope boundingBox, final CS_CoordinateSystem srcCRS, final ObjectFactory factory, final DocumentType documentType ) throws Exception
  {

    final GeoTransformer transformer = new GeoTransformer( GoogleEarthUtils.GOOGLE_EARTH_CS );

    final GM_Position min = boundingBox.getMin();
    final GM_Position max = boundingBox.getMax();

    // north = min.getX
    // west = min.gety
    final GM_Point northWest = (GM_Point) transformer.transform( GeometryFactory.createGM_Point( min, srcCRS ) );

    // south = max.getX
    // east = max.getY
    final GM_Point southEast = (GM_Point) transformer.transform( GeometryFactory.createGM_Point( max, srcCRS ) );

    // set google earth boundaries
    final LatLonAltBoxType latLonBox = factory.createLatLonAltBoxType();
    latLonBox.setNorth( northWest.getY() );
    latLonBox.setWest( northWest.getX() );
    latLonBox.setSouth( southEast.getY() );
    latLonBox.setEast( southEast.getX() );

    final RegionType region = factory.createRegionType();
    region.setLatLonAltBox( latLonBox );
    documentType.setRegion( region );
  }
}
