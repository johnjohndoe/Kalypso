package org.kalypso.grid.processes.raster2vector.collector;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.kalypso.grid.processes.raster2vector.LinkedCoordinate;
import org.kalypso.grid.processes.raster2vector.LinkedCoordinateException;
import org.kalypso.grid.processes.raster2vector.SegmentCollector;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * Sammelt LineStrings
 * 
 * @author belger
 */
public class LineStringCollector implements SegmentCollector
{
  private final GeometryFactory m_gf;

  private final double[] m_grenzen;

  private final boolean m_bSimple;

  private final List<CollectorDataProvider> m_dataList = new ArrayList<CollectorDataProvider>();

  public LineStringCollector( final GeometryFactory gf, final double[] grenzen, final boolean bSimple )
  {
    m_grenzen = grenzen;
    m_gf = gf;
    m_bSimple = bSimple;
  }

  /**
   * @see com.bce.gis.operation.raster2vector.SegmentCollector#addSegment(int,
   *      com.bce.gis.operation.raster2vector.LinkedCoordinate, com.bce.gis.operation.raster2vector.LinkedCoordinate,
   *      com.vividsolutions.jts.geom.Coordinate, com.vividsolutions.jts.geom.Coordinate)
   */
  public void addSegment( final int index, final LinkedCoordinate lc0, final LinkedCoordinate lc1, final Coordinate nearC0, final Coordinate nearC1 ) throws LinkedCoordinateException
  {
    try
    {
      if( m_bSimple )
      {
        final Double id = new Double( m_dataList.size() );
        final Double grenze = new Double( m_grenzen[index] );
        final String name = grenze.toString();

        final GM_Object gmGeo = JTSAdapter.wrap( m_gf.createLineString( new Coordinate[] { lc0.crd, lc1.crd } ) );
        if( gmGeo instanceof GM_Curve )
        {
          final CollectorDataProvider dataProvider = new CollectorDataProvider( gmGeo, new Double[] { grenze }, id, new String[] { name } );
          m_dataList.add( dataProvider );
        }
      }
      else
      {
        lc0.link( lc1 );

        if( !lc0.isCircle() )
          return;

        final Collection<LineString> newStrings = lc0.getLineStrings( m_gf );

        for( final LineString lineString : newStrings )
        {
          final GM_Object gmGeo = JTSAdapter.wrap( lineString );

          final Double id = new Double( m_dataList.size() );
          final Double grenze = new Double( m_grenzen[index] );
          final String name = "" + m_grenzen[index];

          if( gmGeo instanceof GM_Curve )
          {
            final CollectorDataProvider dataProvider = new CollectorDataProvider( gmGeo, new Double[] { grenze }, id, new String[] { name } );
            m_dataList.add( dataProvider );
          }
        }
      }
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see com.bce.gis.operation.raster2vector.SegmentCollector#getFeatures()
   */
  public CollectorDataProvider[] getData( )
  {
    return m_dataList.toArray( new CollectorDataProvider[m_dataList.size()] );
  }
}