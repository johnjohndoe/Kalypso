package org.kalypso.grid.processes.raster2vector.collector;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.grid.processes.raster2vector.LinkedCoordinate;
import org.kalypso.grid.processes.raster2vector.LinkedCoordinateException;
import org.kalypso.grid.processes.raster2vector.SegmentCollector;
import org.kalypso.grid.processes.raster2vector.collector.ringtree.RingTree;
import org.kalypso.grid.processes.raster2vector.collector.ringtree.RingTreeElement;
import org.kalypso.grid.processes.raster2vector.collector.ringtree.RingTreeWalker;
import org.kalypso.jts.CoordOrientation;
import org.kalypso.jts.CoordOrientationException;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.algorithm.PointInRing;
import com.vividsolutions.jts.algorithm.SIRtreePointInRing;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Sammelt LineStrings und kombiniert Sie als Polygone
 * 
 * @author belger
 */
public class PolygonCollector implements SegmentCollector, RingTreeWalker
{
  private final List<CollectorDataProvider> m_dataList = new ArrayList<CollectorDataProvider>();

  private final GeometryFactory m_gf;

  private final Interval[] m_intervals;

  private final double[] m_grenzen;

  private final boolean m_bSimple;

  private final RingTree m_tree = new RingTree();

  public PolygonCollector( final GeometryFactory gf, final double[] grenzen, final boolean bSimple )
  {
    m_gf = gf;
    m_bSimple = bSimple;
    m_grenzen = grenzen;

    m_intervals = new Interval[grenzen.length + 1];
    m_intervals[0] = new Interval( Double.MIN_VALUE, grenzen[0] );
    for( int i = 0; i < grenzen.length - 1; i++ )
      m_intervals[i + 1] = new Interval( grenzen[i], grenzen[i + 1] );
    m_intervals[grenzen.length] = new Interval( grenzen[grenzen.length - 1], Double.MAX_VALUE );

  }

  /**
   * @see com.bce.gis.operation.raster2vector.SegmentCollector#addSegment(int,
   *      com.bce.gis.operation.raster2vector.LinkedCoordinate, com.bce.gis.operation.raster2vector.LinkedCoordinate,
   *      com.vividsolutions.jts.geom.Coordinate, com.vividsolutions.jts.geom.Coordinate)
   */
  public void addSegment( final int index, final LinkedCoordinate lc0, final LinkedCoordinate lc1, final Coordinate nearC0, final Coordinate nearC1 ) throws LinkedCoordinateException
  {
    lc0.link( lc1 );

    if( !lc0.isCircle() )
      return;

    try
    {
      final Coordinate[] crds = lc0.getAsRing();

      if( m_bSimple )
      {
        final LinearRing lr = m_gf.createLinearRing( crds );
        final Polygon poly = m_gf.createPolygon( lr, new LinearRing[] {} );
        appendFeature( index, poly );
      }
      else
      {
        final PointInRing pir = new SIRtreePointInRing( m_gf.createLinearRing( crds ) );
        Coordinate innerCrd = null;
        if( pir.isInside( nearC0 ) )
          innerCrd = nearC0;
        else if( pir.isInside( nearC1 ) )
          innerCrd = nearC1;
        else
          System.out.println( "Kann nicht sein" );

        CoordOrientation.orient( crds, CoordOrientation.TYPE.NEGATIV );

        final LinearRing lr = m_gf.createLinearRing( crds );

        m_tree.insertElement( new RingTreeElement( lr, index, innerCrd ) );
      }
    }
    catch( final CoordOrientationException coe )
    {
      coe.printStackTrace();
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @param index
   * @param poly
   * @throws GM_Exception
   */
  private void appendFeature( final int index, final Polygon poly ) throws GM_Exception
  {
    final GM_Object gmGeo = JTSAdapter.wrap( poly );

    final Double id = new Double( m_dataList.size() );
    final String name = m_intervals[index].toString();

    final Double von = new Double( Math.max( -9999.99, m_intervals[index].getMin() ) );
    final Double bis = new Double( Math.min( 9999.99, m_intervals[index].getMax() ) );
    final Double volumen = Double.NaN;

    if( gmGeo instanceof GM_Surface< ? > )
    {
      CollectorDataProvider dataProvider = new CollectorDataProvider( gmGeo, new Double[] { von, bis, volumen }, id, new String[] { name } );
      m_dataList.add( dataProvider );
    }
  }

  /**
   * @see com.bce.gis.operation.raster2vector.collector.ringtree.RingTreeWalker#operate(com.bce.gis.operation.raster2vector.collector.ringtree.RingTreeElement)
   */
  public void operate( final RingTreeElement element )
  {
    final Polygon p = element.getAsPolygon( m_gf );

    if( p != null )
    {
      try
      {
        final int intIndex = getIndexForElement( element );

        if( intIndex > 0 )
          appendFeature( intIndex, p );
      }
      catch( final GM_Exception gme )
      {
        gme.printStackTrace();
      }
    }
  }

  private int getIndexForElement( final RingTreeElement element )
  {
    final int index = element.index;
    if( element.hasChildren() )
    {
      final RingTreeElement child = element.getFirstChild();

      if( child.index == index - 1 || child.index == index + 1 )
      {
        final double grenze0 = m_grenzen[index];
        final double grenze1 = m_grenzen[child.index];

        for( int i = 0; i < m_intervals.length; i++ )
        {
          if( (m_intervals[i].getMin() == grenze0 && m_intervals[i].getMax() == grenze1) || (m_intervals[i].getMin() == grenze1 && m_intervals[i].getMax() == grenze0) )
            return i;
        }
      }
      else
      {
        final int childIndex = getIndexForElement( child );
        if( childIndex == -1 )
          return index + 1;

        if( m_grenzen[index] == m_intervals[childIndex].getMax() )
          return index + 1;

        return index;
      }
    }
    else
    {
      // anhand der inneren Coordinate rausfinden, zu welcher Klasse es gehört
      final double value = element.innerCrd.z;
      if( Double.isNaN( value ) )
        return -1;

      for( int i = 0; i < m_intervals.length; i++ )
      {
        if( m_intervals[i].contains( value ) )
          return i;
      }
    }

    return -1;
  }

  private static class Interval
  {
    private final double m_min;

    private final double m_max;

    public double getMin( )
    {
      return m_min;
    }

    public double getMax( )
    {
      return m_max;
    }

    public Interval( final double min, final double max )
    {
      m_min = min;
      m_max = max;
    }

    public boolean contains( final double value )
    {
      if( m_min == Double.MIN_VALUE )
        return value < m_max;
      else if( m_max == Double.MAX_VALUE )
        return m_min <= value;
      else
        return m_min <= value && value < m_max;
    }

    @Override
    public String toString( )
    {
      final StringBuffer str = new StringBuffer();
      if( m_min == Double.MIN_VALUE )
        str.append( "-Inf" );
      else
        str.append( m_min );

      str.append( " - " );

      if( m_max == Double.MAX_VALUE )
        str.append( "Inf" );
      else
        str.append( m_max );

      return str.toString();
    }
  }

  /**
   * @see org.kalypso.grid.processes.raster2vector.SegmentCollector#getData()
   */
  public CollectorDataProvider[] getData( )
  {
    if( !m_bSimple )
      m_tree.walk( this );

    return m_dataList.toArray( new CollectorDataProvider[m_dataList.size()] );
  }

  /**
   * @see org.kalypso.grid.processes.raster2vector.collector.ringtree.RingTreeWalker#getResult()
   */
  public Object getResult( )
  {
    // TODO Auto-generated method stub
    return null;
  }
}
