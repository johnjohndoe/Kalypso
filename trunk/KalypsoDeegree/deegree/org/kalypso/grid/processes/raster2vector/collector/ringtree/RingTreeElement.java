package org.kalypso.grid.processes.raster2vector.collector.ringtree;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import com.vividsolutions.jts.algorithm.PointInRing;
import com.vividsolutions.jts.algorithm.SIRtreePointInRing;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateArrays;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;

/**
 * @author belger
 */
public class RingTreeElement
{
  public final int index;

  public final LinearRing linearRing;

  public final PointInRing pir;

  public final Coordinate innerCrd;

  private final Collection<RingTreeElement> m_children = new ArrayList<RingTreeElement>();

  private final Collection<RingTreeElement> m_unmodChildren = Collections.unmodifiableCollection( m_children );

  public RingTreeElement( final LinearRing lr, final int index, final Coordinate innerCrd )
  {
    this.linearRing = lr;
    this.index = index;
    this.innerCrd = innerCrd;

    this.pir = (lr == null) ? null : new SIRtreePointInRing( lr );
  }

  public void addChild( final RingTreeElement rte )
  {
    m_children.add( rte );
  }

  public void removeChild( final RingTreeElement rte )
  {
    m_children.remove( rte );
  }

  public Collection<RingTreeElement> children( )
  {
    return m_unmodChildren;
  }

  @Override
  public String toString( )
  {
    return "[" + index + ", " + linearRing + "]";
  }

  public Polygon getAsPolygon( final GeometryFactory gf )
  {
    // die Löcher finden
    final ArrayList<LinearRing> holes = new ArrayList<LinearRing>();
    for( final RingTreeElement child : m_children )
    {
      final Coordinate[] holeCrd = child.linearRing.getCoordinates();
      final Coordinate[] newHoleCrds = new Coordinate[holeCrd.length];
      System.arraycopy( holeCrd, 0, newHoleCrds, 0, holeCrd.length );
      CoordinateArrays.reverse( newHoleCrds );

      holes.add( gf.createLinearRing( newHoleCrds ) );
    }

    return gf.createPolygon( linearRing, holes.toArray( new LinearRing[holes.size()] ) );
  }

  public boolean hasChildren( )
  {
    return !m_children.isEmpty();
  }

  public RingTreeElement getFirstChild( )
  {
    return m_children.iterator().next();
  }

  public boolean contains( final RingTreeElement element )
  {
    for( int i = 0; i < element.linearRing.getNumPoints(); i++ )
    {
      if( !pir.isInside( element.linearRing.getCoordinateN( i ) ) )
        return false;
    }

    return true;
  }

}
