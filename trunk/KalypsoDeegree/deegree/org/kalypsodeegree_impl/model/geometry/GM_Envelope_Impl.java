/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 
 history:
 
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
 
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.model.geometry;

import java.awt.geom.Rectangle2D;
import java.io.Serializable;

import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;

/**
 * a boundingbox as child of a GM_Polygon isn't part of the iso19107 spec but it
 * simplifies the geometry handling within jago
 * 
 * <P>
 * ------------------------------------------------------------
 * </P>
 * 
 * @author Andreas Poth href="mailto:poth@lat-lon.de"
 * @author Markus Bedel href="mailto:bedel@giub.uni-bonn.de"
 * @version $Id$
 *  
 */
public class GM_Envelope_Impl implements GM_Envelope, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = 1081219767894344990L;

  private GM_Position m_max = null;

  private GM_Position m_min = null;

  /**
   * Creates a new GM_Envelope_Impl object.
   */
  public GM_Envelope_Impl()
  {
    this.m_min = new GM_Position_Impl();
    this.m_max = new GM_Position_Impl();
  }

  /**
   * Creates a new GM_Envelope_Impl object.
   * 
   * @param min
   * @param max
   */
  public GM_Envelope_Impl( GM_Position min, GM_Position max )
  {
    this.m_min = GeometryFactory.createGM_Position( min.getX() < max.getX() ? min.getX() : max.getX(), min.getY() < max.getY() ? min.getY() : max
        .getY() );
    this.m_max = GeometryFactory.createGM_Position( min.getX() > max.getX() ? min.getX() : max.getX(), min.getY() > max.getY() ? min.getY() : max
        .getY() );
  }

  /**
   * 
   * @see java.lang.Object#clone()
   */
  @Override
  public Object clone()
  {
    return new GM_Envelope_Impl( (GM_Position)( (GM_Position_Impl)m_min ).clone(), (GM_Position)( (GM_Position_Impl)m_max ).clone() );
  }

  /**
   * returns the minimum coordinates of bounding box
   */
  public GM_Position getMin()
  {
    return m_min;
  }

  /**
   * returns the maximum coordinates of bounding box
   */
  public GM_Position getMax()
  {
    return m_max;
  }

  /**
   * returns the width of bounding box
   */
  public double getWidth()
  {
    return this.getMax().getX() - this.getMin().getX();
  }

  /**
   * returns the height of bounding box
   */
  public double getHeight()
  {
    return this.getMax().getY() - this.getMin().getY();
  }

  /**
   * returns true if the bounding box conatins the specified GM_Point
   */
  public boolean contains( GM_Position point )
  {
    if( ( point.getX() >= m_min.getX() ) && ( point.getX() <= m_max.getX() ) && ( point.getY() >= m_min.getY() ) && ( point.getY() <= m_max.getY() ) )
    {
      return true;
    }

    return false;
  }

  /**
   * returns true if this envelope and the submitted intersects
   */
  public boolean intersects( final GM_Envelope bb )
  {
    // TODO: true or false?
    if( bb == null )
      return true;
    
    // coordinates of this GM_Envelope's BBOX
    double minx1 = m_min.getX();
    double miny1 = m_min.getY();
    double maxx1 = m_max.getX();
    double maxy1 = m_max.getY();

    // coordinates of the other GM_Envelope's BBOX
    double minx2 = bb.getMin().getX();
    double miny2 = bb.getMin().getY();
    double maxx2 = bb.getMax().getX();
    double maxy2 = bb.getMax().getY();

    if( !( Math.max( minx1, minx2 ) <= Math.min( maxx1, maxx2 ) ) )
      return false;
    if( !( Math.max( miny1, miny2 ) <= Math.min( maxy1, maxy2 ) ) )
      return false;
    return true;
    //    // special cases: box2 lays completly inside box1
    //    if( ( west1 <= west2 ) && ( south1 <= south2 ) && ( east1 >= east2 ) && (
    // north1 >= north2 ) )
    //    {
    //      return true;
    //    }
    //
    //    if( ( west1 >= west2 ) && ( south1 >= south2 ) && ( east1 <= east2 ) && (
    // north1 <= north2 ) )
    //    {
    //      return true;
    //    }
    //
    //    // in any other case of intersection, at least one line of the BBOX has
    //    // to cross a line of the other BBOX
    //    // check western boundary of box 1
    //    // "touching" boxes must not intersect
    //    if( ( west1 >= west2 ) && ( west1 < east2 ) )
    //    {
    //      if( ( south1 <= south2 ) && ( north1 > south2 ) )
    //      {
    //        return true;
    //      }
    //
    //      if( ( south1 < north2 ) && ( north1 >= north2 ) )
    //      {
    //        return true;
    //      }
    //    }
    //
    //    // check eastern boundary of box 1
    //    // "touching" boxes must not intersect
    //    if( ( east1 > west2 ) && ( east1 <= east2 ) )
    //    {
    //      if( ( south1 <= south2 ) && ( north1 > south2 ) )
    //      {
    //        return true;
    //      }
    //
    //      if( ( south1 < north2 ) && ( north1 >= north2 ) )
    //      {
    //        return true;
    //      }
    //    }
    //
    //    // check southern boundary of box 1
    //    // "touching" boxes must not intersect
    //    if( ( south1 >= south2 ) && ( south1 < north2 ) )
    //    {
    //      if( ( west1 <= west2 ) && ( east1 > west2 ) )
    //      {
    //        return true;
    //      }
    //
    //      if( ( west1 < east2 ) && ( east1 >= east2 ) )
    //      {
    //        return true;
    //      }
    //    }
    //
    //    // check northern boundary of box 1
    //    // "touching" boxes must not intersect
    //    if( ( north1 > south2 ) && ( north1 <= north2 ) )
    //    {
    //      if( ( west1 <= west2 ) && ( east1 > west2 ) )
    //      {
    //        return true;
    //      }
    //
    //      if( ( west1 < east2 ) && ( east1 >= east2 ) )
    //      {
    //        return true;
    //      }
    //    }
    //
    //    return false;
  }

  /**
   * returns true if all points of the submitted bounding box are within this
   * bounding box
   */
  public boolean contains( GM_Envelope bb )
  {
    if(bb==null)
      return false;
    GM_Position p1 = new GM_Position_Impl( bb.getMin().getX(), bb.getMin().getY() );
    GM_Position p2 = new GM_Position_Impl( bb.getMin().getX(), bb.getMax().getY() );
    GM_Position p3 = new GM_Position_Impl( bb.getMax().getX(), bb.getMin().getY() );
    GM_Position p4 = new GM_Position_Impl( bb.getMax().getX(), bb.getMax().getY() );

    boolean ins = ( contains( p1 ) && contains( p2 ) && contains( p3 ) && contains( p4 ) );
    return ins;
  }

  /**
   * returns a new GM_Envelope object representing the intersection of this
   * GM_Envelope with the specified GM_Envelope. * Note: If there is no
   * intersection at all GM_Envelope will be null.
   * 
   * @param bb the GM_Envelope to be intersected with this GM_Envelope
   * @return the largest GM_Envelope contained in both the specified GM_Envelope
   *         and in this GM_Envelope.
   */
  public GM_Envelope createIntersection( GM_Envelope bb )
  {
    Rectangle2D rect = new Rectangle2D.Double( bb.getMin().getX(), bb.getMin().getY(), bb.getWidth(), bb.getHeight() );
    Rectangle2D rect2 = new Rectangle2D.Double( this.getMin().getX(), this.getMin().getY(), this.getWidth(), this.getHeight() );

    if( rect2.intersects( bb.getMin().getX(), bb.getMin().getY(), bb.getWidth(), bb.getHeight() ) )
    {
      rect = rect.createIntersection( rect2 );
    }
    else
    {
      rect = null;
    }

    if( rect == null )
    {
      return null;
    }

    double xmin = rect.getX();
    double ymin = rect.getY();
    double xmax = rect.getX() + rect.getWidth();
    double ymax = rect.getY() + rect.getHeight();

    GM_Position p1 = new GM_Position_Impl( xmin, ymin );
    GM_Position p2 = new GM_Position_Impl( xmax, ymax );

    return new GM_Envelope_Impl( p1, p2 );
  }

  /**
   * checks if this point is completly equal to the submitted geometry
   */
  @Override
  public boolean equals( Object other )
  {
    if( ( other == null ) || !( other instanceof GM_Envelope_Impl ) )
    {
      return false;
    }

    return ( m_min.equals( ( (GM_Envelope)other ).getMin() ) && m_max.equals( ( (GM_Envelope)other ).getMax() ) );
  }

  public GM_Envelope getBuffer( double b )
  {
    GM_Position bmin = new GM_Position_Impl( new double[]
    {
        m_min.getX() - b,
        m_min.getY() - b } );
    GM_Position bmax = new GM_Position_Impl( new double[]
    {
        m_max.getX() + b,
        m_max.getY() + b } );
    return GeometryFactory.createGM_Envelope( bmin, bmax );
  }

  public GM_Envelope getMerged( GM_Position pos )
  {
    double minx = m_min.getX();
    double miny = m_min.getY();
    double maxx = m_max.getX();
    double maxy = m_max.getY();
    if( pos != null )
    {
      if( pos.getX() < minx )
        minx = pos.getX();
      if( pos.getY() < miny )
        miny = pos.getY();
      if( pos.getX() > maxx )
        maxx = pos.getX();
      if( pos.getY() > maxy )
        maxy = pos.getY();
    }
    return GeometryFactory.createGM_Envelope( minx, miny, maxx, maxy );

  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Envelope#getMerged(org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public GM_Envelope getMerged( GM_Envelope envelope )
  {
    double minx = m_min.getX();
    double miny = m_min.getY();
    double maxx = m_max.getX();
    double maxy = m_max.getY();
    if( envelope != null )
    {
      if( envelope.getMin().getX() < minx )
        minx = envelope.getMin().getX();
      if( envelope.getMin().getY() < miny )
        miny = envelope.getMin().getY();
      if( envelope.getMax().getX() > maxx )
        maxx = envelope.getMax().getX();
      if( envelope.getMax().getY() > maxy )
        maxy = envelope.getMax().getY();
    }
    return GeometryFactory.createGM_Envelope( minx, miny, maxx, maxy );
  }

  @Override
  public String toString()
  {
    String ret = null;
    ret = "min = " + m_min;
    ret += ( " max = " + m_max + "\n" );
    return ret;
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Envelope#getPaned(org.kalypsodeegree.model.geometry.GM_Point)
   */
  public GM_Envelope getPaned( GM_Point center )
  {
    double dx = getMax().getX() - getMin().getX();
    double dy = getMax().getY() - getMin().getY();
    double minx = center.getX() - dx / 2d;
    double maxx = center.getX() + dx / 2d;
    double miny = center.getY() - dy / 2d;
    double maxy = center.getY() + dy / 2d;
    return GeometryFactory.createGM_Envelope( minx, miny, maxx, maxy );
  }
}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.16  2006/05/28 15:47:16  devgernot
 * - GML-Version is now determined automatically! Use annotations, default is 2.1;
 * - some yellow thingies
 * - repaired some tests (KalypsoCommon, Core is clean, some Test in KalypsoTest are still not running due to GMLSchemaParser/Writer Problems)
 *
 * Revision 1.15  2005/09/29 12:35:21  doemming
 * *** empty log message ***
 *
 * Revision 1.14  2005/09/18 16:22:58  belger
 * *** empty log message ***
 *
 * Revision 1.13  2005/07/21 02:56:47  doemming
 * *** empty log message ***
 *
 * Revision 1.12  2005/06/19 15:10:01  doemming
 * *** empty log message ***
 * Revision 1.11 2005/04/17 21:19:24 doemming
 * *** empty log message ***
 * 
 * Revision 1.10 2005/03/08 11:01:04 doemming *** empty log message ***
 * 
 * Revision 1.9 2005/03/02 18:17:17 doemming *** empty log message ***
 * 
 * Revision 1.8 2005/02/20 18:56:50 doemming *** empty log message *** Revision
 * 1.7 2005/02/15 17:13:49 doemming *** empty log message ***
 * 
 * Revision 1.6 2005/01/18 12:50:41 doemming *** empty log message ***
 * 
 * Revision 1.5 2004/10/07 14:09:10 doemming *** empty log message ***
 * 
 * Revision 1.1 2004/09/02 23:56:51 doemming *** empty log message *** Revision
 * 1.3 2004/08/31 13:54:32 doemming *** empty log message *** Revision 1.13
 * 2004/03/02 07:38:14 poth no message
 * 
 * Revision 1.12 2004/02/23 07:47:50 poth no message
 * 
 * Revision 1.11 2004/01/27 07:55:44 poth no message
 * 
 * Revision 1.10 2004/01/08 09:50:22 poth no message
 * 
 * Revision 1.9 2003/09/14 14:05:08 poth no message
 * 
 * Revision 1.8 2003/07/10 15:24:23 mrsnyder Started to implement
 * LabelDisplayElements that are bound to a Polygon. Fixed error in
 * GM_MultiSurface_Impl.calculateCentroidArea().
 * 
 * Revision 1.7 2003/07/03 12:32:26 poth no message
 * 
 * Revision 1.6 2003/03/20 12:10:29 mrsnyder Rewrote intersects() method.
 * 
 * Revision 1.5 2003/03/19 15:30:04 axel_schaefer Intersects: crossing
 * envelopes, but points are not in envelope
 * 
 *  
 */
