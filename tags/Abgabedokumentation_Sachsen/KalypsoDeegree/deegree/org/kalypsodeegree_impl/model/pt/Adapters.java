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
package org.kalypsodeegree_impl.model.pt;

// OpenGIS dependencies
import org.opengis.pt.PT_CoordinatePoint;
import org.opengis.pt.PT_Envelope;
import org.opengis.pt.PT_Matrix;

/**
 * <FONT COLOR="#FF6633">Provide methods for interoperability with
 * <code>org.opengis.pt</code> package. </FONT> All methods accept null
 * argument.
 * 
 * @version 1.0
 * @author Martin Desruisseaux
 */
public class Adapters
{
  /**
   * Default adapters. Will be constructed only when first requested.
   */
  private static Adapters DEFAULT;

  /**
   * Default constructor.
   */
  protected Adapters()
  {}

  /**
   * Returns the default adapters.
   */
  public static synchronized Adapters getDefault()
  {
    if( DEFAULT == null )
    {
      DEFAULT = new Adapters();
    }
    return DEFAULT;
  }

  /**
   * Returns an OpenGIS structure for a coordinate point. Changes to the
   * returned structure will not affect the original point.
   */
  public PT_CoordinatePoint export( final CoordinatePoint point )
  {
    if( point == null )
      return null;
    final PT_CoordinatePoint pt = new PT_CoordinatePoint();
    pt.ord = (double[])point.ord.clone();
    return pt;
  }

  /**
   * Returns an OpenGIS structure for an envelope. Changes to the returned
   * structure will not affect the original envelope.
   */
  public PT_Envelope export( final Envelope envelope )
  {
    if( envelope == null )
      return null;
    final int dimension = envelope.getDimension();
    final PT_Envelope ep = new PT_Envelope();
    ep.minCP = new PT_CoordinatePoint();
    ep.maxCP = new PT_CoordinatePoint();
    ep.minCP.ord = new double[dimension];
    ep.maxCP.ord = new double[dimension];
    for( int i = 0; i < dimension; i++ )
    {
      ep.minCP.ord[i] = envelope.getMinimum( i );
      ep.maxCP.ord[i] = envelope.getMaximum( i );
    }
    return ep;
  }

  /**
   * Returns an OpenGIS structure for a matrix. Changes to the returned
   * structure will not affect the original matrix.
   */
  public PT_Matrix export( final Matrix matrix )
  {
    final PT_Matrix m = new PT_Matrix();
    m.elt = matrix.getElements();
    return m;
  }

  /**
   * Returns a coordinate point from an OpenGIS's structure. Changes to the
   * returned point will not affect the original structure.
   */
  public CoordinatePoint wrap( final PT_CoordinatePoint point )
  {
    return ( point != null ) ? new CoordinatePoint( point.ord ) : null;
  }

  /**
   * Returns an envelope from an OpenGIS's structure. Changes to the returned
   * envelope will not affect the original structure.
   */
  public Envelope wrap( final PT_Envelope envelope )
  {
    return ( envelope != null ) ? new Envelope( envelope.minCP.ord, envelope.maxCP.ord ) : null;
  }

  /**
   * Returns a matrix from an OpenGIS's structure. Changes to the returned
   * matrix will not affect the original structure.
   */
  public Matrix wrap( final PT_Matrix matrix )
  {
    return ( matrix != null ) ? new Matrix( matrix.elt ) : null;
  }
}