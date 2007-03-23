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
package org.kalypsodeegree_impl.model.cs;

// OpenGIS dependencies
import java.util.Map;

import org.kalypsodeegree_impl.model.resources.Utilities;
import org.opengis.cs.CS_Ellipsoid;
import org.opengis.cs.CS_HorizontalDatum;
import org.opengis.cs.CS_WGS84ConversionInfo;

/**
 * Procedure used to measure positions on the surface of the Earth.
 * 
 * @version 1.00
 * @author OpenGIS (www.opengis.org)
 * @author Martin Desruisseaux
 * 
 * @see org.opengis.cs.CS_HorizontalDatum
 */
public class HorizontalDatum extends Datum
{
  /**
   * Serial number for interoperability with different versions.
   */
  private static final long serialVersionUID = -1424482162002300865L;

  /**
   * The default WGS 1984 datum.
   */
  public static final HorizontalDatum WGS84 = (HorizontalDatum)pool.intern( new HorizontalDatum( "WGS84",
      DatumType.GEOCENTRIC, Ellipsoid.WGS84, null ) );

  /**
   * The ellipsoid for this datum.
   */
  private final Ellipsoid m_ellipsoid;

  /**
   * Preferred parameters for a Bursa Wolf transformation.
   */
  private final WGS84ConversionInfo m_parameters;

  /**
   * Creates horizontal datum from an ellipsoid. The datum type will be {@link DatumType.Horizontal#OTHER}.
   * 
   * @param name
   *          Name to give new object.
   * @param ellipsoid
   *          Ellipsoid to use in new horizontal datum.
   */
  public HorizontalDatum( final String name, final Ellipsoid ellipsoid )
  {
    this( name, DatumType.Horizontal.OTHER, ellipsoid, null );
  }

  /**
   * Creates horizontal datum from ellipsoid and Bursa-Wolf parameters.
   * 
   * @param name
   *          Name to give new object.
   * @param type
   *          Type of horizontal datum to create.
   * @param ellipsoid
   *          Ellipsoid to use in new horizontal datum.
   * @param parameters
   *          Suggested approximate conversion from new datum to WGS84, or <code>null</code> if there is none.
   *  
   */
  public HorizontalDatum( final String name, final DatumType.Horizontal type, final Ellipsoid ellipsoid,
      final WGS84ConversionInfo parameters )
  {
    super( name, type );
    this.m_ellipsoid = ellipsoid;
    this.m_parameters = ( parameters != null ) ? (WGS84ConversionInfo)parameters.clone() : null;
    ensureNonNull( "ellipsoid", ellipsoid );
  }

  /**
   * Creates horizontal datum from ellipsoid and Bursa-Wolf parameters.
   * 
   * @param properties
   *          The set of properties (see {@link Info}).
   * @param type
   *          Type of horizontal datum to create.
   * @param ellipsoid
   *          Ellipsoid to use in new horizontal datum.
   * @param parameters
   *          Suggested approximate conversion from new datum to WGS84, or <code>null</code> if there is none.
   */
  HorizontalDatum( final Map properties, final DatumType type, final Ellipsoid ellipsoid,
      final WGS84ConversionInfo parameters )
  {
    super( properties, type );
    this.m_ellipsoid = ellipsoid;
    this.m_parameters = parameters;
    // Accept null values.
  }

  /**
   * Gets the type of the datum as an enumerated code.
   * 
   * Note: return type will be changed to {@link DatumType.Horizontal}when we will be able to use generic types (with
   * JDK 1.5).
   * 
   * @see org.opengis.cs.CS_HorizontalDatum#getDatumType()
   */
  @Override
  public DatumType/* .Horizontal */getDatumType()
  {
    return (DatumType.Horizontal)super.getDatumType();
  }

  /**
   * Returns the ellipsoid.
   * 
   * @see org.opengis.cs.CS_HorizontalDatum#getEllipsoid()
   */
  public Ellipsoid getEllipsoid()
  {
    return m_ellipsoid;
  }

  /**
   * Gets preferred parameters for a Bursa Wolf transformation into WGS84. The 7 returned values correspond to
   * (dx,dy,dz) in meters, (ex,ey,ez) in arc-seconds, and scaling in parts-per-million. This method will always returns
   * <code>null</code> for horizontal datums with type {@link DatumType.Horizontal#OTHER}. This method may also
   * returns <code>null</code> if no suitable transformation is available.
   * 
   * @see org.opengis.cs.CS_HorizontalDatum#getWGS84Parameters()
   */
  public WGS84ConversionInfo getWGS84Parameters()
  {
    return ( m_parameters != null ) ? (WGS84ConversionInfo)m_parameters.clone() : null;
  }

  @Override /**
   * Fill the part inside "[...]". Used for formatting Well Know Text (WKT).
   */
  String addString( final StringBuffer buffer )
  {
    super.addString( buffer );
    buffer.append( ", " );
    buffer.append( m_ellipsoid );
    if( m_parameters != null )
    {
      buffer.append( ", " );
      buffer.append( m_parameters );
    }
    return "DATUM";
  }

  /**
   * Compares the specified object with this datum for equality.
   */
  @Override
  public boolean equals( final Object object )
  {
    if( super.equals( object ) )
    {
      final HorizontalDatum that = (HorizontalDatum)object;
      return Utilities.equals( this.m_ellipsoid, that.m_ellipsoid ) && Utilities.equals( this.m_parameters, that.m_parameters );
    }
    return false;
  }

  /**
   * Returns an OpenGIS interface for this datum. The returned object is suitable for RMI use.
   * 
   * Note: The returned type is a generic {@link Object}in order to avoid too early class loading of OpenGIS interface.
   */
  @Override
  final Object toOpenGIS( final Object adapters )
  {
    return new Export( adapters );
  }

  /////////////////////////////////////////////////////////////////////////
  //////////////// ////////////////
  //////////////// OPENGIS ADAPTER ////////////////
  //////////////// ////////////////
  /////////////////////////////////////////////////////////////////////////

  /**
   * Wrap a {@link HorizontalDatum}object for use with OpenGIS. This class is suitable for RMI use.
   * 
   * @version 1.0
   * @author Martin Desruisseaux
   */
  private final class Export extends Datum.Export implements CS_HorizontalDatum
  {
    /**
     * Construct a remote object.
     */
    protected Export( final Object adapters )
    {
      super( adapters );
    }

    /**
     * Returns the Ellipsoid.
     */
    public CS_Ellipsoid getEllipsoid()
    {
      return m_adapters.export( HorizontalDatum.this.getEllipsoid() );
    }

    /**
     * Gets preferred parameters for a Bursa Wolf transformation into WGS84.
     */
    public CS_WGS84ConversionInfo getWGS84Parameters()
    {
      return m_adapters.export( HorizontalDatum.this.getWGS84Parameters() );
    }
  }
}