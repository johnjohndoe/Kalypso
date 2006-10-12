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
import java.util.Arrays;
import java.util.Map;

import javax.units.Unit;

import org.kalypsodeegree_impl.model.resources.Utilities;
import org.kalypsodeegree_impl.model.resources.css.ResourceKeys;
import org.kalypsodeegree_impl.model.resources.css.Resources;
import org.opengis.cs.CS_GeocentricCoordinateSystem;
import org.opengis.cs.CS_HorizontalDatum;
import org.opengis.cs.CS_LinearUnit;
import org.opengis.cs.CS_PrimeMeridian;

/**
 * A 3D coordinate system, with its origin at the center of the Earth. The <var>X </var> axis points towards the prime
 * meridian. The <var>Y </var> axis points East or West. The <var>Z </var> axis points North or South. By default the
 * <var>Z </var> axis will point North, and the <var>Y </var> axis will point East (e.g. a right handed system), but you
 * should check the axes for non-default values.
 * 
 * @version 1.00
 * @author OpenGIS (www.opengis.org)
 * @author Martin Desruisseaux
 * 
 * @see org.opengis.cs.CS_GeocentricCoordinateSystem
 */
public class GeocentricCoordinateSystem extends CoordinateSystem
{
  /**
   * Serial number for interoperability with different versions.
   */
  private static final long serialVersionUID = -6577810243397267703L;

  /**
   * The set of default axis orientation. The <var>X </var> axis points towards the prime meridian. The <var>Y </var>
   * axis points East. The <var>Z </var> axis points North.
   */
  private static final AxisInfo[] DEFAULT_AXIS = new AxisInfo[]
  {
      new AxisInfo( "x", AxisOrientation.OTHER ),
      new AxisInfo( "y", AxisOrientation.EAST ),
      new AxisInfo( "z", AxisOrientation.NORTH ) };

  /**
   * The default geocentric coordinate system. Prime meridian is Greenwich, horizontal datum in WGS84 and linear units
   * are metre. The <var>X </var> axis points towards the prime meridian. The <var>Y </var> axis points East. The <var>Z
   * </var> axis points North.
   */
  public static final GeocentricCoordinateSystem DEFAULT = (GeocentricCoordinateSystem)pool
      .intern( new GeocentricCoordinateSystem( "WGS84", Unit.METRE, HorizontalDatum.WGS84, PrimeMeridian.GREENWICH,
          DEFAULT_AXIS ) );

  /**
   * The linear unit.
   */
  private final Unit m_unit;

  /**
   * The horizontal datum.
   */
  private final HorizontalDatum m_datum;

  /**
   * The prime meridian.
   */
  private final PrimeMeridian m_meridian;

  /**
   * The axis infos.
   */
  private final AxisInfo[] m_axis;

  /**
   * Construct a geocentric coordinate system with default axis. Unit are metres and prime meridian is greenwich.
   * 
   * @param name
   *          The coordinate system name.
   * @param datum
   *          The horizontal datum.
   */
  public GeocentricCoordinateSystem( final String name, final HorizontalDatum datum )
  {
    this( name, Unit.METRE, datum, PrimeMeridian.GREENWICH );
  }

  /**
   * Construct a geocentric coordinate system with default axis. The <var>X </var> axis points towards the prime
   * meridian. The <var>Y </var> axis points East. The <var>Z </var> axis points North.
   * 
   * @param name
   *          The coordinate system name.
   * @param unit
   *          The linear unit.
   * @param datum
   *          The horizontal datum.
   * @param meridian
   *          The prime meridian.
   */
  public GeocentricCoordinateSystem( final String name, final Unit unit, final HorizontalDatum datum,
      final PrimeMeridian meridian )
  {
    this( name, unit, datum, meridian, DEFAULT_AXIS );
  }

  /**
   * Construct a geocentric coordinate system.
   * 
   * @param name
   *          The coordinate system name.
   * @param unit
   *          The linear unit.
   * @param datum
   *          The horizontal datum.
   * @param meridian
   *          The prime meridian.
   * @param axis
   *          The axis info. This is usually an array of lenght 3.
   */
  public GeocentricCoordinateSystem( final String name, final Unit unit, final HorizontalDatum datum,
      final PrimeMeridian meridian, final AxisInfo[] axis )
  {
    super( name );
    m_unit = unit;
    m_datum = datum;
    m_meridian = meridian;
    ensureNonNull( "axis", axis );
    ensureNonNull( "unit", unit );
    ensureNonNull( "datum", datum );
    ensureNonNull( "meridian", meridian );
    ensureLinearUnit( unit );
    this.m_axis = clone( axis );
  }

  /**
   * Construct a geocentric coordinate system.
   * 
   * @param properties
   *          The set of properties (see {@link Info}).
   * @param unit
   *          The linear unit.
   * @param datum
   *          The horizontal datum.
   * @param meridian
   *          The prime meridian.
   * @param axis
   *          The axis info. This is usually an array of lenght 3.
   */
  GeocentricCoordinateSystem( final Map properties, final Unit unit, final HorizontalDatum datum,
      final PrimeMeridian meridian, final AxisInfo[] axis )
  {
    super( properties );
    m_unit = unit;
    m_datum = datum;
    m_meridian = meridian;
    m_axis = clone( axis );
  }

  /**
   * Clone the specified axis array.
   */
  private static AxisInfo[] clone( final AxisInfo[] axis )
  {
    return Arrays.equals( axis, DEFAULT_AXIS ) ? DEFAULT_AXIS : (AxisInfo[])axis.clone();
  }

  /**
   * Returns the dimension of this coordinate system, which is usually 3.
   * 
   * @see org.opengis.cs.CS_GeocentricCoordinateSystem#getDimension()
   */
  @Override
  public int getDimension()
  {
    return m_axis.length;
  }

  /**
   * Override {@link CoordinateSystem#getDatum()}.
   */
  @Override
  final Datum getDatum()
  {
    return getHorizontalDatum();
  }

  /**
   * Returns the horizontal datum. The horizontal datum is used to determine where the center of the Earth is considered
   * to be. All coordinate points will be measured from the center of the Earth, and not the surface.
   * 
   * @see org.opengis.cs.CS_GeocentricCoordinateSystem#getHorizontalDatum()
   */
  public HorizontalDatum getHorizontalDatum()
  {
    return m_datum;
  }

  /**
   * Gets units for dimension within coordinate system. For a <code>GeocentricCoordinateSystem</code>, the units is
   * the same for all axis.
   * 
   * @param dimension
   *          Zero based index of axis.
   * 
   * @see org.opengis.cs.CS_GeocentricCoordinateSystem#getUnits(int)
   */
  @Override
  public Unit getUnits( final int dimension )
  {
    if( dimension >= 0 && dimension < getDimension() )
      return m_unit;
    throw new IndexOutOfBoundsException( Resources.format( ResourceKeys.ERROR_INDEX_OUT_OF_BOUNDS_$1, new Integer(
        dimension ) ) );
  }

  /**
   * Gets axis details for dimension within coordinate system. Each dimension in the coordinate system has a
   * corresponding axis.
   * 
   * @param dimension
   *          Zero based index of axis.
   * 
   * @see org.opengis.cs.CS_CoordinateSystem#getAxis(int)
   */
  @Override
  public AxisInfo getAxis( final int dimension )
  {
    return m_axis[dimension];
  }

  /**
   * Returns the prime meridian.
   * 
   * @see org.opengis.cs.CS_GeocentricCoordinateSystem#getPrimeMeridian()
   */
  public PrimeMeridian getPrimeMeridian()
  {
    return m_meridian;
  }

  /**
   * Returns <code>true</code> if this coordinate system is equivalents to the specified coordinate system. Two
   * coordinate systems are considered equivalent if the
   * {@link org.kalypsodeegree_impl.model.ct.CoordinateTransformation}from <code>this</code> to <code>cs</code>
   * would be the identity transform. The default implementation compare datum, units and axis, but ignore name, alias
   * and other meta-data informations.
   * 
   * @param cs
   *          The coordinate system (may be <code>null</code>).
   * @return <code>true</code> if both coordinate systems are equivalent.
   */
  @Override
  public boolean equivalents( final CoordinateSystem cs )
  {
    if( cs == this )
      return true;
    if( super.equivalents( cs ) )
    {
      final GeocentricCoordinateSystem that = (GeocentricCoordinateSystem)cs;
      return Utilities.equals( this.m_unit, that.m_unit ) && Utilities.equals( this.m_datum, that.m_datum )
          && Utilities.equals( this.m_meridian, that.m_meridian );
    }
    return false;
  }

  @Override /**
   * Fill the part inside "[...]". Used for formatting Well Know Text (WKT).
   */
  String addString( final StringBuffer buffer )
  {
    buffer.append( ", " );
    buffer.append( m_datum );
    buffer.append( ", " );
    buffer.append( m_meridian );
    buffer.append( ", " );
    addUnit( buffer, m_unit );
    for( int i = 0; i < m_axis.length; i++ )
    {
      buffer.append( ", " );
      buffer.append( m_axis[i] );
    }
    return "GEOCCS";
  }

  /**
   * Returns an OpenGIS interface for this local coordinate system. The returned object is suitable for RMI use.
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
   * Wrap a {@link LocalCoordinateSystem}object for use with OpenGIS. This class is suitable for RMI use.
   * 
   * @version 1.0
   * @author Martin Desruisseaux
   */
  private final class Export extends CoordinateSystem.Export implements CS_GeocentricCoordinateSystem
  {
    /**
     * Construct a remote object.
     */
    protected Export( final Object adapters )
    {
      super( adapters );
    }

    /**
     * Gets the local datum.
     */
    public CS_HorizontalDatum getHorizontalDatum()
    {
      return m_adapters.export( GeocentricCoordinateSystem.this.getHorizontalDatum() );
    }

    /**
     * Gets the units used along all the axes.
     */
    public CS_LinearUnit getLinearUnit()
    {
      return (CS_LinearUnit)m_adapters.export( GeocentricCoordinateSystem.this.getUnits() );
    }

    /**
     * Returns the PrimeMeridian.
     */
    public CS_PrimeMeridian getPrimeMeridian()
    {
      return m_adapters.export( GeocentricCoordinateSystem.this.getPrimeMeridian() );
    }
  }
}