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

// Time
import java.util.Date;
import java.util.Map;

import javax.units.Unit;

import org.kalypsodeegree_impl.model.resources.Utilities;
import org.kalypsodeegree_impl.model.resources.css.ResourceKeys;
import org.kalypsodeegree_impl.model.resources.css.Resources;

/**
 * A one-dimensional coordinate system suitable for time measurements.
 * 
 * @version 1.0
 * @author Martin Desruisseaux
 */
public class TemporalCoordinateSystem extends CoordinateSystem
{
  /**
   * Serial number for interoperability with different versions.
   */
  private static final long serialVersionUID = 4436983518157910233L;

  /**
   * The temporal datum.
   */
  private final TemporalDatum m_datum;

  /**
   * Axis details for time dimension within coordinate system.
   */
  private final AxisInfo m_axis;

  /**
   * Units used along the time axis.
   */
  private final Unit m_unit;

  /**
   * The epoch, in milliseconds since January 1, 1970, 00:00:00 UTC.
   */
  private final long m_epoch;

  /**
   * Creates a temporal coordinate system. Datum is UTC, units are days and values are increasing toward future.
   * 
   * @param name
   *          Name to give new object.
   * @param epoch
   *          The epoch (i.e. date of origin).
   */
  public TemporalCoordinateSystem( final String name, final Date epoch )
  {
    this( name, TemporalDatum.UTC, Unit.DAY, epoch, AxisInfo.TIME );
  }

  /**
   * Creates a temporal coordinate system from a datum and time units.
   * 
   * @param name
   *          Name to give new object.
   * @param datum
   *          Datum to use for new coordinate system.
   * @param unit
   *          Units to use for new coordinate system.
   * @param epoch
   *          The epoch (i.e. date of origin).
   * @param axis
   *          Axis to use for new coordinate system.
   */
  public TemporalCoordinateSystem( final String name, final TemporalDatum datum, final Unit unit, final Date epoch,
      final AxisInfo axis )
  {
    super( name );
    ensureNonNull( "datum", datum );
    ensureNonNull( "unit", unit );
    ensureNonNull( "epoch", epoch );
    ensureNonNull( "axis", axis );
    m_datum = datum;
    m_unit = unit;
    m_epoch = epoch.getTime();
    m_axis = axis;
    ensureTimeUnit( unit );
    checkAxis( datum.getDatumType() );
  }

  /**
   * Creates a temporal coordinate system from a datum and time units.
   * 
   * @param properties
   *          The set of properties (see {@link Info}).
   * @param datum
   *          Datum to use for new coordinate system.
   * @param unit
   *          Units to use for new coordinate system.
   * @param epoch
   *          The epoch (i.e. date of origin).
   * @param axis
   *          Axis to use for new coordinate system.
   */
  TemporalCoordinateSystem( final Map properties, final TemporalDatum datum, final Unit unit, final Date epoch,
      final AxisInfo axis )
  {
    super( properties );
    this.m_datum = datum;
    this.m_unit = unit;
    this.m_epoch = epoch.getTime();
    this.m_axis = axis;
    // Accept null values.
  }

  /**
   * Returns the dimension of this coordinate system, which is 1.
   */
  @Override
  public final int getDimension()
  {
    return 1;
  }

  /**
   * Override {@link CoordinateSystem#getDatum()}.
   */
  @Override
  final Datum getDatum()
  {
    return getTemporalDatum();
  }

  /**
   * Gets the temporal datum, which indicates the measurement method.
   */
  public TemporalDatum getTemporalDatum()
  {
    return m_datum;
  }

  /**
   * Returns the epoch. The epoch is the origin of the time axis, i.e. the date for value zero.
   */
  public Date getEpoch()
  {
    return new Date( m_epoch );
  }

  /**
   * Gets axis details for temporal dimension within coordinate system. A temporal coordinate system have only one axis,
   * always at index 0.
   * 
   * @param dimension
   *          Zero based index of axis.
   */
  @Override
  public AxisInfo getAxis( final int dimension )
  {
    final int maxDim = getDimension();
    if( dimension >= 0 && dimension < maxDim )
      return m_axis;
    throw new IndexOutOfBoundsException( Resources.format( ResourceKeys.ERROR_INDEX_OUT_OF_BOUNDS_$1, new Integer(
        dimension ) ) );
  }

  /**
   * Gets units for dimension within coordinate system. A temporal coordinate system have only one unit, always at index
   * 0.
   * 
   * @param dimension
   *          Must be 0.
   */
  @Override
  public Unit getUnits( final int dimension )
  {
    final int maxDim = getDimension();
    if( dimension >= 0 && dimension < maxDim )
      return m_unit;
    throw new IndexOutOfBoundsException( Resources.format( ResourceKeys.ERROR_INDEX_OUT_OF_BOUNDS_$1, new Integer(
        dimension ) ) );
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
      final TemporalCoordinateSystem that = (TemporalCoordinateSystem)cs;
      return Utilities.equals( this.m_datum, that.m_datum ) && Utilities.equals( this.m_unit, that.m_unit )
          && Utilities.equals( this.m_axis, that.m_axis );
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
    addUnit( buffer, m_unit );
    buffer.append( ", " );
    buffer.append( m_axis );
    return "TEMPORAL_CS";
  }
}