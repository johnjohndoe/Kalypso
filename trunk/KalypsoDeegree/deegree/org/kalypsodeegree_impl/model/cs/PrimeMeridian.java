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
package org.deegree_impl.model.cs;

// OpenGIS dependencies
import java.rmi.RemoteException;
import java.util.Map;

import javax.units.Unit;

import org.deegree_impl.model.resources.Utilities;
import org.opengis.cs.CS_AngularUnit;
import org.opengis.cs.CS_PrimeMeridian;

/**
 * A meridian used to take longitude measurements from.
 * 
 * @version 1.00
 * @author OpenGIS (www.opengis.org)
 * @author Martin Desruisseaux
 * 
 * @see org.opengis.cs.CS_PrimeMeridian
 */
public class PrimeMeridian extends Info
{
  /**
   * Serial number for interoperability with different versions.
   */
  private static final long serialVersionUID = 7570594768127669147L;

  /**
   * The Greenwich meridian, with angular measures in degrees.
   */
  public static final PrimeMeridian GREENWICH = (PrimeMeridian)pool.intern( new PrimeMeridian(
      "Greenwich", Unit.DEGREE, 0 ) );

  /**
   * The angular units.
   */
  private final Unit unit;

  /**
   * The longitude value relative to the Greenwich Meridian.
   */
  private final double longitude;

  /**
   * Creates a prime meridian, relative to Greenwich.
   * 
   * @param name
   *          Name to give new object.
   * @param unit
   *          Angular units of longitude.
   * @param longitude
   *          Longitude of prime meridian in supplied angular units East of
   *          Greenwich.
   *  
   */
  public PrimeMeridian( final String name, final Unit unit, final double longitude )
  {
    super( name );
    this.unit = unit;
    this.longitude = longitude;
    ensureNonNull( "unit", unit );
    ensureAngularUnit( unit );
  }

  /**
   * Creates a prime meridian, relative to Greenwich.
   * 
   * @param properties
   *          The set of properties (see {@link Info}).
   * @param unit
   *          Angular units of longitude.
   * @param longitude
   *          Longitude of prime meridian in supplied angular units East of
   *          Greenwich.
   */
  PrimeMeridian( final Map properties, final Unit unit, final double longitude )
  {
    super( properties );
    this.unit = unit;
    this.longitude = longitude;
    // Accept null values.
  }

  /**
   * Returns the longitude value relative to the Greenwich Meridian. The
   * longitude is expressed in this objects angular units.
   * 
   * @see org.opengis.cs.CS_PrimeMeridian#getLongitude()
   */
  public double getLongitude()
  {
    return longitude;
  }

  /**
   * Returns the longitude value relative to the Greenwich Meridian, expressed
   * in the specified units. This convenience method make easier to obtains
   * longitude in degrees (<code>getLongitude(Unit.DEGREE)</code>), no
   * matter the underlying angular units of this prime meridian.
   * 
   * @param targetUnit
   *          The unit in which to express longitude.
   */
  public double getLongitude( final Unit targetUnit )
  {
    return targetUnit.convert( getLongitude(), getAngularUnit() );
  }

  /**
   * Returns the angular units.
   * 
   * @see org.opengis.cs.CS_PrimeMeridian#getAngularUnit()
   */
  public Unit getAngularUnit()
  {
    return unit;
  }

  /**
   * Returns a hash value for this prime meridian.
   */
  public int hashCode()
  {
    final long code = Double.doubleToLongBits( longitude );
    return super.hashCode() * 37 + ( (int)( code >>> 32 ) ^ (int)code );
  }

  /**
   * Compares the specified object with this prime meridian for equality.
   */
  public boolean equals( final Object object )
  {
    if( super.equals( object ) )
    {
      final PrimeMeridian that = (PrimeMeridian)object;
      return Double.doubleToLongBits( this.longitude ) == Double.doubleToLongBits( that.longitude )
          && Utilities.equals( this.unit, that.unit );
    }
    return false;
  }

  /**
   * Fill the part inside "[...]". Used for formatting Well Know Text (WKT).
   */
  String addString( final StringBuffer buffer )
  {
    buffer.append( ", " );
    buffer.append( Unit.DEGREE.convert( longitude, unit ) );
    return "PRIMEM";
  }

  /**
   * Returns an OpenGIS interface for this prime meridian. The returned object
   * is suitable for RMI use.
   * 
   * Note: The returned type is a generic {@link Object}in order to avoid too
   * early class loading of OpenGIS interface.
   */
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
   * Wrap a {@link PrimeMeridian}object for use with OpenGIS. This class is
   * suitable for RMI use.
   * 
   * @version 1.0
   * @author Martin Desruisseaux
   */
  private final class Export extends Info.Export implements CS_PrimeMeridian
  {
    /**
     * Construct a remote object.
     */
    protected Export( final Object adapters )
    {
      super( adapters );
    }

    /**
     * Returns the longitude value relative to the Greenwich Meridian.
     */
    public double getLongitude() throws RemoteException
    {
      return PrimeMeridian.this.getLongitude();
    }

    /**
     * Returns the AngularUnits.
     * 
     * @throws RemoteException
     *           if a remote method call failed.
     */
    public CS_AngularUnit getAngularUnit() throws RemoteException
    {
      return (CS_AngularUnit)adapters.export( PrimeMeridian.this.getAngularUnit() );
    }
  }
}