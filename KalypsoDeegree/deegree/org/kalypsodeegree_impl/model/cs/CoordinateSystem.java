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
import java.io.Serializable;
import java.util.Map;

import javax.units.Unit;

import org.kalypsodeegree_impl.model.pt.Dimensioned;
import org.kalypsodeegree_impl.model.pt.Envelope;
import org.kalypsodeegree_impl.model.resources.Utilities;
import org.kalypsodeegree_impl.model.resources.css.ResourceKeys;
import org.kalypsodeegree_impl.model.resources.css.Resources;
import org.opengis.cs.CS_AxisInfo;
import org.opengis.cs.CS_CoordinateSystem;
import org.opengis.cs.CS_Unit;
import org.opengis.pt.PT_Envelope;

/**
 * Base class for all coordinate systems. A coordinate system is a mathematical space, where the elements of the space
 * are called positions. Each position is described by a list of numbers. The length of the list corresponds to the
 * dimension of the coordinate system. So in a 2D coordinate system each position is described by a list containing 2
 * numbers. <br>
 * <br>
 * However, in a coordinate system, not all lists of numbers correspond to a position - some lists may be outside the
 * domain of the coordinate system. For example, in a 2D Lat/Lon coordinate system, the list (91,91) does not correspond
 * to a position. <br>
 * <br>
 * Some coordinate systems also have a mapping from the mathematical space into locations in the real world. So in a
 * Lat/Lon coordinate system, the mathematical position (lat, long) corresponds to a location on the surface of the
 * Earth. This mapping from the mathematical space into real-world locations is called a Datum.
 * 
 * @version 1.00
 * @author OpenGIS (www.opengis.org)
 * @author Martin Desruisseaux
 * 
 * @see org.opengis.cs.CS_CoordinateSystem
 */
public abstract class CoordinateSystem extends Info implements Dimensioned
{
  /**
   * Serial number for interoperability with different versions.
   */
  private static final long serialVersionUID = -4539963180028417479L;

  /**
   * Construct a coordinate system.
   * 
   * @param name
   *          The coordinate system name.
   */
  public CoordinateSystem( final String name )
  {
    super( name );
  }

  /**
   * Construct a coordinate system.
   * 
   * @param properties
   *          The set of properties (see {@link Info}).
   */
  CoordinateSystem( final Map properties )
  {
    super( properties );
  }

  /**
   * Make sure there is no axis among the same direction (e.g. two north axis, or a east and a west axis). This methods
   * may be invoked from subclasses constructors.
   * 
   * @param type
   *          The datum type, or <code>null</code> if unknow.
   * @throws IllegalArgumentException
   *           if two axis have the same direction.
   */
  final void checkAxis( final DatumType type ) throws IllegalArgumentException
  {
    final int dimension = getDimension();
    for( int i = 0; i < dimension; i++ )
    {
      AxisOrientation check = getAxis( i ).orientation;
      if( type != null && !type.isCompatibleOrientation( check ) )
      {
        throw new IllegalArgumentException( Resources.format( ResourceKeys.ERROR_ILLEGAL_AXIS_ORIENTATION_$2, check
            .getName( null ), Utilities.getShortClassName( this ) ) );
      }
      check = check.absolute();
      if( !check.equals( AxisOrientation.OTHER ) )
      {
        for( int j = i + 1; j < dimension; j++ )
        {
          if( check.equals( getAxis( j ).orientation.absolute() ) )
          {
            final String nameI = getAxis( i ).orientation.getName( null );
            final String nameJ = getAxis( j ).orientation.getName( null );
            throw new IllegalArgumentException( Resources.format( ResourceKeys.ERROR_COLINEAR_AXIS_$2, nameI, nameJ ) );
          }
        }
      }
    }
  }

  /**
   * Returns the dimension of the coordinate system.
   * 
   * @see org.opengis.cs.CS_CoordinateSystem#getDimension()
   */
  public abstract int getDimension();

  /**
   * Gets axis details for dimension within coordinate system. Each dimension in the coordinate system has a
   * corresponding axis.
   * 
   * @param dimension
   *          Zero based index of axis.
   * 
   * @see org.opengis.cs.CS_CoordinateSystem#getAxis(int)
   */
  public abstract AxisInfo getAxis( int dimension );

  /**
   * Gets units for dimension within coordinate system. Each dimension in the coordinate system has corresponding units.
   * 
   * @param dimension
   *          Zero based index of axis.
   * 
   * @see org.opengis.cs.CS_CoordinateSystem#getUnits(int)
   */
  public abstract Unit getUnits( int dimension );

  /**
   * If all dimensions use the same units, returns this units. Otherwise, returns <code>null</code>.
   */
  final Unit getUnits()
  {
    Unit units = null;
    for( int i = getDimension(); --i >= 0; )
    {
      final Unit check = getUnits( i );
      if( units == null )
        units = check;
      else if( !units.equals( check ) )
        return null;
    }
    return units;
  }

  /**
   * Returns the datum.
   */
  Datum getDatum()
  {
    return null;
  }

  /**
   * Gets default envelope of coordinate system. Coordinate systems which are bounded should return the minimum bounding
   * box of their domain. Unbounded coordinate systems should return a box which is as large as is likely to be used.
   * For example, a (lon,lat) geographic coordinate system in degrees should return a box from (-180,-90) to (180,90),
   * and a geocentric coordinate system could return a box from (-r,-r,-r) to (+r,+r,+r) where r is the approximate
   * radius of the Earth. <br>
   * <br>
   * The default implementation returns an envelope with infinite bounds.
   * 
   * @see org.opengis.cs.CS_CoordinateSystem#getDefaultEnvelope()
   */
  public Envelope getDefaultEnvelope()
  {
    final int dimension = getDimension();
    final Envelope envelope = new Envelope( dimension );
    for( int i = dimension; --i >= 0; )
    {
      envelope.setRange( i, Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY );
    }
    return envelope;
  }

  /**
   * Returns <code>true</code> if this coordinate system is equivalents to the specified coordinate system. Two
   * coordinate systems are considered equivalent if the
   * {@link org.kalypsodeegree_impl.model.ct.CoordinateTransformation}from <code>this</code> to <code>cs</code>
   * would be the identity transform. The <code>equivalents</code> method is less strict than <code>equals</code> in
   * that it doesn't compare names, alias, authority codes or others similar informations.
   * 
   * @param cs
   *          The coordinate system (may be <code>null</code>).
   * @return <code>true</code> if both coordinate systems are equivalent.
   */
  public boolean equivalents( final CoordinateSystem cs )
  {
    return ( cs != null ) && cs.getClass().equals( getClass() );
  }

  /**
   * Compares the specified object with this coordinate system for equality.
   */
  public boolean equals( final Object object )
  {
    if( object == this )
      return true; // Slight optimization
    return super.equals( object ) && equivalents( (CoordinateSystem)object );
  }

  /**
   * Returns an OpenGIS interface for this coordinate system. The returned object is suitable for RMI use.
   * 
   * Note: The returned type is a generic {@link Object}in order to avoid too early class loading of OpenGIS interface.
   */
  Object toOpenGIS( final Object adapters )
  {
    return new Export( adapters );
  }

  /////////////////////////////////////////////////////////////////////////
  //////////////// ////////////////
  //////////////// OPENGIS ADAPTER ////////////////
  //////////////// ////////////////
  /////////////////////////////////////////////////////////////////////////

  /**
   * Wrap a {@link CoordinateSystem}object for use with OpenGIS. This class is suitable for RMI use.
   * 
   * @version 1.0
   * @author Martin Desruisseaux
   */
  class Export extends Info.Export implements CS_CoordinateSystem, Serializable
  {
    private static final long serialVersionUID = 3834591023795352117L;

    /**
     * Construct a remote object.
     */
    protected Export( final Object adapters )
    {
      super( adapters );
    }

    /**
     * Dimension of the coordinate system.
     */
    public int getDimension()
    {
      return CoordinateSystem.this.getDimension();
    }

    /**
     * Gets axis details for dimension within coordinate system.
     */
    public CS_AxisInfo getAxis( final int dimension )
    {
      return adapters.export( CoordinateSystem.this.getAxis( dimension ) );
    }

    /**
     * Gets units for dimension within coordinate system.
     */
    public CS_Unit getUnits( final int dimension )
    {
      return adapters.export( CoordinateSystem.this.getUnits( dimension ) );
    }

    /**
     * Gets default envelope of coordinate system.
     */
    public PT_Envelope getDefaultEnvelope()
    {
      return adapters.PT.export( CoordinateSystem.this.getDefaultEnvelope() );
    }
  }
}