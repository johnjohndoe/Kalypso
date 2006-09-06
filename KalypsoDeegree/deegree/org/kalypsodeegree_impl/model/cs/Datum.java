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
import org.opengis.cs.CS_Datum;
import org.opengis.cs.CS_DatumType;

/**
 * A set of quantities from which other quantities are calculated. It may be a textual description and/or a set of
 * parameters describing the relationship of a coordinate system to some predefined physical locations (such as center
 * of mass) and physical directions (such as axis of spin). It can be defined as a set of real points on the earth that
 * have coordinates. For example a datum can be thought of as a set of parameters defining completely the origin and
 * orientation of a coordinate system with respect to the earth. The definition of the datum may also include the
 * temporal behavior (such as the rate of change of the orientation of the coordinate axes).
 * 
 * @version 1.00
 * @author OpenGIS (www.opengis.org)
 * @author Martin Desruisseaux
 * 
 * @see org.opengis.cs.CS_Datum
 */
public class Datum extends Info
{
  /**
   * Serial number for interoperability with different versions.
   */
  private static final long serialVersionUID = 2175857309476007487L;

  /**
   * The datum type.
   */
  private final DatumType type;

  /**
   * Construct a new datum with the specified name and datum type.
   * 
   * @param name
   *          The datum name.
   * @param type
   *          The datum type.
   */
  public Datum( final String name, final DatumType type )
  {
    super( name );
    this.type = type;
    ensureNonNull( "type", type );
  }

  /**
   * Construct a new datum with the specified properties.
   * 
   * @param properties
   *          The set of properties (see {@link Info}).
   * @param type
   *          The datum type.
   */
  Datum( final Map properties, final DatumType type )
  {
    super( properties );
    this.type = type;
    // Accept null value.
  }

  /**
   * Gets the type of the datum as an enumerated code.
   * 
   * @see org.opengis.cs.CS_Datum#getDatumType()
   */
  public DatumType getDatumType()
  {
    return type;
  }

  /**
   * Returns a hash value for this datum.
   */
  public int hashCode()
  {
    int code = 37 * super.hashCode();
    final DatumType t = getDatumType();
    if( t != null )
      code += t.hashCode();
    return code;
  }

  /**
   * Compares the specified object with this datum for equality.
   */
  public boolean equals( final Object object )
  {
    if( super.equals( object ) )
    {
      final Datum that = (Datum)object;
      return Utilities.equals( this.type, that.type );
    }
    return false;
  }

  /**
   * Fill the part inside "[...]". Used for formatting Well Know Text (WKT).
   */
  String addString( final StringBuffer buffer )
  {
    buffer.append( ", " );
    buffer.append( type.getName() );
    return "DATUM";
  }

  /**
   * Returns an OpenGIS interface for this datum. The returned object is suitable for RMI use.
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
   * Wrap a {@link Datum}object for use with OpenGIS. This class is suitable for RMI use.
   * 
   * @version 1.0
   * @author Martin Desruisseaux
   */
  class Export extends Info.Export implements CS_Datum
  {
    /**
     * Construct a remote object.
     */
    protected Export( final Object adapters )
    {
      super( adapters );
    }

    /**
     * Gets the type of the datum as an enumerated code.
     */
    public CS_DatumType getDatumType()
    {
      return m_adapters.export( Datum.this.getDatumType() );
    }
  }
}