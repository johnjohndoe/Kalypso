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

// Miscellaneous
import java.util.Map;

/**
 * Procedure used to measure time.
 * 
 * @version 1.0
 * @author Martin Desruisseaux
 */
public class TemporalDatum extends Datum
{
  /**
   * Serial number for interoperability with different versions.
   */
  private static final long serialVersionUID = 6313740402733520130L;

  /**
   * Default datum for Universal Time Clock (UTC). UTC is based on an atomic clock, while GMT is based on astronomical
   * observations.
   */
  public static final TemporalDatum UTC = (TemporalDatum)pool.intern( new TemporalDatum( "UTC", DatumType.UTC ) );

  /**
   * Creates a temporal datum from an enumerated type value.
   * 
   * @param name
   *          Name to give new object.
   * @param type
   *          Type of temporal datum to create.
   */
  public TemporalDatum( final String name, final DatumType.Temporal type )
  {
    super( name, type );
  }

  /**
   * Creates a temporal datum.
   * 
   * @param properties
   *          The set of properties (see {@link Info}).
   * @param type
   *          Type of temporal datum to create.
   */
  TemporalDatum( final Map properties, final DatumType type )
  {
    super( properties, type );
  }

  /**
   * Gets the type of the datum as an enumerated code.
   * 
   * Note: return type will be changed to {@link DatumType.Temporal}when we will be able to use generic types (with JDK
   * 1.5).
   */
  public DatumType/* .Temporal */getDatumType()
  {
    return (DatumType.Temporal)super.getDatumType();
  }

  /**
   * Fill the part inside "[...]". Used for formatting Well Know Text (WKT).
   */
  String addString( final StringBuffer buffer )
  {
    super.addString( buffer );
    return "TEMPORAL_DATUM";
  }
}