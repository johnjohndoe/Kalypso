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
package org.kalypsodeegree_impl.model.ct;

// OpenGIS dependencies
import java.io.ObjectStreamException;
import java.util.Locale;
import java.util.NoSuchElementException;

import javax.media.jai.EnumeratedParameter;

import org.kalypsodeegree_impl.model.resources.XArray;
import org.kalypsodeegree_impl.model.resources.css.ResourceKeys;
import org.kalypsodeegree_impl.model.resources.css.Resources;
import org.opengis.ct.CT_DomainFlags;

/**
 * Flags indicating parts of domain covered by a convex hull. These flags can be
 * combined. For example, the enum
 * <code>{@link #INSIDE}.or({@link #OUTSIDE})</code> means that some parts
 * of the convex hull are inside the domain, and some parts of the convex hull
 * are outside the domain.
 * 
 * @version 1.00
 * @author OpenGIS (www.opengis.org)
 * @author Martin Desruisseaux
 * 
 * @see org.opengis.ct.CT_DomainFlags
 */
public final class DomainFlags extends EnumeratedParameter
{
  /**
   * Serial number for interoperability with different versions.
   */
  private static final long serialVersionUID = 6991557069252861278L;

  /**
   * Domain flags by value. Used to canonicalize after deserialization.
   */
  private static final DomainFlags[] ENUMS = new DomainFlags[8];

  /**
   * Initialize {@link #ENUMS}during class loading. This code must be done
   * before we initialize public fields.
   */
  static
  {
    for( int i = ENUMS.length; --i >= 0; )
    {
      String name = null;
      switch( i )
      {
      case 0:
        name = "UNKNOW";
        break;
      case CT_DomainFlags.CT_DF_Inside:
        name = "INSIDE";
        break;
      case CT_DomainFlags.CT_DF_Outside:
        name = "OUTSIDE";
        break;
      case CT_DomainFlags.CT_DF_Discontinuous:
        name = "DISCONTINUOUS";
        break;
      }
      ENUMS[i] = new DomainFlags( name, i );
    }
  }

  /**
   * At least one point in a convex hull is inside the transform's domain.
   * 
   * @see org.opengis.ct.CT_DomainFlags#CT_DF_Inside
   */
  public static final DomainFlags INSIDE = ENUMS[CT_DomainFlags.CT_DF_Inside];

  /**
   * At least one point in a convex hull is outside the transform's domain.
   * 
   * @see org.opengis.ct.CT_DomainFlags#CT_DF_Outside
   */
  public static final DomainFlags OUTSIDE = ENUMS[CT_DomainFlags.CT_DF_Outside];

  /**
   * At least one point in a convex hull is not transformed continuously. As an
   * example, consider a "Longitude_Rotation" transform which adjusts longitude
   * coordinates to take account of a change in Prime Meridian. If the rotation
   * is 5 degrees east, then the point (Lat=0,Lon=175) is not transformed
   * continuously, since it is on the meridian line which will be split at
   * +180/-180 degrees.
   * 
   * @see org.opengis.ct.CT_DomainFlags#CT_DF_Discontinuous
   */
  public static final DomainFlags DISCONTINUOUS = ENUMS[CT_DomainFlags.CT_DF_Discontinuous];

  /**
   * Construct a new enum value.
   */
  private DomainFlags( final String name, final int value )
  {
    super( name, value );
  }

  /**
   * Return the enum for the specified value. This method is provided for
   * compatibility with {@link org.opengis.ct.CT_DomainFlags}.
   * 
   * @param value
   *          The enum value.
   * @return The enum for the specified value.
   * @throws NoSuchElementException
   *           if there is no enum for the specified value.
   */
  public static DomainFlags getEnum( final int value ) throws NoSuchElementException
  {
    if( value >= 1 && value < ENUMS.length )
      return ENUMS[value];
    throw new NoSuchElementException( String.valueOf( value ) );
  }

  /**
   * Returns enum's names in the specified locale. For example if this enum has
   * value "3", then <code>getNames</code> returns an array of two elements:
   * "Inside" and "Outside".
   * 
   * @param locale
   *          The locale, or <code>null</code> for the current default locale.
   * @return Enum's names in the specified locale (never <code>null</code>).
   */
  public String[] getNames( final Locale locale )
  {
    int count = 0;
    int bits = getValue();
    Resources resources = null;
    final int[] nameKeys =
    { ResourceKeys.INSIDE, ResourceKeys.OUTSIDE, ResourceKeys.DISCONTINUOUS };
    final String[] names = new String[nameKeys.length];
    for( int i = 0; i < nameKeys.length; i++ )
    {
      if( ( bits & 1 ) != 0 )
      {
        if( resources == null )
          resources = Resources.getResources( locale );
        names[count++] = resources.getString( nameKeys[i] );
      }
      bits >>>= 1;
    }
    return (String[])XArray.resize( names, count );
  }

  /**
   * Returns a combination of two domain flags. This is equivalent to
   * <code>getEnum(this.getValue()&nbsp;|&nbsp;flags.getValue())</code>.
   */
  public DomainFlags or( final DomainFlags flags )
  {
    return getEnum( getValue() | flags.getValue() );
  }

  /**
   * Use a single instance of {@link DomainFlags}after deserialization. It
   * allow client code to test <code>enum1==enum2</code> instead of
   * <code>enum1.equals(enum2)</code>.
   * 
   * @return A single instance of this enum.
   * @throws ObjectStreamException
   *           is deserialization failed.
   */
  private Object readResolve() throws ObjectStreamException
  {
    final int value = getValue();
    if( value >= 0 && value < ENUMS.length )
      return ENUMS[value]; // Canonicalize
    else
      return ENUMS[0]; // Collapse unknow value to a single canonical one
  }
}