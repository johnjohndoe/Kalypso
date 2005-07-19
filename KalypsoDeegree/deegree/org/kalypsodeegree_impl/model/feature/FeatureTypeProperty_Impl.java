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

package org.kalypsodeegree_impl.model.feature;

import java.io.Serializable;
import java.util.Map;

import org.kalypsodeegree.model.feature.FeatureTypeProperty;

/**
 * 
 * the interface describes a property entry of a feature type. the name of the property must be equal to the name of the
 * corresponding feature property.
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class FeatureTypeProperty_Impl extends AbstractFeatureType implements FeatureTypeProperty, Serializable
{
  private final String m_type;

  private final boolean m_nullable;

  /**
   * initializes a FeatureTypeProperty with its name its associated type and a boolean variable that says if the
   * propetry maybe <tt>null</tt>
   */
  protected FeatureTypeProperty_Impl( final String name, final String namespace, final String type, final boolean nullable, final Map annotationMap )
  {
    super( name, namespace, annotationMap );
  
    if( type == null )
      throw new NullPointerException( "'type' is null" );
    
    m_type = type;
    m_nullable = nullable;
  }

  /**
   * returns the name of the data type of the property
   */
  public String getType()
  {
    return m_type;
  }

  /**
   * returns true if the property value is allowed to be null
   */
  public boolean isNullable()
  {
    return m_nullable;
  }

  public String toString()
  {
    String ret = null;
    ret = "name = " + getName() + "\n";
    ret += "type = " + m_type + "\n";
    ret += "nullable = " + m_nullable + "\n";
    return ret;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureTypeProperty#isGeometryProperty()
   */
  public boolean isGeometryProperty()
  {
    return m_type.startsWith( "org.kalypsodeegree.model.geometry." ) && !m_type.endsWith( "Envelope" );
  }

  /**
   * 
   * @see java.lang.Object#equals(java.lang.Object)
   */
  public boolean equals( Object obj )
  {
    if( obj == null || !( obj instanceof FeatureTypeProperty ) )
      return false;
    final FeatureTypeProperty other = (FeatureTypeProperty)obj;
    return other.getName().equals( getName() ) && other.getNamespace().equals( getNamespace() );
  }

  /**
   * 
   * @see java.lang.Object#hashCode()
   */
  public int hashCode()
  {
    if( getNamespace() != null )
      return ( getName() + getNamespace() ).hashCode();
    return getName().hashCode();
  }
}