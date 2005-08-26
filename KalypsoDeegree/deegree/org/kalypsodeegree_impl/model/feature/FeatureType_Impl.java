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
import java.util.HashMap;
import java.util.Map;

import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;

/**
 * 
 * 
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
class FeatureType_Impl extends AbstractFeatureType implements FeatureType, Serializable
{

  private final String m_substitutionGroup;

  private final FeatureTypeProperty[] m_properties;

  private final int[] m_minOccurs;

  private final int[] m_maxOccurs;

  private final HashMap m_posOfFTP;

  private int m_defaultGeometryPropPos = -1;

  private FeatureTypeProperty[] m_virtualProperties = new FeatureTypeProperty[0];

  public FeatureType_Impl( String name, String namespace, FeatureTypeProperty[] properties, int[] minOccurs,
      int[] maxOccurs, String substitutionGroup, Map annotationMap )
  {
    super( name, namespace, annotationMap );

    m_substitutionGroup = substitutionGroup;
    m_properties = properties;
    m_minOccurs = minOccurs;
    m_maxOccurs = maxOccurs;
    m_posOfFTP = new HashMap();
    for( int i = 0; i < properties.length; i++ )
    {
      // set default geoemtry
      if( m_defaultGeometryPropPos < 0 && properties[i].isGeometryProperty() )
        m_defaultGeometryPropPos = i;

      // this supports qualified and unqualified position questions

      m_posOfFTP.put( properties[i].getName(), new int[]
      { i } );
      m_posOfFTP.put( properties[i].getNamespace() + ":" + properties[i].getName(), new int[]
      { i } );
    }
  }

  /**
   * returns the properties of this feature type
   */
  public FeatureTypeProperty[] getProperties()
  {
    return m_properties;
  }

  /**
   * returns a property of this feature type identified by its name
   */
  public FeatureTypeProperty getProperty( String name )
  {
    try
    {
      return m_properties[getPropertyPosition( name )];
    }
    catch( Exception e )
    {
      return null;
    }
  }

  public String toString()
  {
    String ret = null;
    // ret = "parents = " + parents + "\n";
    // ret += "children = " + children + "\n";
    ret += "name = " + getName() + "\n";
    ret += "properties = ";
    for( int i = 0; i < m_properties.length; i++ )
    {
      ret += m_properties[i] + "\n";
    }
    return ret;
  }

  public int getMinOccurs( int pos )
  {
    return m_minOccurs[pos];
  }

  public int getMaxOccurs( int pos )
  {
    return m_maxOccurs[pos];
  }

  public int getPropertyPosition( String name )
  {
    final int[] object = (int[])m_posOfFTP.get( name );
    if( object == null || object.length == 0 )
      return -1;
    return object[0];
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureType#getDefaultGeometryPropertyPosition()
   */
  public int getDefaultGeometryPropertyPosition()
  {
    return m_defaultGeometryPropPos;
  }

  public String getSubstitutionGroup()
  {
    return m_substitutionGroup;

  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureType#setVirtuelFeatureTypeProperty(org.kalypsodeegree.model.feature.FeatureTypeProperty[])
   */
  public void setVirtuelFeatureTypeProperty( FeatureTypeProperty[] virtualProperties )
  {
    m_virtualProperties = virtualProperties;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureType#getVirtuelFeatureTypeProperty()
   */
  public FeatureTypeProperty[] getVirtuelFeatureTypeProperty()
  {
    return m_virtualProperties;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureType#getDefaultGeometryProperty()
   */
  public FeatureTypeProperty getDefaultGeometryProperty()
  {
    if( m_defaultGeometryPropPos > -1 )
      return m_properties[m_defaultGeometryPropPos];
    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureType#getVirtuelFeatureTypeProperty(java.lang.String)
   */
  public FeatureTypeProperty getVirtuelFeatureTypeProperty( String propName )
  {
    for( int i = 0; i < m_virtualProperties.length; i++ )
      if( propName.equals( m_virtualProperties[i].getName() ) )
        return m_virtualProperties[i];
    return null;
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  public boolean equals( Object obj )
  {
    if( obj == null || ( !( obj instanceof FeatureType ) ) )
      return false;
    final FeatureType other = (FeatureType)obj;
    if( getNamespace() != null )
      if( !getNamespace().equals( other.getNamespace() ) )
        return false;
    if( !getName().equals( other.getName() ) )
      return false;
    return true;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  public int hashCode()
  {
    if( getNamespace() != null )
      return ( getNamespace() + getName() ).hashCode();
    return getName().hashCode();
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureType#isVirtuelProperty(java.lang.String)
   */
  public boolean isVirtuelProperty( String propertyName )
  {
    for( int i = 0; i < m_virtualProperties.length; i++ )
    {
      if( m_virtualProperties[i].getName().equals( propertyName ) )
        return true;
    }
    return false;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureType#getMaxOccurs(java.lang.String)
   */
  public int getMaxOccurs( String linkName )
  {
    return m_maxOccurs[getPropertyPosition( linkName )];
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureType#getMinOccurs(java.lang.String)
   */
  public int getMinOccurs( String linkName )
  {
    return m_minOccurs[getPropertyPosition( linkName )];
  }

  /**
   * 
   * @see org.kalypsodeegree.model.feature.FeatureType#isListProperty(java.lang.String)
   */
  public boolean isListProperty( String propName )
  {
    final int maxOccurs = getMaxOccurs( propName );
    return ( maxOccurs > 1 || maxOccurs == UNBOUND_OCCURENCY );
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureType#hasGeometryProperty()
   */
  public boolean hasGeometryProperty()
  {
    if( m_defaultGeometryPropPos != -1 )
      return true;
    for( int i = 0; i < m_properties.length; i++ )
    {
      FeatureTypeProperty property = m_properties[i];
      if( property.isGeometryProperty() )
        return true;
    }
    return false;
  }
}