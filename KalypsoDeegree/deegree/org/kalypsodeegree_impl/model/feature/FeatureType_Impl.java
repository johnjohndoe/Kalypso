/*----------------    FILE HEADER  ------------------------------------------
 
 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de
 
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
 
 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de
 
 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de
 
 
 ---------------------------------------------------------------------------*/

package org.deegree_impl.model.feature;

import java.io.Serializable;
import java.util.HashMap;

import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;

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
class FeatureType_Impl implements FeatureType, Serializable
{
  private final String m_namespace;

  // private final FeatureType[] children;
  // es werden keine children gebraucht, da keine featuretypes direkt unterhalb
  // eines featuretypes sein kann, es können höchstens featureassociationtypes
  // unterhalb von featuretype sein, diese werden
  // jedoch von featuretypeproperty abgeleitet

  private final String m_name;

  private final FeatureTypeProperty[] m_properties;

  private final int[] m_minOccurs;

  private final int[] m_maxOccurs;

  private final HashMap m_posOfFTP;

  private int m_defaultGeometryPropPos = -1;

  public FeatureType_Impl( String name, String namespace, FeatureTypeProperty[] properties,
      int[] minOccurs, int[] maxOccurs )
  {
    this.m_name = name;
    m_namespace = namespace;
    this.m_properties = properties;
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
   * returns the direct parents of the FeatureType. If it hasn't a parent null
   * should be returned
   */
  public FeatureType[] getParents()
  {
    return null;
  }

  /**
   * returns the direct children of the FeatureType
   */
  public FeatureType[] getChildren()
  {
    return null;
  }

  /**
   * returns the name of the FeatureType
   */
  public String getName()
  {
    return m_name;
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
    ret += "name = " + m_name + "\n";
    ret += "properties = ";
    for( int i = 0; i < m_properties.length; i++ )
    {
      ret += m_properties[i] + "\n";
    }
    return ret;
  }

  public String getNamespace()
  {
    return m_namespace;
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
    return ( (int[])m_posOfFTP.get( name ) )[0];
  }

  /**
   * @see org.deegree.model.feature.FeatureType#getDefaultGeometryPropertyPosition()
   */
  public int getDefaultGeometryPropertyPosition()
  {
    return m_defaultGeometryPropPos;
  }
}