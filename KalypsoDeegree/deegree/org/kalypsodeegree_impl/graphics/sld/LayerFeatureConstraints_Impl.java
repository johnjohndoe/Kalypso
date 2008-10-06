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
package org.kalypsodeegree_impl.graphics.sld;

import java.util.ArrayList;

import org.kalypsodeegree.graphics.sld.FeatureTypeConstraint;
import org.kalypsodeegree.graphics.sld.LayerFeatureConstraints;
import org.kalypsodeegree.xml.Marshallable;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * The LayerFeatureConstraints element is optional in a NamedLayer and allows the user to specify constraints on what
 * features of what feature types are to be selected by the named-layer reference. It is essentially a filter that
 * allows the selection of fewer features than are present in the named layer.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version $Revision$ $Date$
 */
public class LayerFeatureConstraints_Impl implements LayerFeatureConstraints, Marshallable
{
  private ArrayList m_featureTypeConstraint = null;

  /**
   * constructor initializing the class with the <LayerFeatureConstraints>
   */
  LayerFeatureConstraints_Impl( FeatureTypeConstraint[] featureTypeConstraint )
  {
    this.m_featureTypeConstraint = new ArrayList();
    setFeatureTypeConstraint( featureTypeConstraint );
  }

  /**
   * A FeatureTypeConstraint element is used to identify a feature type by a well-known name, using the FeatureTypeName
   * element.
   * 
   * @return the FeatureTypeConstraints as Array
   */
  public FeatureTypeConstraint[] getFeatureTypeConstraint()
  {
    return (FeatureTypeConstraint[])m_featureTypeConstraint.toArray( new FeatureTypeConstraint[m_featureTypeConstraint
        .size()] );
  }

  /**
   * sets the <FeatureTypeConstraint>
   * 
   * @param featureTypeConstraint
   *          the <FeatureTypeConstraint>
   */
  public void setFeatureTypeConstraint( FeatureTypeConstraint[] featureTypeConstraint )
  {
    this.m_featureTypeConstraint.clear();

    if( featureTypeConstraint != null )
    {
      for( int i = 0; i < featureTypeConstraint.length; i++ )
      {
        this.m_featureTypeConstraint.add( featureTypeConstraint[i] );
      }
    }
  }

  /**
   * adds the <FeatureTypeConstraint>
   * 
   * @param featureTypeConstraint
   *          the <FeatureTypeConstraint>
   */
  public void addFeatureTypeConstraint( FeatureTypeConstraint featureTypeConstraint )
  {
    this.m_featureTypeConstraint.add( featureTypeConstraint );
  }

  /**
   * Removes a FeatureTypeConstraint.
   * 
   * @param featureTypeConstraint
   *          the <FeatureTypeConstraint>
   */
  public void removeFeatureTypeConstraint( FeatureTypeConstraint featureTypeConstraint )
  {
    this.m_featureTypeConstraint.remove( this.m_featureTypeConstraint.indexOf( featureTypeConstraint ) );
  }

  /**
   * returns the LayerFeatureConstraints as String.
   * 
   * @return the LayerFeatureConstraints as String
   */
  public String toString()
  {
    String ret = getClass().getName() + "\n";
    ret = "featureTypeConstraint = " + m_featureTypeConstraint + "\n";

    return ret;
  }

  /**
   * exports the content of the Font as XML formated String
   * 
   * @return xml representation of the Font
   */
  public String exportAsXML()
  {
    Debug.debugMethodBegin();

    StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<LayerFeatureConstraints>" );
    for( int i = 0; i < m_featureTypeConstraint.size(); i++ )
    {
      sb.append( ( (Marshallable)m_featureTypeConstraint.get( i ) ).exportAsXML() );
    }
    sb.append( "</LayerFeatureConstraints>" );

    Debug.debugMethodEnd();
    return sb.toString();
  }

}