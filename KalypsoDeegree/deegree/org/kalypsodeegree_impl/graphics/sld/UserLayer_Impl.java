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

import org.kalypsodeegree.graphics.sld.LayerFeatureConstraints;
import org.kalypsodeegree.graphics.sld.RemoteOWS;
import org.kalypsodeegree.graphics.sld.Style;
import org.kalypsodeegree.graphics.sld.UserLayer;
import org.kalypsodeegree.xml.Marshallable;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * In addition to using named layers, it is also useful to be able to define
 * custom user-defined layers for rendering.
 * <p>
 * </p>
 * Since a layer is defined as a collection of potentially mixed-type features,
 * the UserLayer element must provide the means to identify the features to be
 * used. All features to be rendered are assumed to be fetched from a Web
 * Feature Server (WFS) or a Web Coverage Service (WCS, in which case the term
 * features is used loosely).
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version $Revision$ $Date$
 */
public class UserLayer_Impl extends Layer_Impl implements UserLayer, Marshallable
{
  private RemoteOWS remoteOWS = null;

  /**
   * constructor initializing the class with the <UserLayer>
   */
  UserLayer_Impl( String name, LayerFeatureConstraints layerFeatureConstraints, Style[] userStyles,
      RemoteOWS remoteOWS )
  {
    super( name, layerFeatureConstraints, userStyles );
    setRemoteOWS( remoteOWS );
  }

  /**
   * the method returns a remote web service that enables the access to data
   * that are not stored on same server as the WMS.
   * 
   * @return the RemoteOWS
   */
  public RemoteOWS getRemoteOWS()
  {
    return remoteOWS;
  }

  /**
   * sets the <RemoteOWS>
   * 
   * @param remoteOWS
   *          the RemoteOWS
   */
  public void setRemoteOWS( RemoteOWS remoteOWS )
  {
    this.remoteOWS = remoteOWS;
  }

  public String toString()
  {
    String ret = getClass().getName() + "\n";
    ret = "remoteOWS = " + remoteOWS + "\n";

    return ret;
  }

  /**
   * exports the content of the UserLayer as XML formated String
   * 
   * @return xml representation of the UserLayer
   */
  public String exportAsXML()
  {
    Debug.debugMethodBegin();

    StringBuffer sb = new StringBuffer( 5000 );
    sb.append( "<UserLayer>" );
    sb.append( "<Name>" ).append( name ).append( "</Name>" );
    if( remoteOWS != null )
    {
      sb.append( ( (Marshallable)remoteOWS ).exportAsXML() );
    }

    sb.append( ( (Marshallable)layerFeatureConstraints ).exportAsXML() );

    for( int i = 0; i < styles.size(); i++ )
    {
      sb.append( ( (Marshallable)styles.get( i ) ).exportAsXML() );
    }
    sb.append( "</UserLayer>" );

    Debug.debugMethodEnd();
    return sb.toString();
  }
}