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
package org.deegree_impl.services.wms.capabilities;

import org.deegree.services.wms.capabilities.UserDefinedSymbolization;
import org.deegree.xml.Marshallable;

/**
 * The interface defines the access to optional user-defined symbolization that
 * are only used by SLD-enabled WMSes.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$
 */
public class UserDefinedSymbolization_Impl implements UserDefinedSymbolization, Marshallable
{
  private boolean remoteWFSEnabled = false;

  private boolean sldSupported = false;

  private boolean userLayer = false;

  private boolean userStyle = false;

  /**
   * default constructor
   */
  UserDefinedSymbolization_Impl()
  {}

  /**
   * constructor initializing the class with the <UserDefinedSymbolization>
   */
  UserDefinedSymbolization_Impl( boolean sldSupported, boolean userLayer, boolean remoteWFSEnabled,
      boolean userStyle )
  {
    setSldSupported( sldSupported );
    setUserLayer( userLayer );
    setRemoteWFSEnabled( remoteWFSEnabled );
    setUserStyle( userStyle );
  }

  /**
   * returns true if layer and/or style definition conform to SLD are supported.
   */
  public boolean isSldSupported()
  {
    return sldSupported;
  }

  /**
   * sets true if layer and/or style definition conform to SLD are supported.
   */
  public void setSldSupported( boolean sldSupported )
  {
    this.sldSupported = sldSupported;
  }

  /**
   * returns true if the WMS has user defined layers.
   */
  public boolean hasUserLayer()
  {
    return userLayer;
  }

  /**
   * sets true if the WMS has user defined layers.
   */
  public void setUserLayer( boolean userLayer )
  {
    this.userLayer = userLayer;
  }

  /**
   * returns true if the WMS has user defined styles.
   */
  public boolean hasUserStyle()
  {
    return userStyle;
  }

  /**
   * sets true if the WMS has user defined styles.
   */
  public void setUserStyle( boolean userStyle )
  {
    this.userStyle = userStyle;
  }

  /**
   * returns true if the WMS enables the use of a remote (user defined) WFS.
   */
  public boolean isRemoteWFSEnabled()
  {
    return remoteWFSEnabled;
  }

  /**
   * sets true if the WMS enables the use of a remote (user defined) WFS.
   */
  public void setRemoteWFSEnabled( boolean remoteWFSEnabled )
  {
    this.remoteWFSEnabled = remoteWFSEnabled;
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = null;
    ret = "sldSupported = " + sldSupported + "\n";
    ret += ( "userLayer = " + userLayer + "\n" );
    ret += ( "remoteWFSEnabled = " + remoteWFSEnabled + "\n" );
    ret += ( "userStyle = " + userStyle + "\n" );
    return ret;
  }

  /**
   * Returns an XML representation of this object.
   */
  public String exportAsXML()
  {
    StringBuffer sb = new StringBuffer();

    sb.append( "<UserDefinedSymbolization" ).append( sldSupported ? " SupportSLD=\"1\"" : "" )
        .append( userLayer ? " UserLayer=\"1\"" : "" ).append( userStyle ? " UserStyle=\"1\"" : "" )
        .append( remoteWFSEnabled ? " RemoteWFS=\"1\"" : "" ).append( "/>" );

    return sb.toString();
  }
}