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

package org.deegree_impl.services.wcas.capabilities;

import java.io.BufferedReader;
import java.io.FileReader;
import java.net.URL;
import java.util.HashMap;

import org.deegree.services.wcas.capabilities.Capability;
import org.deegree.services.wcas.capabilities.WCASCapabilities;
import org.deegree.services.wcas.metadatadesc.ISO19119;
import org.deegree_impl.services.capabilities.OGCWebServiceCapabilities_Impl;

/**
 * The parent element of the Capabilities document includes as children a
 * Service element with general information about the server, a Capability
 * element with specific information about the kinds of functionality offered by
 * the server and a featureTypeList element defining the list of all feature
 * types available from this server.
 * <p>
 * -----------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public final class WCASCapabilities_Impl extends OGCWebServiceCapabilities_Impl implements
    WCASCapabilities
{

  private Capability capability = null;

  private ISO19119 iso19119 = null;

  private HashMap locations = null;

  private String capabilitiesSource = null;

  /**
   * @param version
   * @param updateSequence
   * @param iso19119
   * @param capability
   * @param locations
   */
  WCASCapabilities_Impl( String version, String updateSequence, ISO19119 iso19119,
      Capability capability, HashMap locations )
  {
    super( version, updateSequence, null );

    setCapability( capability );
    setISO19119( iso19119 );
    setSchemaLocations( locations );
  }

  /**
   * A Capability lists available request types, how exceptions may be reported,
   * and whether any vendor-specific capabilities are defined. It also lists all
   * the feature types available from this feature server.
   */
  public Capability getCapability()
  {
    return capability;
  }

  /**
   * @see WCASCapabilities_Impl#getCapability()
   */
  public void setCapability( Capability capability )
  {
    this.capability = capability;
  }

  /**
   * returns the service metadata of the catalog
   */
  public ISO19119 getISO19119()
  {
    return iso19119;
  }

  /**
   * @see WCASCapabilities_Impl#getISO19119()
   */
  public void setISO19119( ISO19119 iso19119 )
  {
    this.iso19119 = iso19119;
  }

  /**
   * return the URL (location) of the schema definition for the submitted format
   *  
   */
  public URL getSchemaLocation( String format )
  {
    return (URL)locations.get( format );
  }

  /**
   * @see WCASCapabilities_Impl#getSchemaLocation(String)
   */
  public void setSchemaLocations( HashMap locations )
  {
    this.locations = locations;
  }

  /**
   * @see WCASCapabilities_Impl#getSchemaLocation(String)
   */
  public void addSchemaLocation( String format, URL location )
  {
    locations.put( format, location );
  }

  public void setCapabilitiesSource( String capabilitiesSource )
  {
    this.capabilitiesSource = capabilitiesSource;
  }

  /**
   * exports the capabilities as OGC WFS conform XML document
   */
  public String exportAsXML()
  {
    StringBuffer sb = new StringBuffer( 10000 );
    try
    {
      BufferedReader br = new BufferedReader( new FileReader( capabilitiesSource ) );
      String line = null;
      while( ( line = br.readLine() ) != null )
      {
        sb.append( line );
      }
      br.close();
    }
    catch( Exception e )
    {
      System.out.println( e );
    }

    return sb.toString();
  }

}