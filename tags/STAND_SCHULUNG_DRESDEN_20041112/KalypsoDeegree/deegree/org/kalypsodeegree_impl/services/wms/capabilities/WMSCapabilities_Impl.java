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

import org.deegree.services.capabilities.Service;
import org.deegree.services.wms.capabilities.Capability;
import org.deegree.services.wms.capabilities.DeegreeParam;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree.xml.Marshallable;
import org.deegree_impl.services.capabilities.OGCWebServiceCapabilities_Impl;

/**
 * The purpose of the GetCapabilities operation is described in the Basic
 * Service Elements section, above. In the particular case of a Web Map Service,
 * the response of a GetCapabilities request is general information about the
 * service itself and specific information about the available maps.
 * 
 * The available output formats and the online resource are listed for each
 * operation offered by the server,
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$
 */
public class WMSCapabilities_Impl extends OGCWebServiceCapabilities_Impl implements
    WMSCapabilities, Marshallable
{
  private Capability capability = null;

  private DeegreeParam deegreeParam = null;

  /**
   * constructor initializing the class with the <WMSCapabilities>
   */
  WMSCapabilities_Impl( String version, String updateSequence, Service service,
      Capability capability, DeegreeParam deegreeParam )
  {
    super( version, updateSequence, service );

    setCapability( capability );
    setDeegreeParam( deegreeParam );
  }

  /**
   * returns deegree specific capabilities/coniguration parameters
   *  
   */
  public DeegreeParam getDeegreeParam()
  {
    return deegreeParam;
  }

  /**
   * 
   * 
   * @param deegreeParam
   */
  public void setDeegreeParam( DeegreeParam deegreeParam )
  {
    this.deegreeParam = deegreeParam;
  }

  /**
   * The <Capability>element of the Capabilities XML names the actual operations
   * that are supported by the service instance, the output formats offered for
   * those operations, and the URL prefix for each operation.
   */
  public Capability getCapability()
  {
    return capability;
  }

  /**
   * sets the capability
   */
  public void setCapability( Capability capability )
  {
    this.capability = capability;
  }

  /**
   * Returns an XML representation of this object.
   */
  public String exportAsXML()
  {
    StringBuffer sb = new StringBuffer();
    sb.append( "<WMT_MS_Capabilities " ).append(
        "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" " ).append(
        "version=\"1.1.1\" updateSequence=\"1.1.0\">" );

    if( deegreeParam != null )
    {
      sb.append( ( (Marshallable)deegreeParam ).exportAsXML() );
    }

    sb.append( ( (Marshallable)getService() ).exportAsXML() ).append(
        ( (Marshallable)capability ).exportAsXML() );

    sb.append( "</WMT_MS_Capabilities>" );

    return sb.toString();
  }
}