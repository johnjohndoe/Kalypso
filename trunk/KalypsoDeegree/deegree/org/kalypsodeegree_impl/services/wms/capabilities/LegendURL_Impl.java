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

import java.net.URL;

import org.deegree.services.wms.capabilities.LegendURL;
import org.deegree.xml.Marshallable;
import org.deegree.xml.XMLTools;
import org.deegree_impl.ogcbasic.ImageURL;
import org.deegree_impl.tools.NetWorker;

/**
 * A Map Server may use zero or more LegendURL elements to provide an image(s)
 * of a legend relevant to each Style of a Layer. The Format element indicates
 * the MIME type of the legend. Width and height attributes are provided to
 * assist client applications in laying out space to display the legend.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$
 */
public class LegendURL_Impl extends ImageURL implements LegendURL, Marshallable
{

  /*
   * constructor initializing the class with the <LegendURL>
   */
  LegendURL_Impl( int width, int height, String format, URL onlineResource )
  {
    super( width, height, format, onlineResource );
  }

  /**
   * Returns an XML representation of this object.
   */
  public String exportAsXML()
  {
    StringBuffer sb = new StringBuffer();

    sb.append( "<LegendURL width=\"" ).append( getWidth() ).append( "\" height=\"" ).append(
        getHeight() ).append( "\">" ).append( "<Format>" ).append(
        XMLTools.validateCDATA( getFormat() ) ).append( "</Format>" ).append( "<OnlineResource " )
        .append( "xmlns:xlink=\"http://www.w3.org/1999/xlink\" " ).append(
            "xlink:type=\"simple\" xlink:href=\"" ).append(
            NetWorker.url2String( getOnlineResource() ) ).append( "\"/>" ).append( "</LegendURL>" );

    return sb.toString();
  }
}