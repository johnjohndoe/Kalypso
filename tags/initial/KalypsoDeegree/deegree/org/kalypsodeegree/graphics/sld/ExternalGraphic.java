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

package org.deegree.graphics.sld;

import java.net.*;
import java.awt.image.*;

/**
 * The ExternalGraphic element allows a reference to be made to an external graphic
 * file with a Web URL. The OnlineResource sub-element gives the URL and the
 * Format sub-element identifies the expected document MIME type of a successful
 * fetch. Knowing the MIME type in advance allows the styler to select the best-
 * supported format from the list of URLs with equivalent content.
 * <p>----------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public interface ExternalGraphic {

   /**
    * the Format sub-element identifies the expected document MIME type of a
    * successful fetch.
    * @return Format of the external graphic
    */
    String getFormat();

   /**
    * the Format sub-element identifies the expected document MIME type of a
    * successful fetch. 
    * This method sets the format of an ExternalGraphic.
    * @param format Format of the external graphic
    */
    void setFormat(String format);
    
   /**
    *  The OnlineResource gives the URL of the external graphic
    *  @return URL of the external graphic
    */
    URL getOnlineResource();
    
   /**
    *  The OnlineResource gives the URL of the external graphic
    *  This method sets the OnlineRessource of an ExternalGraphic.
    *  @param onlineResource URL of the external graphic
    */
    void setOnlineResource(URL onlineResource);
    
   /**
    * returns the external graphic as an image. this method is not part
    * of the sld specifications but it is added for speed up applications
    * @return the external graphic as BufferedImage
    */ 
    BufferedImage getAsImage();   
    
   /**
    * sets the external graphic as an image.
    * @param image the external graphic as BufferedImage 
    */     
    void setAsImage(BufferedImage image); 
}
