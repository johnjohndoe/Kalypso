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

import java.net.*;

import org.deegree.services.wms.capabilities.*;
import org.deegree.xml.Marshallable;
import org.deegree.xml.XMLTools;

import org.deegree_impl.tools.NetWorker;
import org.deegree_impl.ogcbasic.BaseURL_Impl;


/**
 * A Map Server may use FeatureListURL to point to a list of the features
 * represented in a Layer.
 * <p>----------------------------------------------------------------------</p>
 *
* @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider</a>
 * @version $Revision$
 */
class FeatureListURL_Impl extends BaseURL_Impl implements FeatureListURL, Marshallable {
  

    /**
     * Creates a new FeatureListURL_Impl object.
     *
     * @param format 
     * @param onlineResource 
     */
    FeatureListURL_Impl( String format, URL onlineResource ) {
        super( format, onlineResource );
    }

    /**
     * Returns an XML representation of this object.
     */
    public String exportAsXML() {
        StringBuffer sb = new StringBuffer();

        sb.append( "<FeatureListURL>" ).append( "<Format>" )
          .append( XMLTools.validateCDATA( getFormat() ) ).append( "</Format>" )
          .append( "<OnlineResource " ).append( "xmlns:xlink=\"http://www.w3.org/1999/xlink\" " )
          .append( "xlink:type=\"simple\" xlink:href=\"" )
          .append( NetWorker.url2String ( getOnlineResource ()))
          .append( "\"/>" ).append( "</FeatureListURL>" );

        return sb.toString();
    }
}