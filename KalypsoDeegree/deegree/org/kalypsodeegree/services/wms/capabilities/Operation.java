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
package org.deegree.services.wms.capabilities;

import org.deegree.services.capabilities.*;

/**
 * The interface defines the differt types of operations that may be performed
 * by a map server, their access addresses and formats.
 * <p>----------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 2002-03-08
 */
public interface Operation {

      int GETCAPABILITIES  = 0;
      int CAPABILITIES     = 10;
      int GETMAP           = 1;
      int MAP              = 11;
      int GETFEATUREINFO   = 2;
      int FEATUREINFO      = 12;
      int DESCRIBELAYER    = 3;
      int GETLEGENDGRAPHIC = 4;
      int GETSTYLES        = 5;
      int PUTSTYLES        = 6;
      int UNKNOWN          = -1;

      String GETCAPABILITIES_NAME  = "GetCapabilities";
      String CAPABILITIES_NAME     = "Capabilities";
      String GETMAP_NAME           = "GetMap";
      String MAP_NAME              = "Map";
      String GETFEATUREINFO_NAME   = "GetFeatureInfo";
      String FEATUREINFO_NAME      = "FeatureInfo";
      String DESCRIBELAYER_NAME    = "DescribeLayer";
      String GETLEGENDGRAPHIC_NAME = "GetLegendGraphic";
      String GETSTYLES_NAME        = "GetStyles";
      String PUTSTYLES_NAME        = "PutStyles";
      String UNKNOWN_NAME          = "Unknown";
     
    /**
     * returns the formats a operation is able to return its results
     */
     Format[] getFormats();

    /**
     * Returns the specified <tt>Format</tt> (currently as a <tt>String</tt>),
     * if the <tt>Operation</tt> supports it, else null.
     * <p>
     * @param format the name of the <tt>Operation</tt> to look u
     * @return the name of the <tt>Operation</tt>, null if it is not supported
     */
     Format getFormat (String format);

     /**
      * Adds a format to the <tt>Operation</tt>'s formats (if it is not defined
      * yet).
      * <p>
      * @param format the name of the format to add
      */
     void addFormat (Format format);
     
    /**
     * returns the available Distributed Computing Platforms (DCPs) for a operation.
     * At present, only HTTP (GET & POST) is defined.
     */
     DCPType[] getDCPTypes();

    /**
    * adds the available Distributed Computing Platforms (DCPs) for a operation.
    * At present, only HTTP (GET & POST) is defined.
    */
    void addDCPType( DCPType dCPType );
     
    /**
     * returns the operation type defnied above. If the operation isn't known
     * <tt>Operation.UNKNOWN</tt> (-1) will be returned.
     */
     int getOperationType();

    /**
     * returns the name of the operation defined above.
     */
     String getOperationName();
     
     /**
      * returns the name of the class that's responsible for handling the 
      * operation (request). Defaults are:
      * <ul>
      * <li>GetMap -> org.deegree_impl.services.wms.GetMapHandler
      * <li>GetFeatureInfo -> org.deegree_impl.services.wms.GetFeatureInfoHandler
      * </ul>
      * For GetCapabilities operation no handler is required.
      */
     String getResponsibleClass();

}
