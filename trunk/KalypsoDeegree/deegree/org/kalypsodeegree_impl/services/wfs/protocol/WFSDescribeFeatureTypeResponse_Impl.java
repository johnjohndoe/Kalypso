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
package org.deegree_impl.services.wfs.protocol;

import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.wfs.protocol.WFSDescribeFeatureTypeResponse;
import org.deegree.xml.DOMPrinter;
import org.w3c.dom.Document;

/**
 * In response to a DescribeFeatureType request, where the output format has
 * been specified as XMLSCHEMA. A WFS may support different formats for
 * formatting its responses. Only GML is mandatory.
 * 
 * <p>
 * --------------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version $Revision$ $Date$
 */
class WFSDescribeFeatureTypeResponse_Impl extends WFSBasicResponse_Impl implements
    WFSDescribeFeatureTypeResponse
{
  private Document featureTypeSchema = null;

  /**
   * constructor initializing the class with the
   * <WFSDescribeFeatureTypeResponse_Impl >
   */
  WFSDescribeFeatureTypeResponse_Impl( OGCWebServiceRequest request, String[] affectedFeatureTypes,
      Document exception, Document featureTypeSchema )
  {
    super( request, exception, affectedFeatureTypes );
    setFeatureTypeSchema( featureTypeSchema );
  }

  /**
   * returns the respose of a DescribeFeatureTypeRequest as XML-Schema
   */
  public Document getFeatureTypeSchema()
  {
    return featureTypeSchema;
  }

  /**
   * sets the respose of a DescribeFeatureTypeRequest as XML-Schema
   */
  public void setFeatureTypeSchema( Document featureTypeSchema )
  {
    this.featureTypeSchema = featureTypeSchema;
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = this.getClass().getName() + ":\n";
    ret += ( "featureTypeSchema: " + DOMPrinter.nodeToString( featureTypeSchema, "UTF-8" ) + "\n" );
    return ret;
  }
}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.3  2004/10/07 14:09:12  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:56:58  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31
 * 12:53:32 doemming *** empty log message *** Revision 1.3 2004/03/12 15:56:49
 * poth no message
 * 
 * Revision 1.2 2003/04/07 07:26:54 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:26 poth no message
 * 
 * Revision 1.8 2002/08/15 10:01:40 ap no message
 * 
 * Revision 1.7 2002/08/09 15:36:30 ap no message
 * 
 * Revision 1.6 2002/05/14 14:39:51 ap no message
 * 
 * Revision 1.5 2002/05/13 16:11:02 ap no message
 * 
 * Revision 1.4 2002/05/06 16:02:22 ap no message
 * 
 * Revision 1.3 2002/04/26 09:05:36 ap no message
 * 
 * Revision 1.1 2002/04/04 16:17:15 ap no message
 *  
 */
