/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree (Java Framework for Geospatial Solutions).
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
package org.deegree.services.wcas.protocol;

import org.deegree.services.OGCWebServiceRequest;

/**
 * The describe record type operation describes the architecture of a possible
 * result to a getRecord request as a XML schema. At the moment two basic
 * schemas are known: ISO19115 (by NIMA and GDI NRW) and ISO19119 (by OWS1.2 and
 * GDI NRW). The basic schemas are splitted into three subschemas (Full, Summary
 * and Brief) called setNames.
 * <p>
 * --------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-04-16
 */
public interface CASDescribeRecordTypeRequest extends OGCWebServiceRequest
{

  /**
   * returns the (catalog) types that shall be described. three catalog types
   * are known: Service, Product and Collection
   */
  String[] getTypeNames();

  /**
   * returns the set names of the record types that shall be described. each set
   * name is associated with the type name returned by <tt>getTypeNames</tt>
   * at same index position.
   */
  String[] getSetNames();

  /**
   * returns the format the result to the request will e returned. At the moment
   * only XML-schema is defined as valid output format.
   */
  String getOutputFormat();

}