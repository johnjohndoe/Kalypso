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
import org.deegree.services.wfs.filterencoding.Filter;

/**
 * A query request against a catalog service is described using the
 * &gt;GetRecord&lt; element. The getRecord request bases on the request
 * specified at the "OGC Web Services Stateless Catalog Profile" and is enhanced
 * by some parameters defined within the GDI NRW Testbed II.
 * <p>
 * --------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:uzs6tr@uni-bonn.de">Axel Schaefer </a>
 * @version $Revision$ $Date$
 */

public interface CASGetRecordRequest extends OGCWebServiceRequest
{

  /**
   * returns the query objects of the getRecord request that specifies the query
   * in detail
   */
  CASQuery[] getQuery();

  /**
   * returns a Filter object that is common to all queries definied in a
   * getRecord request
   */
  public Filter getFilter();

  /**
   * returns the maximum amount of records that shall be returned as result of a
   * getRecord request. if <tt>getMaxRecords</tt> returns a value that is
   * larger than the maxRecords defined in the capabilities of the service the
   * last one will be used.
   */
  public int getMaxRecords();

  /**
   * returns the position (e.g. row of a table) where to start the query. this
   * is a usefull parameter if you like to retrieve the result of a query in
   * block of e.g. 20 entries.
   */
  public int getStartPosition();

  /**
   * returns a number that indicates if the request is part of a cascading
   * request
   */
  public int getQueryScope();

  /**
   * @see CASGetRecordRequest#getQueryScope() because the queryScope changes (it
   *      is lowered) if the request is forced to the the next server at the
   *      cascade it have to be changable.
   */
  public void setQueryScope( int queryScope );

  /**
   * returns the name of the format the result of the request have to be
   * formatted (default = XML)
   */
  public String getOutputFormat();

  /**
   * returns the record type the result shall be formated in. a catalog may
   * offers different metadata formats for different kind of metadata (service-
   * versus data-metadata) or different formats for the same kind of metadata
   * (e.g. different profils on ISO19115).
   */
  public String getOutputRecType();

}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.3  2004/10/07 14:09:04  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:57:04  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 12:45:01 doemming
 * *** empty log message *** Revision 1.2 2004/03/29 10:34:32 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:55 poth no message
 * 
 * Revision 1.4 2002/08/19 15:57:11 ap no message
 * 
 * Revision 1.3 2002/08/08 07:08:57 ap no message
 * 
 * Revision 1.2 2002/08/05 16:13:11 ap no message
 *  
 */
