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

/**
 * The interface describes the access to a part of the result of a GetRecord
 * request. The part of a GetRecord request capsulated by a
 * <tt>CASSearchResult</tt> is defined by one of the queries defined within.
 * <p>
 * --------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-04-16
 */
public interface CASSearchResult
{

  /**
   * returns the search parameters that leads to this result.
   */
  String[] getSearchParameter();

  /**
   * returns the reslut of the query
   */
  Object getResultData();

  /**
   * returns the name of the element set the result data contains
   */
  String getElementSetName();

  /**
   * returns true if the request that underlays the <tt>CASSearchResult</tt>
   * has been performed successful.
   */
  boolean isSuccessful();

  /**
   * returns the number of records contained within the <tt>CASSearchResult</tt>
   * s.
   */
  int getNumberOfRecords();

  /**
   * returns the name of the schema the response data are formated at.
   */
  String getSchema();

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
 * Revision 1.3 2004/08/31 12:45:01 doemming ***
 * empty log message *** Revision 1.2 2004/02/09 07:57:02 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:55 poth no message
 * 
 * Revision 1.2 2002/08/19 15:57:11 ap no message
 * 
 * Revision 1.1 2002/08/08 07:08:57 ap no message
 *  
 */
