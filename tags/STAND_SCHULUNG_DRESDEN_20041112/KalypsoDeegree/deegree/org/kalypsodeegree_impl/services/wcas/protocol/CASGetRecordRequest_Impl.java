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
package org.deegree_impl.services.wcas.protocol;

import java.util.ArrayList;
import java.util.HashMap;

import org.deegree.services.wcas.protocol.CASGetRecordRequest;
import org.deegree.services.wcas.protocol.CASQuery;
import org.deegree.services.wfs.filterencoding.Filter;
import org.deegree_impl.services.OGCWebServiceRequest_Impl;

/**
 * A query request against a catalog service is described using the
 * &gt;GetRecord&lt; element. The getRecord request bases on the request
 * specified at the "OGC Web Services Stateless Catalog Profile" and is enhanced
 * by some parameters defined within the GDI NRW Testbed II.
 * <p>
 * --------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
final public class CASGetRecordRequest_Impl extends OGCWebServiceRequest_Impl implements
    CASGetRecordRequest
{
  private ArrayList queries = null;

  private Filter filter = null;

  private String outputFormat = null;

  private String outputRecType = null;

  private int maxRecords = 0;

  private int queryScope = 0;

  private int startPosition = 0;

  /**
   * Creates a new CASGetRecordRequest_Impl object.
   * 
   * @param version
   * @param id
   * @param vendorSpecificParameter
   * @param maxRecords
   * @param startPosition
   * @param outputFormat
   * @param outputRecType
   * @param queries
   * @param queryScope
   * @param filter
   */
  CASGetRecordRequest_Impl( String version, String id, HashMap vendorSpecificParameter,
      int maxRecords, int startPosition, String outputFormat, String outputRecType,
      CASQuery[] queries, int queryScope, Filter filter )
  {
    super( "GetRecord", "WCAS", version, id, vendorSpecificParameter );
    this.queries = new ArrayList();
    setMaxRecords( maxRecords );
    setStartPosition( startPosition );
    setOutputFormat( outputFormat );
    setOutputRecType( outputRecType );
    setQuery( queries );
    setQueryScope( queryScope );
    setFilter( filter );
  }

  /**
   * returns the query objects of the getRecord request that specifies the query
   * in detail
   */
  public CASQuery[] getQuery()
  {
    return (CASQuery[])queries.toArray( new CASQuery[queries.size()] );
  }

  /**
   * @see CASGetRecordRequest_Impl#getQuery()
   */
  public void setQuery( CASQuery[] queries )
  {
    this.queries.clear();

    if( queries != null )
    {
      for( int i = 0; i < queries.length; i++ )
      {
        addQuery( queries[i] );
      }
    }
  }

  /**
   * @see CASGetRecordRequest_Impl#getQuery()
   */
  public void addQuery( CASQuery query )
  {
    queries.add( query );
  }

  /**
   * returns a Filter object that is common to all queries definied in a
   * getRecord request
   */
  public Filter getFilter()
  {
    return filter;
  }

  /**
   * 
   * 
   * @param filter
   */
  public void setFilter( Filter filter )
  {
    this.filter = filter;
  }

  /**
   * returns the maximum amount of records that shall be returned as result of a
   * getRecord request. if <tt>getMaxRecords</tt> returns a value that is
   * larger than the maxRecords defined in the capabilities of the service the
   * last one will be used.
   */
  public int getMaxRecords()
  {
    return maxRecords;
  }

  /**
   * @see CASGetRecordRequest_Impl#getMaxRecords()
   */
  public void setMaxRecords( int maxRecords )
  {
    this.maxRecords = maxRecords;
  }

  /**
   * returns the position (e.g. row of a table) where to start the query. this
   * is a usefull parameter if you like to retrieve the result of a query in
   * block of e.g. 20 entries.
   */
  public int getStartPosition()
  {
    return startPosition;
  }

  /**
   * @see CASGetRecordRequest_Impl#getStartPosition()
   */
  public void setStartPosition( int startPosition )
  {
    this.startPosition = startPosition;
  }

  /**
   * returns a number that indicates if the request is part of a cascading
   * request
   */
  public int getQueryScope()
  {
    return queryScope;
  }

  /**
   * @see CASGetRecordRequest_Impl#getQueryScope() because the queryScope
   *      changes (it is lowered) if the request is forced to the the next
   *      server at the cascade it have to be changable.
   */
  public void setQueryScope( int queryScope )
  {
    this.queryScope = queryScope;
  }

  /**
   * returns the name of the format the result of the request have to be
   * formatted (default = XML)
   */
  public String getOutputFormat()
  {
    return outputFormat;
  }

  /**
   * @see CASGetRecordRequest_Impl#getOutputFormat()
   */
  public void setOutputFormat( String outputFormat )
  {
    this.outputFormat = outputFormat;
  }

  /**
   * returns the record type the result shall be formated in. a catalog may
   * offers different metadata formats for different kind of metadata (service-
   * versus data-metadata) or different formats for the same kind of metadata
   * (e.g. different profils on ISO19115).
   */
  public String getOutputRecType()
  {
    return outputRecType;
  }

  /**
   * @see CASGetRecordRequest_Impl#getOutputRecType()
   */
  public void setOutputRecType( String outputRecType )
  {
    this.outputRecType = outputRecType;
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = this.getClass().getName() + ":\n";
    ret += ( "maxRecords: " + maxRecords + "\n" );
    ret += ( "startPosition: " + startPosition + "\n" );
    ret += ( "outputFormat: " + outputFormat + "\n" );
    ret += ( "outputRecType: " + outputRecType + "\n" );
    ret += ( "queries: " + queries + "\n" );
    ret += ( "queryScope: " + queryScope + "\n" );
    ret += ( "filter: " + filter + "\n" );
    return ret;
  }
}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.3  2004/10/07 14:09:04  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:56:52  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 12:53:32
 * doemming *** empty log message *** Revision 1.4 2004/03/29 10:39:04 poth no
 * message
 * 
 * Revision 1.3 2004/01/08 09:50:23 poth no message
 * 
 * Revision 1.2 2003/04/07 07:26:14 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:32 poth no message
 * 
 * Revision 1.4 2002/08/19 15:58:21 ap no message
 * 
 * Revision 1.3 2002/08/15 10:01:24 ap no message
 * 
 * Revision 1.2 2002/08/08 07:09:42 ap no message
 * 
 * Revision 1.2 2002/08/05 16:13:11 ap no message
 *  
 */
