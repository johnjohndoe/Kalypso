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

import java.sql.Timestamp;
import java.util.ArrayList;

import org.deegree.services.wcas.protocol.CASSearchResponse;
import org.deegree.services.wcas.protocol.CASSearchResult;

/**
 * The interface describes the access to the result of a GetRecord request.
 * Notice, because of GetRecord request may addresses different setNames (Full,
 * Summary or Brief) by different queries each result to a query capsulated by a
 * <tt>CASSearchResult</tt>.
 * <p>
 * --------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-04-16
 */
final public class CASSearchResponse_Impl implements CASSearchResponse
{
  private ArrayList searchResults = null;

  private Timestamp timestamp = null;

  private boolean success = true;

  private int numberOfRecords = 0;

  /**
   * Creates a new CASSearchResponse_Impl object.
   * 
   * @param numberOfRecords
   * @param searchResults
   * @param timestamp
   * @param success
   */
  CASSearchResponse_Impl( int numberOfRecords, CASSearchResult[] searchResults,
      Timestamp timestamp, boolean success )
  {
    this.searchResults = new ArrayList();
    setNumberOfRecords( numberOfRecords );
    setSearchResults( searchResults );
    setTimeStamp( timestamp );
    setSuccess( success );
  }

  /**
   * returns true if the request that underlays the response has been performed
   * successful.
   */
  public boolean isSuccessful()
  {
    return success;
  }

  /**
   * @see isSuccessful
   */
  public void setSuccess( boolean success )
  {
    this.success = success;
  }

  /**
   * returns the number of records contained within all <tt>CASSearchResult</tt>
   * s.
   */
  public int getNumberOfRecords()
  {
    return numberOfRecords;
  }

  /**
   * @see getNumberOfRecords
   */
  public void setNumberOfRecords( int numberOfRecords )
  {
    this.numberOfRecords = numberOfRecords;
  }

  /**
   * returns the timestamp when the performing of the request has been finished.
   */
  public Timestamp getTimeStamp()
  {
    return timestamp;
  }

  /**
   * @see getTimeStamp
   */
  public void setTimeStamp( Timestamp timestamp )
  {
    this.timestamp = timestamp;
  }

  /**
   * returns the element 'searchResults'
   */
  public CASSearchResult[] getSearchResults()
  {
    CASSearchResult[] tmp = new CASSearchResult[searchResults.size()];
    return (CASSearchResult[])searchResults.toArray( tmp );
  }

  /**
   * @see getSearchResults
   */
  public void setSearchResults( CASSearchResult[] searchResults )
  {
    this.searchResults.clear();

    if( searchResults != null )
    {
      for( int i = 0; i < searchResults.length; i++ )
      {
        addSearchResult( searchResults[i] );
      }
    }
  }

  /**
   * @see getSearchResults
   */
  public void addSearchResult( CASSearchResult searchResult )
  {
    searchResults.add( searchResult );
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = this.getClass().getName() + ":\n";
    ret = "numberOfRecords = " + numberOfRecords + "\n";
    ret += ( "searchResults = " + searchResults + "\n" );
    ret += ( "timestamp = " + timestamp + "\n" );
    ret += ( "success = " + success + "\n" );
    return ret;
  }
}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.2  2004/08/30 00:36:48  doemming
 * *** empty log message ***
 * Revision 1.1.1.1 2004/05/11 16:43:26
 * doemming backup of local modified deegree sources
 * 
 * Revision 1.4 2004/02/09 08:00:03 poth no message
 * 
 * Revision 1.3 2004/01/08 09:50:23 poth no message
 * 
 * Revision 1.2 2003/04/07 07:26:19 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:32 poth no message
 * 
 * Revision 1.4 2002/08/19 15:58:21 ap no message
 * 
 * Revision 1.3 2002/08/15 10:01:24 ap no message
 * 
 * Revision 1.2 2002/08/08 07:09:42 ap no message
 * 
 *  
 */
