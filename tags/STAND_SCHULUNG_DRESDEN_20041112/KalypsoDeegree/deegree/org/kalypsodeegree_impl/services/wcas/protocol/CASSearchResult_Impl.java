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

import org.deegree.services.wcas.protocol.CASSearchResult;

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
final public class CASSearchResult_Impl implements CASSearchResult
{
  private ArrayList searchParameter = null;

  private Object resultData = null;

  private String elementSetName = null;

  private String schemaName = null;

  private boolean success = true;

  private int numberOfRecords = 0;

  /**
   * Creates a new CASSearchResult_Impl object.
   * 
   * @param searchParameter
   * @param resultData
   * @param elementSetName
   * @param success
   * @param numberOfRecords
   * @param schemaName
   */
  CASSearchResult_Impl( String[] searchParameter, Object resultData, String elementSetName,
      boolean success, int numberOfRecords, String schemaName )
  {
    this.searchParameter = new ArrayList();
    setSearchParameter( searchParameter );
    setResultData( resultData );
    setElementSetName( elementSetName );
    setSuccess( success );
    setNumberOfRecords( numberOfRecords );
    setSchema( schemaName );
  }

  /**
   * returns the search parameters that leads to this result.
   */
  public String[] getSearchParameter()
  {
    return (String[])searchParameter.toArray( new String[searchParameter.size()] );
  }

  /**
   * @see #getSearchParameter()
   */
  public void setSearchParameter( String[] searchParameter )
  {
    this.searchParameter.clear();

    if( searchParameter != null )
    {
      for( int i = 0; i < searchParameter.length; i++ )
      {
        addSearchParameter( searchParameter[i] );
      }
    }
  }

  /**
   * @see #getSearchParameter()
   */
  public void addSearchParameter( String searchParameter )
  {
    this.searchParameter.add( searchParameter );
  }

  /**
   * returns the reslut of the query
   */
  public Object getResultData()
  {
    return resultData;
  }

  /**
   * @see #getResultData()
   */
  public void setResultData( Object resultData )
  {
    this.resultData = resultData;
  }

  /**
   * returns the name of the element set the result data contains
   */
  public String getElementSetName()
  {
    return elementSetName;
  }

  /**
   * @see #getElementSetName()
   */
  public void setElementSetName( String elementSetName )
  {
    this.elementSetName = elementSetName;
  }

  /**
   * returns true if the request that underlays the <tt>CASSearchResult</tt>
   * has been performed successful.
   */
  public boolean isSuccessful()
  {
    return success;
  }

  /**
   * @see #isSuccessful()
   */
  public void setSuccess( boolean success )
  {
    this.success = success;
  }

  /**
   * returns the number of records contained within the <tt>CASSearchResult</tt>
   * s.
   */
  public int getNumberOfRecords()
  {
    return numberOfRecords;
  }

  /**
   * @see #getNumberOfRecords()
   */
  public void setNumberOfRecords( int numberOfRecords )
  {
    this.numberOfRecords = numberOfRecords;
  }

  /**
   * returns the name of the schema the response data are formated at.
   */
  public String getSchema()
  {
    return schemaName;
  }

  /**
   * @see #getSchema()
   */
  public void setSchema( String schemaName )
  {
    this.schemaName = schemaName;
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = this.getClass().getName() + ":\n";
    ret = "searchParameter = " + searchParameter + "\n";
    ret += ( "resultData = " + resultData + "\n" );
    ret += ( "success = " + success + "\n" );
    ret += ( "numberOfRecords = " + numberOfRecords + "\n" );
    ret += ( "schemaName = " + schemaName + "\n" );
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
 * Revision 1.1  2004/09/02 23:56:51  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 12:53:32 doemming
 * *** empty log message *** Revision 1.4 2004/05/10 08:40:58 poth no message
 * 
 * Revision 1.3 2004/02/09 08:00:13 poth no message
 * 
 * Revision 1.2 2003/04/07 07:26:20 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:33 poth no message
 * 
 * Revision 1.3 2002/08/19 15:58:21 ap no message
 * 
 * Revision 1.2 2002/08/15 10:01:24 ap no message
 * 
 * Revision 1.1 2002/08/08 07:09:42 ap no message
 *  
 */
