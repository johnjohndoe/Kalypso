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

import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.wcas.protocol.CASInsertResult;
import org.deegree.services.wcas.protocol.CASTransactionResponse;
import org.deegree_impl.services.OGCWebServiceResponse_Impl;
import org.w3c.dom.Document;

/**
 * 
 * <p>
 * --------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-04-16
 */
final public class CASTransactionResponse_Impl extends OGCWebServiceResponse_Impl implements
    CASTransactionResponse
{
  private ArrayList insertResults = null;

  private String handle = null;

  private String locator = null;

  private String message = null;

  private String status = null;

  /**
   * Creates a new CASTransactionResponse_Impl object.
   * 
   * @param request
   * @param exception
   * @param insertResults
   * @param status
   * @param handle
   */
  CASTransactionResponse_Impl( OGCWebServiceRequest request, Document exception,
      CASInsertResult[] insertResults, String status, String handle )
  {
    super( request, exception );
    this.insertResults = new ArrayList();
    setInsertResult( insertResults );
    setStatus( status );

    if( exception != null )
    {
      setMessage( null );
      setLocator( null );
    }

    setHandle( handle );
  }

  /**
   * The <InsertResult>is used to delimit one or more feature identifiers of
   * newly created features. The insert results are reported in the order in
   * which the <Insert>operations were specified in the <Transaction>request.
   * Additionally, they can be correlated using the handle attribute.
   */
  public CASInsertResult[] getInsertResult()
  {
    CASInsertResult[] is = new CASInsertResult[insertResults.size()];
    return (CASInsertResult[])insertResults.toArray( is );
  }

  /**
   * @see getInsertResult
   */
  public void addInserResult( CASInsertResult insertResult )
  {
    insertResults.add( insertResult );
  }

  /**
   * @see getInsertResult
   */
  public void setInsertResult( CASInsertResult[] insertResults )
  {
    this.insertResults.clear();

    if( insertResults != null )
    {
      for( int i = 0; i < insertResults.length; i++ )
      {
        addInserResult( insertResults[i] );
      }
    }
  }

  /**
   * A transaction can terminate with a status of:
   * <li>SUCCESS: The transaction was successfully completed.
   * <li>FAILED: One or more operations in the transaction failed.
   * <li>PARTIAL: The transaction partially succeeded and the data may be in an
   * inconsistent state. For systems that do not support atomic transactions,
   * this outcome is a distinct possibility.
   */
  public String getStatus()
  {
    return status;
  }

  /**
   * @see getStatus
   */
  public void setStatus( String status )
  {
    this.status = status;
  }

  /**
   * In the event that a transaction request fails, the <Locator>can be used to
   * indicate which part of the transaction failed. If the failed operation is
   * labeled using a handle attribute then that can be used to locate the
   * failure. Otherwise, the web feature server may try to identify the failure
   * relative to the beginning of the transaction request possibly using line
   * numbers or some other convenient mechanism.
   */
  public String getLocator()
  {
    return locator;
  }

  /**
   * @see getLocator
   */
  public void setLocator( String locator )
  {
    this.locator = locator;
  }

  /**
   * Returns any error messages.
   */
  public String getMessage()
  {
    return message;
  }

  /**
   * @see getMessage
   */
  public void setMessage( String message )
  {
    this.message = message;
  }

  /**
   * The handle attribute is included to allow a server to associate any text to
   * the response. The purpose of the handle attribute is to provide an error
   * handling mechanism for locating a statement that might fail.
   */
  public String getHandle()
  {
    return handle;
  }

  /**
   * @see getHandle
   */
  public void setHandle( String handle )
  {
    this.handle = handle;
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = this.getClass().getName() + ":\n";
    ret += ( "insertResults: " + insertResults + "\n" );
    ret += ( "status: " + status + "\n" );
    ret += ( "message: " + message + "\n" );
    ret += ( "locator: " + locator + "\n" );
    ret += ( "handle: " + handle + "\n" );
    return ret;
  }
}