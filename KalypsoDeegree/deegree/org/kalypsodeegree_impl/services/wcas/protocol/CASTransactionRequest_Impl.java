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

import org.deegree.services.wcas.protocol.CASOperation;
import org.deegree.services.wcas.protocol.CASTransactionRequest;
import org.deegree_impl.services.OGCWebServiceRequest_Impl;

/**
 * The interface defines the access to the parts of a Transaction request. Each
 * Transaction request contains one or more maybe different operations.
 * <p>
 * --------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-04-16
 */
final public class CASTransactionRequest_Impl extends OGCWebServiceRequest_Impl implements
    CASTransactionRequest
{
  private ArrayList operations = null;

  private String handle = null;

  private String lockId = null;

  /**
   * Creates a new CASTransactionRequest_Impl object.
   * 
   * @param version
   * @param id
   * @param vendorSpecificParameter
   * @param lockId
   * @param handle
   * @param operations
   */
  CASTransactionRequest_Impl( String version, String id, HashMap vendorSpecificParameter,
      String lockId, String handle, CASOperation[] operations )
  {
    super( "TransactionRequest", "WCAS", version, id, vendorSpecificParameter );
    this.operations = new ArrayList();
    setLockId( lockId );
    setHandle( handle );
    setOperations( operations );
  }

  /**
   * returns the id that handles the locking associated to the request
   */
  public String getLockId()
  {
    return lockId;
  }

  /**
   * @see #getLockId()
   */
  public void setLockId( String lockId )
  {
    this.lockId = lockId;
  }

  /**
   * returns a descriptor that enables easy identifying the transaction
   */
  public String getHandle()
  {
    return handle;
  }

  /**
   * @see #getHandle()
   */
  public void setHandle( String handle )
  {
    this.handle = handle;
  }

  /**
   * A <Transaction>element is used to define a single transaction composed of
   * zero or more <Insert>, <Update>, or <Delete>elements. An empty
   * <Transaction>request is valid but not very useful.
   * <p>
   * An operation is meant to be applied to a single feature type, but multiple
   * operations on multiple feature types can be packaged within a single
   * Transaction request.
   */
  public CASOperation[] getOperations()
  {
    CASOperation[] tmp = new CASOperation[operations.size()];
    return (CASOperation[])operations.toArray( tmp );
  }

  /**
   * @see #getOperations()
   */
  public void setOperations( CASOperation[] operations )
  {
    this.operations.clear();

    if( operations != null )
    {
      for( int i = 0; i < operations.length; i++ )
      {
        addOperation( operations[i] );
      }
    }
  }

  /**
   * @see #getOperations()
   */
  public void addOperation( CASOperation operation )
  {
    operations.add( operation );
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = this.getClass().getName() + ":\n";
    ret = "lockId = " + lockId + "\n";
    ret += ( "handle = " + handle + "\n" );
    ret += ( "operations = " + operations + "\n" );
    return ret;
  }
}