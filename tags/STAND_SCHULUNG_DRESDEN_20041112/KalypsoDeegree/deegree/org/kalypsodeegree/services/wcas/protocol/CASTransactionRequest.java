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
 * The interface defines the access to the parts of a Transaction request. Each
 * Transaction request contains one or more maybe different operations.
 * <p>
 * --------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-04-16
 */
public interface CASTransactionRequest extends OGCWebServiceRequest
{

  /**
   * returns the id that handles the locking associated to the request
   */
  public String getLockId();

  /**
   * returns a descriptor that enables easy identifying the transaction
   */
  public String getHandle();

  /**
   * A <Transaction>element is used to define a single transaction composed of
   * zero or more <Insert>, <Update>, or <Delete>elements. An empty
   * <Transaction>request is valid but not very useful.
   * <p>
   * An operation is meant to be applied to a single feature type, but multiple
   * operations on multiple feature types can be packaged within a single
   * Transaction request.
   */
  public CASOperation[] getOperations();

}