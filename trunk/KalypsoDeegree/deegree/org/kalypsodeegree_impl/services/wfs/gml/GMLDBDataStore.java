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
package org.deegree_impl.services.wfs.gml;

import java.net.URL;

import org.deegree.services.wfs.DataStoreException;
import org.deegree.services.wfs.protocol.WFSDescribeFeatureTypeRequest;
import org.deegree.services.wfs.protocol.WFSGetFeatureRequest;
import org.deegree.services.wfs.protocol.WFSGetFeatureWithLockRequest;
import org.deegree.services.wfs.protocol.WFSLockFeatureRequest;
import org.deegree.services.wfs.protocol.WFSTransactionRequest;
import org.deegree_impl.services.wfs.AbstractDataStore;

/**
 * 
 * The class concretes the implementation of a data store object for accessing
 * simple databases containing just point geometries (and none geometric
 * properties) through the WFS interface.
 * 
 * <p>
 * -------------------------------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */
final public class GMLDBDataStore extends AbstractDataStore
{
  /**
   * Creates a new GMLDBDataStore object.
   * 
   * @param config
   * 
   * @throws DataStoreException
   */
  public GMLDBDataStore( URL config ) throws DataStoreException
  {
    super( config );
  }

  /**
   * returns the describtion of one or more feature types
   * 
   * @param request
   *          conainting the list of feature types that should be described
   */
  public void describeFeatureType( WFSDescribeFeatureTypeRequest request )
  {
    Thread thread = new CDescribeFeatureType( this, request );
    thread.start();
  }

  /**
   * returns the features that matches the submitted request
   * 
   * @param request
   *          containing the request for zero, one or more features. The
   *          request, may contains a filter that describes the request more
   *          detailed
   */
  public void getFeature( WFSGetFeatureRequest request )
  {
    Thread thread = new CGetFeature( this, request );
    thread.start();
  }

  /**
   * same as <tt>describeFeatureType(..)</tt> but locking the feature during
   * processing.
   * 
   * @see #describeFeatureType(WFSDescribeFeatureTypeRequest)
   * 
   * @param request
   *          containing the request for zero, one or more features. The
   *          request, may contains a filter that describes the request more
   *          detailed
   */
  public void getFeatureWithLock( WFSGetFeatureWithLockRequest request )
  {
    Thread thread = new CGetFeatureWithLock( this, request );
    thread.start();
  }

  /**
   * performs a transaction against the data store. This could be an update, an
   * insert or a delete of one or more features.
   * 
   * @param request
   *          containing the transaction instruction(s)
   */
  public void transaction( WFSTransactionRequest request )
  {
    Thread thread = new CTransaction( this, request );
    thread.start();
  }

  /**
   * performs the locking/unlocking of one or more features.
   * 
   * @param request
   *          the features that should be (un)locked
   */
  public void lockFeature( WFSLockFeatureRequest request )
  {
    Thread thread = new CLockFeature( this, request );
    thread.start();
  }
}