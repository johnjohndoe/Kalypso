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
package org.deegree.services.wfs;

import org.deegree.services.Handler;
import org.deegree.services.wfs.protocol.WFSDescribeFeatureTypeRequest;
import org.deegree.services.wfs.protocol.WFSGetFeatureRequest;
import org.deegree.services.wfs.protocol.WFSGetFeatureWithLockRequest;
import org.deegree.services.wfs.protocol.WFSLockFeatureRequest;
import org.deegree.services.wfs.protocol.WFSTransactionRequest;

/**
 * Interface describing the access to a data store that encapsulates the access
 * to concrete data sources like databases or files:
 * 
 * <p>
 * ---------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface DataStore extends Handler
{

  /**
   * registers a feature type to the data store. So the data shall be able to
   * handle operations on the feature type.
   */
  void registerFeatureType( String featureType );

  /**
   * the inverse operation to <tt>registerFeatureType</tt>
   * 
   * @see registerFeatureType
   */
  void removeFeatureType( String featureType );

  /**
   * returns true if the submitted feature type is known by the data store
   */
  public boolean isKnownFeatureType( String featureType );

  /**
   * returns the describtion of one or more feature types
   * 
   * @param request
   *          conainting the list of feature types that should be described
   */
  void describeFeatureType( WFSDescribeFeatureTypeRequest request );

  /**
   * returns the features that matches the submitted request
   * 
   * @param request
   *          containing the request for zero, one or more features. The
   *          request, may contains a filter that describes the request more
   *          detailed
   */
  void getFeature( WFSGetFeatureRequest request );

  /**
   * same as <tt>describeFeatureType(..)</tt> but locking the feature during
   * processing.
   * 
   * @see #describeFeatureType
   * 
   * @param request
   *          containing the request for zero, one or more features. The
   *          request, may contains a filter that describes the request more
   *          detailed
   */
  void getFeatureWithLock( WFSGetFeatureWithLockRequest request );

  /**
   * performs a transaction against the data store. This could be an update, an
   * insert or a delete of one or more features.
   * 
   * @param request
   *          containing the transaction instruction(s)
   */
  void transaction( WFSTransactionRequest request );

  /**
   * performs the locking/unlocking of one or more features.
   * 
   * @param request
   *          the features that should be (un)locked
   */
  void lockFeature( WFSLockFeatureRequest request );
}