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
package org.deegree.services.wfs.protocol;

import org.deegree.services.wfs.filterencoding.Filter;

/**
 * The purpose of the LockFeature interface is to expose a long term feature
 * locking mechanism to ensure consistency. The lock is considered long term
 * because network latency would make feature locks last relatively longer than
 * native commercial database locks.
 * <p>
 * The LockFeature interface is optional and need only be implemented if the
 * underlying datastore supports (or can be made to support) data locking. In
 * addition, the implementation of locking is completely opaque to the client.
 * 
 * <p>
 * --------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface WFSLock
{
  /**
   * Specify how the lock should be acquired. ALL indicates to try to get all
   * feature locks otherwise fail. SOME indicates to try to get as many feature
   * locks as possible. The default LOCKACTION is ALL.
   */
  public String getLockAction();

  /**
   * If a filter is not specified, then the optional typeName attribute can be
   * used to specify that all feature instances of a particular feature type
   * should be locked.
   */
  public String getTypeName();

  /**
   *  
   */
  public String getHandle();

  /**
   * A filter specification describes a set of features to operate upon. The
   * format of the filter is defined in the OGC Filter Encoding Specification.
   * Optional. No default. Prerequisite: TYPENAME
   */
  public Filter getFilter();

  /**
   * A list of feature identifiers upon which the specified operation shall be
   * applied. Optional. No default.
   */
  public String[] getFeatureIds();
}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.2  2004/08/30 00:36:51  doemming
 * *** empty log message ***
 * Revision 1.1.1.1 2004/05/11 16:43:22 doemming backup
 * of local modified deegree sources
 * 
 * Revision 1.3 2004/02/09 07:57:02 poth no message
 * 
 * Revision 1.2 2003/04/23 07:23:15 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:54 poth no message
 * 
 * Revision 1.4 2002/08/15 10:02:41 ap no message
 * 
 * Revision 1.3 2002/04/26 09:02:51 ap no message
 * 
 * Revision 1.1 2002/04/04 16:17:15 ap no message
 *  
 */
