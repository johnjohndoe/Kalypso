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

package org.deegree.services.wcas.capabilities;

import org.deegree.services.capabilities.CException;
import org.w3c.dom.Document;

/**
 * The capabilities section specifies the list of requests that the WCAS can
 * handle. Two classes of web feature servers, based on the capabilities they
 * support, are defined in the Overview
 * 
 * <p>
 * ---------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:uzs6tr@uni-bonn.de">Axel Schaefer </a>
 * @version $Revision$ $Date$
 */

public interface Capability
{

  /**
   * returns an object that contains a list of txonomies known by the catalog
   */
  TaxonomyTypeList getTaxonomyTypeList();

  /**
   * A basic catalog would include the GetCapabilities, DescribeRecordType and
   * the GetRecord interfaces. A transactional catalog would also include the
   * Transaction interface, possibly the LockRecord interface and/or the
   * GetRecordWithLock interface.
   * 
   * @return request known by a catalog
   */
  Request getRequest();

  /**
   * returns vendor specific capabilities of the wfs
   * 
   * @return vendor specific capabilities
   */
  Document getVendorSpecificCapabilities();

  /**
   *  
   */
  RecordTypeList getRecordTypeList();

  /**
   *  
   */
  PresentOptions getPresentOptions();

  /**
   * return an object that contains information about which query languages are
   * known by a catalog
   * 
   * @return query languages are known by a catalog
   */
  QueryLanguages getQueryLanguages();

  /**
   * returns a list of federated catalogs known by the catalog
   * 
   * @return federated catalogs known by the catalog
   */
  FederatedCatalog[] getFederatedCatalogs();

  /**
   * returns the format exceptions will be returned
   * 
   * @return format exceptions will be returned
   */
  CException getExceptions();
}
/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.3  2004/10/07 14:09:05  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:57:04  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 12:45:01 doemming *** empty
 * log message *** Revision 1.1.1.1 2002/09/25 16:01:56 poth no message
 * 
 * Revision 1.1 2002/08/19 15:56:59 ap no message
 * 
 * Revision 1.4 2002/04/26 09:02:34 ap no message
 * 
 * Revision 1.2 2002/04/25 16:16:36 ap no message
 * 
 * Revision 1.1 2002/04/04 16:17:15 ap no message
 * 
 * Revision 1.1 2001/10/25 13:14:21 ap no message
 * 
 * Revision 1.1 2001/10/16 07:01:30 ap no message
 * 
 * Revision 1.1 2001/10/05 05:59:45 ap no message
 * 
 * Revision 1.3 2001/09/06 15:08:34 ap no message
 * 
 * Revision 1.2 2001/08/15 10:43:58 axel as: see Open GIS Web Feature Server
 * Specification (01-023r1) Version 0.0.13 (10-MAR-2001), p. 50ff for more
 * details
 *  
 */
