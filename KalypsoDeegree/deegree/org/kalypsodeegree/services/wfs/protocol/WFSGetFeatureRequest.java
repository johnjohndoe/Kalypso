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
 * The GetFeature interface can be used to package one or more query
 * descriptions into a single request. The results of all queries packaged in a
 * GetFeature request are concatenated to produce the result set.
 * 
 * <p>
 * --------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface WFSGetFeatureRequest extends WFSBasicRequest
{
  /**
   * The outputFormat attribute defines the format to use to generate the result
   * set. Vendor specific formats, declared in the capabilities document are
   * possible. The WFS-specs implies GML as default output format.
   */
  public String getOutputFormat();

  /**
   * The handle attribute is included to allow a client to associate a mnemonic
   * name to the <Query>request. The purpose of the handle attribute is to
   * provide an error handling mechanism for locating a statement that might
   * fail.
   */
  public String getHandle();

  /**
   * The query defines which feature type to query, what properties to retrieve
   * and what constraints (spatial and non-spatial) to apply to those
   * properties.
   * <p>
   * only used for xml-coded requests
   */
  public WFSQuery[] getQuery();

  /**
   * The optional maxFeatures attribute can be used to limit the number of
   * features that a GetFeature request retrieves. Once the maxFeatures limit is
   * reached, the result set is truncated at that point.
   */
  public int getMaxFeatures();

  /**
   * The startPosition parameter identifies the first result set entry to be
   * returned specified the default is the first record
   */
  public int getStartPosition();

  /**
   * The property names is used to enumerate the feature properties or
   * attributes that should be selected. If no property names are specified then
   * all properties should be fetched.
   * <p>
   * only use for name-value-pair encoded requests
   */
  public String[] getPropertyNames();

  /**
   * A list of feature identifiers upon which the specified operation shall be
   * applied. Optional. No default.
   * <p>
   * Only used for name-value-pair encoded requests
   */
  public String[] getFeatureIds();

  /**
   * A list of feature type names to query. Optional. No default.
   * <p>
   * only used for name-value-pair encoded requests
   */
  public String[] getTypeNames();

  /**
   * A filter specification describes a set of features to operate upon. The
   * format of the filter is defined in the OGC Filter Encoding Specification.
   * Optional. No default. Prerequisite: TYPENAME
   */
  public Filter getFilter();

  /**
   * @clientCardinality 1..*
   */

  /* #WFSQuery lnkWFSQuery; */
}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.3  2004/10/07 14:09:07  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:56:55  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 12:45:00 doemming
 * *** empty log message *** Revision 1.3 2004/02/09 07:57:02 poth no message
 * 
 * Revision 1.2 2003/04/23 07:23:14 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:54 poth no message
 * 
 * Revision 1.5 2002/08/15 10:02:41 ap no message
 * 
 * Revision 1.4 2002/04/26 09:02:51 ap no message
 * 
 * Revision 1.2 2002/04/25 16:17:20 ap no message
 * 
 * Revision 1.1 2002/04/04 16:17:15 ap no message
 *  
 */
