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
package org.deegree.services.wfs.capabilities;

/**
 * This section defines the list of feature types (and operations on each
 * feature type) that are available from a web feature server. Additional
 * information, such as SRS, about each feature type is also provided.
 * <p>
 * The main purpose of the <FeatureTypeList>section is to define the list of
 * feature types that a WFS can service and define the operations that are
 * supported on each feature type.
 * 
 * <p>
 * -----------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:uzs6tr@uni-bonn.de">Axel Schaefer </a>
 * @version $Revision$ $Date$
 */
public interface FeatureTypeList
{
  /**
   * <ul>
   * <li>INSERT: The <InsertFeature>element is used to indicate that the WFS is
   * capable of creating new instances of a feature type.
   * <li>UPDATE: The <UpdateFeature>element indicates that the WFS can change
   * the existing state of a feature. <li DELETE: The <DeleteFeature>element
   * indicates that the WFS can delete or remove instances of a feature type
   * from the datastore.
   * <li>QUERY: The <QueryFeature>element indicates that the WFS is capable of
   * executing a query on a feature type.
   * <li>LOCK: The <LockFeature>element indicates that the WFS is capable of
   * locking instances of a feature type.
   * </ul>
   * <p>
   * Operations can be defined globally for all feature types or locally for
   * each specific feature type. Local <Operations>specifications take
   * precedence over global <Operations>specifications. If no operations are
   * defined anywhere, then the default operation list will include <Query>
   * only.
   */
  Operation[] getOperations();

  /**
   * returns the list of the FeatureType.
   */
  FeatureType[] getFeatureTypes();

  /**
   * returns the <tt>FeatureType</tt> that matches the submitted name. If no
   * <tt>FeatureType</tt> matches, <tt>null</tt> will be returned
   */
  FeatureType getFeatureType( String name );

}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.3  2004/10/07 14:09:07  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:57:12  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 12:45:01 doemming ***
 * empty log message *** Revision 1.2 2003/06/10 07:52:04 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:52 poth no message
 * 
 * Revision 1.6 2002/08/15 10:02:41 ap no message
 * 
 * Revision 1.5 2002/05/06 16:01:41 ap no message
 * 
 * Revision 1.4 2002/04/26 09:02:34 ap no message
 * 
 * Revision 1.2 2002/04/25 16:16:36 ap no message
 * 
 * Revision 1.1 2002/04/04 16:17:15 ap no message
 * 
 *  
 */
