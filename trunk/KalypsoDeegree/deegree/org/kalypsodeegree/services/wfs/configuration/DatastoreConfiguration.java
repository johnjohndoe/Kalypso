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

package org.deegree.services.wfs.configuration;


/**
 * the toplevel interface describing a deegree wfs datastore 
 *
 * <p>---------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public interface DatastoreConfiguration {

    public static final int ORACLESPATIAL   = 0;
    public static final int GMLDB           = 1;
    public static final int POINTDB         = 2;
    public static final int SHAPEFILES      = 3;
    public static final int SDE             = 4;
    public static final int BNA             = 5;
    public static final int POSTGIS         = 6;
    public static final int MYSQL           = 7;
    public static final int MAPINFO         = 8;

   /**
    * returns the name of a datastore
    */
    String getName();

   /**
    * returns the type of a datastore. at the moment four types
    * are known:
    * <ul>
    *	<li>ORACLESPATIAL
    *	<li>GMLDB	
    *	<li>POINTDB
    *	<li>SHAPEFILES
    *   <li>SDE
    * </ul>
    */
    int getType();

   /**
    * returns an object that describes the connection to a database
    * if datastore type equals ORACLESPATIAL, GMLDB or POINTDB
    */	
    Connection getConnection();

   /**
    * return describing objects for each feature type that is accessible
    * through a datastore.
    */
    FeatureType[] getFeatureTypes(); 
    
   /**
    * returns the feature description for a named feature type. if no feature
    * type is known with the submitted name <tt>null</tt> will be returned
    */ 
    FeatureType getFeatureType(String name);
    
    
}

