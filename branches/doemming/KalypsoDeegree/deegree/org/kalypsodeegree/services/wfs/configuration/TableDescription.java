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
 * the interface describes a table that contains data that are part
 * of a feature type
 *
 * <p>---------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public interface TableDescription {
    
    /**
     * returns the name of the table. in the case that a datastore
     * encapsulates an esri shapefile this is the name of the shape
     * without extension
     */
    String getName();
    
    /**
     * returns the name the table shall be mapped to at a feature (type)
     */
    String getTargetName();
    
    /**
     * returns the name of the table's field that shall be interpreted
     * as ID
     */
    String getIdField();
    
    /**
     * returns true if the id field is a number data type
     */
    boolean isIdFieldNumber();
    
    /**
     * returns true if the value of the id will be set automaticly by
     * a database
     */
    boolean isIdFieldAutoIncremented();
    
    /**
     * returns true if the submitted field is a reference to another
     * table
     */
    boolean isReference(String datastoreField);
    
    /**
     * returns an object that describes the reference to another
     * table
     */
    Reference[] getReferences(String datastoreField);
    
    /**
     * returns all references contained within the table. an implementation
     * have to ensure that if no references are contained a zero length
     * will be returned
     */
    Reference[] getReferences();
    
    /**
     * returns true if the submitted field shall be interpreted as field
     * that contains geo spatial data. only needed for GMLDB and POINTDB
     */
    boolean isGeoFieldIdentifier(String datastoreIdentifier);
    
    /**
     * return an object that describes a field that contains geo spatial
     * data.
     */
    GeoFieldIdentifier getGeoFieldIdentifier(String datastoreField);
    
    /**
     * returns all describtions for all fields that contains geo spatial data.
     * an implementation have to ensure that if no references are contained a
     * zero length will be returned
     */
    GeoFieldIdentifier[] getGeoFieldIdentifier();
    
    /**
     * returns true if inserts into the table are allowed
     */
    boolean isInsertAllowed();
    
    /**
     * returns true if updates of the tables rows are allowed
     */
    boolean isUpdateAllowed();
    
    /**
     * returns true if deleting rows from the tables are allowed
     */
    boolean isDeleteAllowed();
    
}
