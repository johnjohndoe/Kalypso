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

import java.util.HashMap;

/**
 * describes the mapping of a feature type to one or more tables
 * of a datasource
 *
 * <p>---------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public interface FeatureType {
    
    public static final int GEOMETRY = java.sql.Types.VARCHAR+10000;
    public static final int UNKNOWN  = java.sql.Types.VARCHAR+10001;
    

   /**
    * returns the name of the feature type
    */
    String getName();

   /**
    * returns an array of objects that describes the output formatting for the
    * feature type. at least one 
    */
    OutputFormat[] getOutputFormat();
    
   /**
    * returns a named output format. if no output format with the submitted name
    * is known <tt>null</tt> will be returned
    */ 
    OutputFormat getOutputFormat(String name);
    
    /**
     * returns all mappings with property names as keys
     */
    HashMap getMappings();

   /**
    * returns the names of the datastore fields a property of the
    * feature type will be mapped to
    */
    String[] getDatastoreField(String property);
    
    /**
     * returns the alieas names of the datastore fields assigned to a property
     */
    String[] getAlias(String propetry);
    
    /**
    * returns the names of the property a datastore field (alias) will be mapped to
    */
    String getPropertyFromAlias(String alias);
    
    /**
    * returns the names of the property a datastore field will be mapped to
    */
    String getProperty(String datastoreField);    
    
     
   /**
    * returns the type of a named datastore fields The types are encoded like defined
    * at <tt>java.sql.types</tt>
    */ 
    int getDatastoreFieldType(String datastoreField);        

    /**
     * Checks if a Property (XPath-expression or mapping) is known to the
     * datastore. XPath expressions (e.g. "/TABLE/FIELD") are only valid,
     * if no mappings are defined at all. Properties may be used if they are
     * explicitly defined in the datastore configuration.
     * <p>
     * @param property Property to be looked up
     * @return true, if it is known, else false
     */
    public boolean isPropertyKnown (String property);
    
    /**
     * returns the type of the property a datastore field will be mapped to
     */
    int getPropertyType(String property);

   /**
    * returns the description of the datastores master table
    */
    MasterTable getMasterTable();

   /**
    * returns a description of all tables that are related direct or
    * indirect to the master table. if no related tables are defined
    * the method returns a zero length array
    */
    RelatedTable[] getRelatedTables();   
    
    /**
     * returns the name of the table where the submitted table (name) is
     * referenced by. If the name of the master table is submitted, <tt>null</tt>
     * will be returned because it isn't referenced by a related table.
     */
    String getReferencedBy(String name);
    
   /**
    * returns a description object for the table identified by the submitted name.
    * if no table with the submitted can be found <tt>null</tt> will be returned.
    */ 
    TableDescription getTableByName(String name);

   /**
    * returns the name of the coordinated reference system the geo spatial
    * data are stored. 
    */
    String getCRS();
    
    /**
     * returns the name of the coordinated reference system as it is used within
     * the data source. This code may differs from the EPSG code stored in 
     * element <CRS> 
     */
    String getInternalCRS();
    
    /**
     * returns the list (in correct order) of tables that are used to reach
     * the submitted property
     */
    TableDescription[] getPath(String property);

}
 