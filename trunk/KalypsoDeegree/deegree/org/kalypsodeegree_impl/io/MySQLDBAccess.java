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
package org.deegree_impl.io;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.Properties;

import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.table.Table;
import org.deegree.model.table.TableException;
import org.deegree_impl.model.geometry.WKTAdapter;
import org.deegree_impl.model.table.Table_Impl;


/**
 * Class for accessing MySQL (spatial) database
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class MySQLDBAccess extends DBAccess {
    
    private Properties geoColumns = new Properties();
    
    
    /**
     * constructor
     *
     * @param con connection to a PostGIS database
     */
    public MySQLDBAccess(Connection con) {
        super( con );        
    }

    /**
     * constructor
     *
     * @param con connection to a oracle database with spatial extension.
     */
    public MySQLDBAccess(Connection con,  boolean autoCommit) throws SQLException {
        this( con );
        con.setAutoCommit( autoCommit );
    }

   
    /**
     * @param driver name of the jdbc driver
     * @param logon address of the database (and additional parameters)
     * @param user
     * @param password
     * @throws SQLException
     * @throws Exception
     */
    public MySQLDBAccess( String driver,  String logon,  String user, 
                          String password) throws SQLException, Exception {
        super( driver, logon, user, password );
    }

   
    /**
     * @param driver name of the jdbc driver
     * @param logon address of the database (and additional parameters)
     * @param properties additional parameters
     * @throws SQLException
     * @throws Exception
     */
    public MySQLDBAccess( String driver,  String logon, 
                          Properties properties) throws SQLException, Exception {
        super( driver, logon, properties );
    }

   
    /**
     * @param driver name of the jdbc driver
     * @param logon address of the database (and additional parameters)
     * @param user
     * @param password
     * @param autoCommit
     * @throws SQLException
     * @throws Exception
     */
    public MySQLDBAccess( String driver,  String logon,  String user, 
                          String password,  boolean autoCommit) 
                         throws SQLException, Exception {
        this( driver, logon, user, password );
        con.setAutoCommit( autoCommit );
    }

    
    /**
     * @param driver name of the jdbc driver
     * @param logon address of the database (and additional parameters)
     * @param properties
     * @param autoCommit
     * @throws SQLException
     * @throws Exception
     */
    public MySQLDBAccess( String driver,  String logon, 
                          Properties properties,  boolean autoCommit) 
                         throws SQLException, Exception {
        this( driver, logon, properties );
        con.setAutoCommit( autoCommit );
    }
    
    /**
     * announces the names of the columns that shall be treated as geometries.
     * This must be set because MySQL JDBC driver returnts VARCHAR as type for
     * this column.<p>
     * If <tt>null</tt> will be passed, 'ASTEXT(GEOM)' will be used as default
     *
     * @param geoColumns names of the geometry columns
     */
    public void announceGeometryColumnNames(String[] geoColumns) {
        if ( this.geoColumns == null ) {
            this.geoColumns.put( "ASTEXT(GEOM)", "ASTEXT(GEOM)" );
        } else {
            for (int i = 0; i < geoColumns.length; i++) {
                this.geoColumns.put( geoColumns[i].toUpperCase(), geoColumns[i] );
            }
        }
    }

    /**
     * performs a query against a postgis database. the returned object is an
     * instance of org.deegree.model.table.Table
     *
     * @param query sql query expression
     * @param startFeature index of the first feature (row) to return
     * @param maxFeatures maximum number of returned features (rows)
     *
     * @return 
     *
     * @throws SQLException 
     * @throws TableException 
     * @throws GM_Exception 
     */
    public Object performQuery( String query,  int startFeature,  int maxFeatures )
                        throws SQLException, TableException, GM_Exception {                   
                            
        query = escape( query );
                            
        // table containing the Result
        Table tm = null;

        //prepare the statement for query
        PreparedStatement ps = reformatStatement( query );

        // if query is limited to maxfeatures lines
        if ( maxFeatures > 0 ) {
            ps.setMaxRows( maxFeatures + startFeature );
        }

        // execute the query
        ResultSet pgrs = ps.executeQuery();

        // metadata of query's resultset
        ResultSetMetaData rsmd = pgrs.getMetaData();
        // get number of columns
        int columns = rsmd.getColumnCount();

        // for column names being placed in
        String[] columnNames = new String[columns];
        // for column types being placed in
        String[] columnTypes = new String[columns];

        // for all columns
        for ( int i = 0; i < columns; ++i ) {
            // save column name i in array position i-1
            columnNames[i] = rsmd.getColumnName( i + 1 ).toUpperCase();
            // save column type i in array position i-1
            if ( geoColumns.get( columnNames[i] ) != null ) {
                columnTypes[i] = "org.deegree.model.geometry.GM_Object";
            } else {
                columnTypes[i] = mapTypes( rsmd.getColumnType( i + 1 ) );
            }
        }

        // if a column exists
        if ( columns > 0 ) {
            // if maxfeatures is set
            if ( maxFeatures > 0 ) {
                // create a table with maxfeatures rows
                tm = new Table_Impl( "Table", columnNames, columnTypes, maxFeatures );
            } else {
                // create a table with 1000 rows
                tm = new Table_Impl( "Table", columnNames, columnTypes, 1000 );
            }
        } else {
            // if there are no columns
            return null;
        }

        int ii = -1;        
        while ( pgrs.next() ) {
            ++ii;
                    
            if ( ii >= startFeature ) {
                Object[] objArray = new Object[columns];

                for ( int i = 0; i < columns; ++i ) {
                    Object obj = pgrs.getObject( i + 1 );
                    if ( columnTypes[i].endsWith( "GM_Object" ) ) {
                        if ( obj != null ) {
                            objArray[i] = WKTAdapter.wrap( (String)obj, null );
                        }
                    } else {
                        objArray[i] = obj;
                    }   
                }

                tm.appendRow( objArray );

                if ( tm.getRowCount() == maxFeatures ) {
                    break;
                }
            }
        }

        pgrs.close();
        ps.close();        

        return tm;
    }
    
    /**
     *
     *
     * @param query 
     *
     * @return 
     *
     * @throws SQLException 
     */
    private PreparedStatement reformatStatement( String query ) throws SQLException {
        // create PreparedStatement
        PreparedStatement ps = con.prepareStatement( query );
        //return the PreparedStatement
        return ps;
    }
}