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
import java.sql.Types;
import java.util.Properties;

import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.table.Table;
import org.deegree.model.table.TableException;
import org.deegree_impl.model.geometry.PostGISAdapter;
import org.deegree_impl.model.table.Table_Impl;
import org.postgis.PGgeometry;


/**
 * 
 *
 * @version $Revision$
 * @author <a href="mailto:wanhoff@giub.uni-bonn.de">Jeronimo Wanhoff</a>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class PostgisDBAccess extends DBAccess {
    
    
    /**
     * constructor
     *
     * @param con connection to a PostGIS database
     */
    public PostgisDBAccess(Connection con) {
        super( con );

        ( (org.postgresql.PGConnection)con ).addDataType( "geometry", "org.postgis.PGgeometry" );
        ( (org.postgresql.PGConnection)con ).addDataType( "box3d", "org.postgis.PGbox3d" );
    }

    /**
     * constructor
     *
     * @param con connection to a postgis database with spatial extension.
     */
    public PostgisDBAccess(Connection con,  boolean autoCommit) throws SQLException {
        this( con );
        con.setAutoCommit( autoCommit );
    }

    
    /**
     * @param driver jdbc driver 
     * @param logon address of the database (additional parameters)
     * @param user
     * @param password
     * @throws SQLException
     * @throws Exception
     */
    public PostgisDBAccess( String driver,  String logon,  String user,
    					    String password) throws SQLException, Exception {
        super( driver, logon, user, password );
        ( (org.postgresql.PGConnection)con ).addDataType( "geometry", "org.postgis.PGgeometry" );
        ( (org.postgresql.PGConnection)con ).addDataType( "box3d", "org.postgis.PGbox3d" );
    }

    
    /**
     * @param driver jdbc driver 
     * @param logon address of the database (additional parameters)
     * @param properties additional parameters
     * @throws SQLException
     * @throws Exception
     */
    public PostgisDBAccess( String driver,  String logon,  Properties properties) 
												throws SQLException, Exception {
        super( driver, logon, properties );
        ( (org.postgresql.PGConnection)con ).addDataType( "geometry", "org.postgis.PGgeometry" );
        ( (org.postgresql.PGConnection)con ).addDataType( "box3d", "org.postgis.PGbox3d" );
    }

    
    /**
     * @param driver jdbc driver 
     * @param logon address of the database (additional parameters)
     * @param user
     * @param password
     * @param autoCommit
     * @throws SQLException
     * @throws Exception
     */
    public PostgisDBAccess( String driver,  String logon,  String user,  
    						String password,  boolean autoCommit) throws SQLException, Exception {
        this( driver, logon, user, password );
        con.setAutoCommit( autoCommit );
    }

    
    /**
     * @param driver jdbc driver 
     * @param logon address of the database (additional parameters)
     * @param properties additional parameters
     * @param autoCommit
     * @throws SQLException
     * @throws Exception
     */
    public PostgisDBAccess( String driver,  String logon,  Properties properties,  
    						boolean autoCommit) throws SQLException, Exception {
        this( driver, logon, properties );
        con.setAutoCommit( autoCommit );
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
            columnTypes[i] = mapTypes( rsmd.getColumnType( i + 1 ) );
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

            PGgeometry geom = null;

            if ( ii >= startFeature ) {
                Object[] objArray = new Object[columns];

                for ( int i = 0; i < columns; ++i ) {
                    Object obj = pgrs.getObject( i + 1 );

                    if ( obj instanceof PGgeometry ) {
                        geom = (PGgeometry)obj;

                        GM_Object dgeom = PostGISAdapter.wrap( geom, null );
                        objArray[i] = dgeom;
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
     * @param type 
     *
     * @return 
     */
    protected String mapTypes( int type ) {
        String clname = null;

        switch ( type ) {
            case Types.CHAR:
                clname = "java.lang.String";
                break;
            case Types.VARCHAR:
                clname = "java.lang.String";
                break;
            case Types.LONGVARCHAR:
                clname = "java.lang.String";
                break;
            case Types.DATE:
                clname = "java.sql.Date";
                break;
            case Types.FLOAT:
                clname = "java.lang.Float";
                break;
            case Types.DOUBLE:
                clname = "java.lang.Double";
                break;
            case Types.INTEGER:
                clname = "java.lang.Integer";
                break;
            case Types.SMALLINT:
                clname = "java.lang.Short";
                break;
            case Types.TINYINT:
                clname = "java.lang.Byte";
                break;
            case Types.NUMERIC:
                clname = "java.math.BigDecimal";
                break;
            case Types.STRUCT:
                clname = "java.io.ByteArrayOutputStream";
                break;
            case Types.ARRAY:
                clname = "java.sql.Array";
                break;
            case Types.BLOB:
                clname = "java.io.InputStream";
                break;
            case Types.CLOB:
                clname = "java.io.InputStream";
                break;
            case 1111:
                clname = "org.deegree.model.geometry.GM_Object";
                break;
            default:
                clname = "java.lang.Object";
                break;
        }

        return clname;
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