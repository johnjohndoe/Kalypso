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

import java.io.ByteArrayOutputStream;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.Properties;

import oracle.jdbc.driver.OracleResultSet;
import oracle.spatial.geometry.JGeometry;
import oracle.sql.ARRAY;
import oracle.sql.ArrayDescriptor;
import oracle.sql.STRUCT;

import org.deegree.model.table.Table;
import org.deegree_impl.model.geometry.OracleAdapter;
import org.deegree_impl.model.table.Table_Impl;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.StringExtend;

/**
 * This class extends the DBAccess class to read fields from a oracle spatial
 * table. this may or may not include geometries. if the reques result contains
 * one or more geometries they can be accessed as table column(s) or as OGC
 * WKBs.
 * 
 * 
 * ------------------------------------------------------------------------
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth <a>
 * @author <a href="mailto:luigimarinucci@yahoo.com">Luigi Marinucci <a>
 * @version 5.05.2003
 */
public class OracleSpatialAccess extends DBAccess
{
  private int[] struct = null;

  /**
   * constructor
   * 
   * @param con
   *          connection to a oracle database with spatial extension.
   * @param sdoVersion
   *          version of the spatial extension
   */
  public OracleSpatialAccess( Connection con )
  {
    super( con );
  }

  /**
   * constructor
   * 
   * @param con
   *          connection to a oracle database with spatial extension.
   */
  public OracleSpatialAccess( Connection con, final boolean autoCommit ) throws SQLException
  {
    super( con, autoCommit );
  }

  /**
   * @param driver
   *          name of the jdbc driver
   * @param logon
   *          address of the database (and additional parameters)
   * @param user
   * @param password
   * @param sdoVersion
   *          version of the spatial data objects (depends on the used spatial
   *          driver/lib)
   * @throws SQLException
   * @throws Exception
   */
  public OracleSpatialAccess( final String driver, final String logon, final String user,
      final String password ) throws SQLException, Exception
  {
    super( driver, logon, user, password );
  }

  /**
   * @param driver
   *          name of the jdbc driver
   * @param logon
   *          address of the database (and additional parameters)
   * @param properties
   *          additional properties
   * @param sdoVersion
   * @throws SQLException
   * @throws Exception
   */
  public OracleSpatialAccess( final String driver, final String logon, final Properties properties )
      throws SQLException, Exception
  {
    super( driver, logon, properties );
  }

  /**
   * @param driver
   *          name of the jdbc driver
   * @param logon
   *          address of the database (and additional parameters)
   * @param user
   * @param password
   * @param autoCommit
   * @param sdoVersion
   *          version of the spatial data objects (depends on the used spatial
   *          driver/lib)
   * @throws SQLException
   * @throws Exception
   */
  public OracleSpatialAccess( final String driver, final String logon, final String user,
      final String password, final boolean autoCommit ) throws SQLException, Exception
  {
    super( driver, logon, user, password, autoCommit );
  }

  /**
   * @param driver
   *          name of the jdbc driver
   * @param logon
   *          address of the database (and additional parameters)
   * @param properties
   * @param autoCommit
   * @param sdoVersion
   *          version of the spatial data objects (depends on the used spatial
   *          driver/lib)
   * @throws SQLException
   * @throws Exception
   */
  public OracleSpatialAccess( final String driver, final String logon, final Properties properties,
      final boolean autoCommit ) throws SQLException, Exception
  {
    super( driver, logon, properties, autoCommit );
  }

  /**
   * perfomrs a general query against a database. The calling class (method) has
   * to know the type of the returned object to cast it to a more specialized
   * class.
   * 
   * @param query
   *          the query parameter contains the query to perform.
   * @param maxFeatures
   *          the maximum amount of features that should be returned by the
   *          request. if <tt>maxFeatures</tt> is <= 0 all features will be
   *          returned.
   * @return result of the query. if the query failed null will be returned.
   * @exception SQLException
   *              will be thrown if the submitted query can't be parsed to a
   *              valid sql-statement.
   */
  public Object performQuery( String query, final int startFeature, final int maxFeatures )
      throws SQLException, Exception
  {
    Debug.debugMethodBegin();

    query = escape( query );

    Table tm = null;
    ByteArrayOutputStream baos = null;

    OracleResultSet ors = null;

    //check if at least one ordinate array it's present
    int start = query.toUpperCase().indexOf( "mdsys.sdo_ordinate_array".toUpperCase() );

    Statement stmt = null;
    PreparedStatement ps = null;

    if( start != -1 && 1 == 2 )
    { //it's present at least one ordinate array
      ps = reformatStatement( query );
      if( maxFeatures > 0 )
      {
        ps.setMaxRows( maxFeatures + startFeature );
      }
      ors = (OracleResultSet)ps.executeQuery();
    }
    else
    {
      stmt = con.createStatement();
      if( maxFeatures > 0 )
      {
        stmt.setMaxRows( maxFeatures + startFeature );
      }
      ors = (OracleResultSet)stmt.executeQuery( query );
    }

    ResultSetMetaData rsmd = ors.getMetaData();
    int cols = rsmd.getColumnCount();

    // -------------------------------------------
    // identify columns which type equlas STRUCT
    // and get column names and column types
    int cc = 0;
    String[] columnnames = new String[cols];
    String[] columntypes = new String[cols];

    for( int i = 0; i < cols; i++ )
    {
      columnnames[i] = rsmd.getColumnName( i + 1 ).toUpperCase();
      columntypes[i] = mapTypes( rsmd.getColumnType( i + 1 ) );

      if( rsmd.getColumnType( i + 1 ) == Types.STRUCT )
      {
        cc++;
      }
    }

    struct = new int[cc];
    cc = 0;

    for( int i = 0; i < cols; i++ )
    {
      if( rsmd.getColumnType( i + 1 ) == Types.STRUCT )
      {
        struct[cc++] = i;
      }
    }

    if( cols > 0 )
    {
      if( maxFeatures > 0 )
      {
        tm = new Table_Impl( "Table", columnnames, columntypes, maxFeatures );
      }
      else
      {
        tm = new Table_Impl( "Table", columnnames, columntypes, 1000 );
      }
    }
    else
    {
      return null;
    }

    // get the query result row by row and transform it to
    // simple features
    int ii = -1;

    while( ors.next() )
    {
      ii++;

      if( ii >= startFeature )
      {
        Object[] o = new Object[cols];
        for( int i = 0; i < cols; i++ )
        {
          // read db Geometry from column 1 of the next row
          Object ob = ors.getObject( i + 1 );
          // if the column represents a geometry
          if( ob instanceof STRUCT )
          {
            JGeometry jgeom = JGeometry.load( (STRUCT)ob );
            o[i] = OracleAdapter.wrap( jgeom );
          }
          else
          {
            // if it isn't a geometry
            o[i] = ob;
          }
        }
        // add row to the table
        tm.appendRow( o );
        // checks if the max amount of features is reached
        if( tm.getRowCount() == maxFeatures )
        {
          break;
        }
      }
    }

    if( stmt != null )
    {
      stmt.close();
    }
    else
    {
      ps.close();
    }
    ors.close();

    Debug.debugMethodEnd();

    return tm;
  }

  /**
   * reformates the select statement to an prepared statement. this method will
   * be called if the select statement contains a geometry operator
   */
  private PreparedStatement reformatStatement( String query ) throws SQLException
  {
    Debug.debugMethodBegin();

    //count how many array are present
    int count = StringExtend.countString( query.toUpperCase(), "MDSYS.SDO_ORDINATE_ARRAY" );

    //instance necessary array descriptor
    ArrayDescriptor[] coordinate = new ArrayDescriptor[count];

    String[] sdo_ordinate = new String[count];

    for( int l = 0; l < count; l++ )
    {
      coordinate[l] = ArrayDescriptor.createDescriptor( "MDSYS.SDO_ORDINATE_ARRAY", con );
    }

    //store the strings MDSYS.SDO_ORDINATE_ARRAY(x1,y1,x2,y2......)
    sdo_ordinate = StringExtend
        .extractString( query.toUpperCase(), "MDSYS.SDO_ORDINATE_ARRAY", ")" );

    //replace the strings MDSYS.SDO_ORDINATE_ARRAY(x1,y1,x2,y2......) with an
    // "?"
    for( int i = 0; i < count; i++ )
      query = StringExtend.replace( query.toUpperCase(), sdo_ordinate[i], "?", false );

    //extract the strings that represent only the ordinates
    for( int i = 0; i < count; i++ )
    {
      String buffer = sdo_ordinate[i].toString();
      sdo_ordinate[i] = StringExtend.extractArray( buffer, "(", ")", true, true );
    }

    ARRAY[] points = new ARRAY[count];

    //convert the string representing the coordinates in double and load it
    // into an ARRAY
    for( int i = 0; i < count; i++ )
    {
      double[] punti = StringExtend.toArrayDouble( sdo_ordinate[i].toString(), "," );
      points[i] = new ARRAY( coordinate[i], con, punti );
    }

    PreparedStatement ps = con.prepareStatement( query );

    for( int i = 0; i < count; i++ )
    {
      ps.setObject( i + 1, points[i] );
    }

    Debug.debugMethodEnd();

    return ps;
  }

  /**
   * Performs a query against a oracle spatial database. If the type of the
   * query result isn't aren't wkb a Exception will be thrown.
   * 
   * @param query
   *          the query parameter contains the query to perform.
   * @return the wkb's corresponding to the submitted query. If no wkb matches
   *         the query null will be returned.
   * @exception SQLException
   *              will be thrown if the result aren't a wkb's
   */
  public byte[][][] performWKBQuery( final String query ) throws Exception
  {
    Debug.debugMethodBegin( this, "performWKBQuery(String)" );

    byte[][][] data = performWKBQuery( query, 0, -1 );
    Debug.debugMethodEnd();
    return data;
  }

  /**
   * Performs a query against a oracle spatial database. If the type of the
   * query result isn't aren't wkb a JOSException will be thrown.
   * 
   * @param query
   *          the query parameter contains the query to perform.
   * @param maxFeatures
   *          the maximum amount of features that should be returned by the
   *          request. if <tt>maxFeatures</tt> is <= 0 all features will be
   *          returned.
   * @return the wkb's corresponding to the submitted query. If no wkb matches
   *         the query null will be returned.
   * @exception Exception
   *              will be thrown if the result aren't a wkb's
   */
  public byte[][][] performWKBQuery( final String query, final int startFeature,
      final int maxFeatures ) throws Exception
  {
    Debug.debugMethodBegin( this, "performWKBQuery" );

    if( ( struct == null ) || ( struct.length == 0 ) )
    {
      return null;
    }

    byte[][][] result = new byte[struct.length][][];
    Table table = null;

    table = (Table)performQuery( query, startFeature, maxFeatures );

    // transforms each sdo_geometry (WKB) column to a ByteArrayOutputStream
    // containing the WKBs
    for( int j = 0; j < struct.length; j++ )
    {
      result[j] = new byte[table.getRowCount()][];

      for( int i = 0; i < table.getRowCount(); i++ )
      {
        result[j][i] = ( (ByteArrayOutputStream)table.getValueAt( i, struct[j] ) ).toByteArray();
      }
    }

    Debug.debugMethodEnd();

    return result;
  }
}