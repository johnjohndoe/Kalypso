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

/**
 * 
 * ------------------------------------------------------------------------
 * 
 * @author Andreas Poth <a href="mailto:poth@lat-lon.de">Andreas Poth <a>
 * @version 13.07.2001
 */

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.HashMap;
import java.util.Properties;

import org.deegree.model.table.Table;
import org.deegree_impl.model.table.Table_Impl;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.StringExtend;

public class DBAccess
{

  protected Connection con = null;

  private DBConnectionPool pool = null;

  /**
   * constructor
   * 
   * @param con
   *          connection to a oracle database with spatial extension.
   */
  public DBAccess( Connection con )
  {
    this.con = con;
  }

  /**
   * constructor
   * 
   * @param con
   *          connection to a oracle database with spatial extension.
   */
  public DBAccess( Connection con, final boolean autoCommit ) throws SQLException
  {

    this.con = con;
    con.setAutoCommit( autoCommit );
  }

  /**
   * @param driver
   *          jdbc driver name
   * @param logon
   *          logon (database address, additional parameters)
   * @param user
   * @param password
   * @throws SQLException
   * @throws Exception
   */
  public DBAccess( final String driver, final String logon, final String user, final String password )
      throws SQLException, Exception
  {

    pool = DBConnectionPool.getInstance();
    con = pool.acuireConnection( driver, logon, user, password );
    con.setAutoCommit( true );
  }

  /**
   * @param driver
   *          jdbc driver name
   * @param logon
   *          logon (database address, additional parameters)
   * @param properties
   *          additional parameters
   * @throws SQLException
   * @throws Exception
   */
  public DBAccess( final String driver, final String logon, final Properties properties )
      throws SQLException, Exception
  {

    pool = DBConnectionPool.getInstance();
    con = pool.acuireConnection( driver, logon, properties );
    con.setAutoCommit( true );
  }

  /**
   * @param driver
   *          jdbc driver name
   * @param logon
   *          logon (database address, additional parameters)
   * @param user
   * @param password
   * @param autoCommit
   * @throws SQLException
   * @throws Exception
   */
  public DBAccess( final String driver, final String logon, final String user,
      final String password, final boolean autoCommit ) throws SQLException, Exception
  {

    pool = DBConnectionPool.getInstance();
    con = pool.acuireConnection( driver, logon, user, password );
    con.setAutoCommit( autoCommit );
  }

  /**
   * @param driver
   *          jdbc driver name
   * @param logon
   *          logon (database address, additional parameters)
   * @param properties
   * @param autoCommit
   * @throws SQLException
   * @throws Exception
   */
  public DBAccess( final String driver, final String logon, final Properties properties,
      final boolean autoCommit ) throws SQLException, Exception
  {

    pool = DBConnectionPool.getInstance();
    con = pool.acuireConnection( driver, logon, properties );
    con.setAutoCommit( autoCommit );
  }

  public void kill()
  {}

  /**
   * sets the auto commit status of the connection
   */
  public void setAutoCommit( boolean autoCommit ) throws Exception
  {
    con.setAutoCommit( autoCommit );
  }

  public void rollback() throws Exception
  {
    con.rollback();
  }

  /**
   * returns the SQL connection encapsulated by the object
   */
  public Connection getConnection()
  {
    return con;
  }

  /**
   * perfomrs a general query against a database. The calling class (method) has
   * to know the type of the returned object to cast it to a more specialized
   * class.
   * 
   * @param query
   *          the query parameter contains the query to perform.
   * @return result of the query. if the query failed null will be returned.
   * @exception SQLException
   *              will be thrown if the submitted query can't be parsed to a
   *              valid sql-statement.
   */
  public Object performQuery( final String query ) throws SQLException, Exception
  {
    Debug.debugMethodBegin( this, "performQuery(String)" );
    Object o = performQuery( query, 0, -1 );
    Debug.debugMethodEnd();
    return o;
  }

  /**
   * perfomrs a general query against a database. The calling class (method) has
   * to know the type of the returned object to cast it to a more specialized
   * class.
   * 
   * @param query
   *          the query parameter contains the query to perform.
   * @param startFeature
   *          index of the feature the query starts
   * @param maxFeatures
   *          the maximum amount of features that should be returned by the
   *          request. if <tt>maxFeatures</tt> is <= 0 all features will be
   *          returned.
   * @return result of the query. if the query failed null will be returned.
   * @exception SQLException
   *              will be thrown if the submitted query can't be parsed to a
   *              valid sql-statement.
   */
  public Object performQuery( String query, int startFeature, int maxFeatures )
      throws SQLException, Exception
  {

    Debug.debugMethodBegin();

    query = escape( query );
    Table tm = null;

    if( startFeature < 0 )
      startFeature = 0;

    // create a statement object
    Statement stmt = con.createStatement();

    if( maxFeatures > 0 )
    {
      stmt.setMaxRows( maxFeatures + startFeature );
    }
    // execute query against the database
    ResultSet ors = stmt.executeQuery( query );

    ResultSetMetaData rsmd = ors.getMetaData();

    int cols = rsmd.getColumnCount();

    // -------------------------------------------
    // identify columns which type equlas STRUCT
    // and get column names and column types
    String[] columnnames = new String[cols];
    String[] columntypes = new String[cols];
    for( int i = 0; i < cols; i++ )
    {
      columnnames[i] = rsmd.getColumnName( i + 1 ).toUpperCase();
      columntypes[i] = mapTypes( rsmd.getColumnType( i + 1 ) );
    }
    //--------------------------------------------

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

    // get the query result row by row
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
          o[i] = ors.getObject( i + 1 );
        }
        // add row to the table
        tm.appendRow( o );
      }
    }
    ors.close();
    stmt.close();

    Debug.debugMethodEnd();

    return tm;
  }

  protected String mapTypes( final int type )
  {

    String clname = null;

    switch( type )
    {

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
    default:
      clname = "java.lang.Object";
      break;

    }

    return clname;

  }

  /**
   * commits the perfomerd queries, inserts and updates if autoCommit is set to
   * false.
   */
  public void commit() throws SQLException
  {
    con.commit();
  }

  /**
   * perfomrs a simple query to get a Table
   * 
   * @param query
   *          the query parameter contains the query to perform.
   * @return the result of the query as table. the names of the table columns
   *         are expressed in capital letters.
   */
  public Table performTableQuery( final String query ) throws Exception
  {
    Debug.debugMethodBegin( this, "performTableQuery(String)" );
    Table table = performTableQuery( query, 0, -1 );
    Debug.debugMethodEnd();
    return table;
  }

  /**
   * perfomrs a simple query to get a Table
   * 
   * @param query
   *          the query parameter contains the query to perform.
   * @param startFeature
   *          index of the feature the query starts
   * @param maxFeatures
   *          the maximum amount of features that should be returned by the
   *          request. if <tt>maxFeatures</tt> is <= 0 all features will be
   *          returned.
   * @return the result of the query as table. the names of the table columns
   *         are expressed in capital letters.
   */
  public Table performTableQuery( final String query, final int startFeature, final int maxFeatures )
      throws Exception
  {

    Debug.debugMethodBegin();
    Table table = (Table)performQuery( query, startFeature, maxFeatures );
    Debug.debugMethodEnd();

    return table;

  }

  /**
   * performs the update described by the submitted SQL-Statement
   * 
   * @param update
   *          sql-statement that should be performed
   */
  public void performUpdate( final String update ) throws Exception
  {
    Debug.debugMethodBegin( this, "performUpdate" );

    // executes a database update
    Statement stmt = con.createStatement();
    stmt.executeUpdate( update );
    stmt.close();

    Debug.debugMethodEnd();
  }

  /**
   * Performs the given SQL-statement against the database.
   * <p>
   * 
   * @param sql
   *          the SQL-statement to perform
   * @throws SQLException
   *           if the SQL was erroneus or a general database error occured
   */
  public void executeStatement( final String sql ) throws SQLException
  {
    Debug.debugMethodBegin( this, "executeStatement (String)" );

    // execute a database insertion
    Statement stmt = con.createStatement();
    try
    {
      stmt.execute( sql );
    }
    catch( SQLException e )
    {
      stmt.close();
      throw ( e );
    }
    stmt.close();

    Debug.debugMethodEnd();
  }

  /**
   * performs the insertion described by the submitted SQL-Statement
   * 
   * @param insert
   *          sql-statement that should be performed
   */
  public void performInsert( final String insert ) throws Exception
  {
    Debug.debugMethodBegin( this, "performInsert(String)" );

    // execute a database insertion
    Statement stmt = con.createStatement();
    stmt.execute( insert );
    stmt.close();

    Debug.debugMethodEnd();
  }

  /**
   * performs the insertion described by the submitted SQL-Statement
   * 
   * @param insert
   *          sql-statement that should be performed
   */
  public void performInsert( final String[] insert ) throws Exception
  {
    Debug.debugMethodBegin( this, "performInsert(String[])" );

    // execute a database insertion
    Statement stmt = con.createStatement();
    for( int i = 0; i < insert.length; i++ )
    {
      stmt.execute( insert[i] );
    }
    stmt.close();

    Debug.debugMethodEnd();
  }

  /**
   * returns a HashMap that maps a column to a data type. if null is submitted
   * for columns all columns are considered.
   */
  public HashMap getColumnTypes( String table, String[] columns )
  {
    Debug.debugMethodBegin();

    HashMap map = null;
    try
    {
      Statement stmt = con.createStatement();
      stmt.setMaxRows( 1 );
      String query = " Select ";
      if( columns != null )
      {
        for( int i = 0; i < columns.length; i++ )
        {
          query = query + columns[i] + ",";
        }
        query = StringExtend.validateString( query, "," );
      }
      else
      {
        query = query + " * ";
      }
      query = query + " FROM " + table + " WHERE 1 = 0";
      ResultSet rs = stmt.executeQuery( query );

      ResultSetMetaData rsmd = rs.getMetaData();
      map = new HashMap( rsmd.getColumnCount() );
      for( int i = 0; i < rsmd.getColumnCount(); i++ )
      {
        int k = rsmd.getColumnType( i + 1 );
        String s = rsmd.getColumnName( i + 1 );
        map.put( s.toUpperCase(), mapTypes( k ) );
      }
      rs.close();
      stmt.close();
    }
    catch( Exception ex )
    {
      System.out.println( ex );
    }

    Debug.debugMethodEnd();
    return map;
  }

  /**
   * returns a HashMap that maps a column to a data type. if null is submitted
   * for columns all columns are considered.
   */
  public HashMap getColumnTypesAsInt( String table, String[] columns )
  {
    Debug.debugMethodBegin( this, "getColumnTypesAsInt" );

    HashMap map = null;
    try
    {
      Statement stmt = con.createStatement();
      stmt.setMaxRows( 1 );
      String query = " Select ";
      if( columns != null )
      {
        for( int i = 0; i < columns.length; i++ )
        {
          query = query + columns[i] + ",";
        }
        query = StringExtend.validateString( query, "," );
      }
      else
      {
        query = query + " * ";
      }
      query = query + " FROM " + table + " WHERE 1 = 0";

      ResultSet rs = stmt.executeQuery( query );

      map = new HashMap();
      ResultSetMetaData rsmd = rs.getMetaData();

      for( int i = 0; i < rsmd.getColumnCount(); i++ )
      {
        int k = rsmd.getColumnType( i + 1 );
        String s = rsmd.getColumnName( i + 1 );
        map.put( s.toUpperCase(), new Integer( k ) );
      }
      rs.close();
      stmt.close();
    }
    catch( Exception ex )
    {
      System.out.println( ex );
    }

    Debug.debugMethodEnd();
    return map;
  }

  /**
   * returns the database vendor the access id made to
   */
  public String getDataBaseVendor()
  {
    String dbVendor = null;
    try
    {
      DatabaseMetaData dbmd = con.getMetaData();
      dbVendor = dbmd.getDatabaseProductName();
    }
    catch( Exception e )
    {
      System.out.println( e );
    }
    return dbVendor;
  }

  /**
   * escapes characters depending on the database vendor
   */
  protected String escape( String query )
  {

    if( "ACCESS".equalsIgnoreCase( getDataBaseVendor() ) )
    {
      query = StringExtend.replace( query, "$'$", "''", true );
      query = StringExtend.replace( query, "$%$", "[%]", true );
    }
    else if( "ORACLE".equalsIgnoreCase( getDataBaseVendor() ) )
    {
      //            query = StringExtend.replace( query, "$'$", "''", true );
      //            query = StringExtend.replace( query, "$%$", "/%", true );
      //            query = StringExtend.replace( query, "$_$", "/_", true );
      //            query += " {ESCAPE '/'}";
    }
    else
    {
      query = StringExtend.replace( query, "$'$", "''", true );
      query = StringExtend.replace( query, "$%$", "\\%", true );
    }

    return query;
  }

}