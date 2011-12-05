package com.bce.datacenter.db;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

/**
 * Convenience database connection class for the ingres database
 * 
 * @author schlienger
 */
public class IngresDatabase
{
  private final String m_user;

  private final String m_password;

  private final String m_url;
  
  public final static String DRIVER = "ca.edbc.jdbc.EdbcDriver";

  private Connection m_connection = null;

  /**
   * initialise this Database connection
   * 
   * @param url
   *          url of the database (example:
   *          jdbc:edbc://PC070:ME7/BCE_PC070::dbflomatis/INGRES)
   * @param user
   *          user name for connection
   * @param password
   *          user password for connection
   */
  public IngresDatabase( final String url, final String user, final String password )
  {
    m_url = url;
    m_user = user;
    m_password = password;
  }

  /**
   * @return sql connection to database
   */
  public Connection getConnection( )
  {
    if( m_connection == null )
    {
      try
      {
        Class.forName( DRIVER ).newInstance();
        m_connection = DriverManager.getConnection( m_url, m_user, m_password );
        m_connection.setAutoCommit( false );
      }
      catch( Exception e )
      {
        e.printStackTrace();

        throw new IllegalStateException(
            "Database connection could not be established. See stack-trace" );
      }
    }

    return m_connection;
  }

  public boolean tableExists( String tableName )
  {
    try
    {
      tableName = tableName.toUpperCase();

      final Statement st = m_connection.createStatement();

      final ResultSet set = st
          .executeQuery( "SELECT COUNT(*) FROM IITABLES WHERE UPPERCASE(TABLE_NAME) = "
              + tableName );

      set.next();

      if( set.getInt( 1 ) > 0 )
        return true;

      return false;
    }
    catch( SQLException e )
    {
      return false;
    }
  }

  public String getUrl( )
  {
    return m_url;
  }
}