package com.bce.datacenter.db.timeseries;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.Vector;

import com.bce.datacenter.db.common.DataObject;

/**
 * Channel
 * 
 * @author schlienger
 */
public class Channel extends DataObject
{
  /** separate identifier used for user own needs */
  private String m_identifier;

  /** timeseries belonging to this channel */
  private List m_timeseries = null;

  private int m_typeRef;

  private int m_unitRef;

  private List m_tables = null;

  /**
   * Constructor for existing object to build from database
   * @param con
   * @param id
   *          internal object id
   */
  public Channel( final Connection con, int id )
  {
    super( con, id );
  }

  /**
   * Consructor with parameters
   * 
   * @param con
   * @param id
   * @param name
   * @param desc
   * @param identifier
   * @param ownerRef
   * @param typeRef
   * @param unitRef
   */
  public Channel( final Connection con, int id, String name, String desc, String identifier,
      int ownerRef, int typeRef, int unitRef )
  {
    super( con, id, name, desc );

    m_identifier = identifier;
    m_ownerRef = ownerRef;
    m_typeRef = typeRef;
    m_unitRef = unitRef;
  }

  /**
   * Returns the list of tables that should receive external data from sensors.
   * 
   * @return the list of tables that should receive external data from sensors.
   * @throws SQLException
   */
  public List getTableNames( ) throws SQLException
  {
    if( m_tables == null )
    {
      String sql = "SELECT dataTableName FROM TS_TIMESERIES WHERE is_Recipient = 1 and channel_ref = "
          + m_ID;
      ResultSet rs = m_con.createStatement().executeQuery(
          sql );

      m_tables = new Vector();

      while( rs.next() )
        m_tables.add( rs.getString( 1 ) );

      rs.close();

      m_con.commit();
    }

    return m_tables;
  }

  /**
   * Returns the timeseries of that channel
   * 
   * @return timeseries
   */
  public List getTimeseries( )
  {
    if( m_timeseries != null )
    {
      return m_timeseries;
    }

    m_timeseries = Timeserie.dbReadAll( m_con, m_ID );

    return m_timeseries;
  }

  /**
   * Looks up the database for a channel according to the given identifier
   * 
   * @param con
   * @param identifier
   * @return channel
   * @throws SQLException
   */
  public static Channel findChannel( final Connection con, final String identifier )
      throws SQLException
  {
    String sql = "SELECT ID FROM TS_CHANNEL WHERE IDENTIFIER = ?";

    PreparedStatement stmt = con.prepareStatement( sql );

    stmt.setString( 1, identifier );

    ResultSet rs = stmt.executeQuery();

    rs.next();

    Channel c = null;

    if( rs.getObject( 1 ) != null )
    {
      c = new Channel( con, rs.getInt( 1 ) );
    }

    stmt.close();

    con.commit();

    return c;
  }

  /**
   * read from db to init members
   */
  protected void dbRead( )
  {
    try
    {
      PreparedStatement stmt = m_con.prepareStatement(
              "SELECT NAME, DESCRIPTION, IDENTIFIER, OWNER_REF, TYPE_REF, UNIT_REF FROM TS_CHANNEL WHERE ID = ?" );

      stmt.setInt( 1, m_ID );

      ResultSet rs = stmt.executeQuery();

      rs.next();

      m_name = rs.getString( 1 );
      m_description = rs.getString( 2 );
      m_identifier = rs.getString( 3 );
      m_ownerRef = rs.getInt( 4 );
      m_typeRef = rs.getInt( 5 );
      m_unitRef = rs.getInt( 6 );

      rs.close();
      stmt.close();

      m_con.commit();
    }
    catch( SQLException e )
    {
      e.printStackTrace( System.out );

      try
      {
        m_con.rollback();
      }
      catch( SQLException e1 )
      {
        e1.printStackTrace();
      }
    }
  }
}