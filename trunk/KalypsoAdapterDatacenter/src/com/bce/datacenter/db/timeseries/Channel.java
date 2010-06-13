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
  private List<Timeserie> m_timeseries = null;

  private int m_typeRef;

  private int m_unitRef;

  private List<String> m_tables = null;

  private String m_strUnit = null;

  private String m_strType = null;

  /**
   * Constructor for existing object to build from database
   * 
   * @param con
   * @param id
   *          internal object id
   */
  public Channel( final Connection con, final int id )
  {
    super( con, id );
  }

  /**
   * Consructor with parameters
   */
  public Channel( final Connection con, final int id, final String name, final String desc, final String identifier, final int ownerRef, final int typeRef, final int unitRef )
  {
    super( con, id, name, desc );

    m_identifier = identifier;
    m_ownerRef = ownerRef;
    m_typeRef = typeRef;
    m_unitRef = unitRef;
  }

  public String getIdentifier( )
  {
    return m_identifier;
  }

  public String getUnit( ) throws SQLException
  {
    if( m_strUnit == null )
    {
      final String sql = "SELECT UNITSTR FROM G_UNITS WHERE UNITID = " + m_unitRef;
      final ResultSet rs = m_con.createStatement().executeQuery( sql );
      rs.next();

      m_strUnit = rs.getString( 1 );

      rs.close();
      m_con.commit();
    }

    return m_strUnit;
  }

  public String getType( ) throws SQLException
  {
    if( m_strType == null )
    {
      final String sql = "SELECT NAME FROM TS_CHANNELTYPE WHERE ID = " + m_typeRef;
      final ResultSet rs = m_con.createStatement().executeQuery( sql );
      rs.next();

      m_strType = rs.getString( 1 );

      rs.close();
      m_con.commit();
    }

    return m_strType;
  }

  /**
   * Returns the list of tables that should receive external data from sensors.
   * 
   * @return the list of tables that should receive external data from sensors.
   * @throws SQLException
   */
  public List<String> getTableNames( ) throws SQLException
  {
    if( m_tables == null )
    {
      final String sql = "SELECT dataTableName FROM TS_TIMESERIES WHERE is_Recipient = 1 and channel_ref = " + m_ID;
      final ResultSet rs = m_con.createStatement().executeQuery( sql );

      m_tables = new Vector<String>();

      while( rs.next() )
        m_tables.add( rs.getString( 1 ) );

      rs.close();

      m_con.commit();
    }

    return m_tables;
  }

  /**
   * Returns the timeseries of that channel
   */
  public List<Timeserie> getTimeseries( )
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
   */
  public static Channel findChannel( final Connection con, final String identifier ) throws SQLException
  {
    final String sql = "SELECT ID FROM TS_CHANNEL WHERE IDENTIFIER = ?";

    final PreparedStatement stmt = con.prepareStatement( sql );

    stmt.setString( 1, identifier );

    final ResultSet rs = stmt.executeQuery();

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
  @Override
  protected void dbRead( )
  {
    try
    {
      final PreparedStatement stmt = m_con.prepareStatement( "SELECT NAME, DESCRIPTION, IDENTIFIER, OWNER_REF, TYPE_REF, UNIT_REF FROM TS_CHANNEL WHERE ID = ?" );

      stmt.setInt( 1, m_ID );

      final ResultSet rs = stmt.executeQuery();

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
    catch( final SQLException e )
    {
      e.printStackTrace( System.out );

      try
      {
        m_con.rollback();
      }
      catch( final SQLException e1 )
      {
        e1.printStackTrace();
      }
    }
  }
}