package com.bce.datacenter.db.timeseries;

import java.io.FileWriter;
import java.io.IOException;
import java.sql.Connection;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.Vector;

import com.bce.datacenter.db.persistent.Persistent;

/**
 * Wraps all kind of timeseries that can be owned by a channel or a computation.
 * Channels own work, original, and computed timeseries. NOTE: TimeserieWrapper
 * are not stored as is into the database, they just represent some common
 * business logic upon timeseries.
 * 
 * @author Marc Schlienger
 * 
 * @see Channel
 */
public class Timeserie extends Persistent
{
  private String m_dataTableName;

  private String m_description;

  private String m_name;

  private String m_type;

  private int m_channelRef;

  private Channel m_channel = null;

  /**
   * Constructor
   * @param con
   * @param id
   *          internal db identifier
   */
  public Timeserie( final Connection con, int id )
  {
    super( con, id, true );
  }

  /**
   * Constructor with parameters
   * @param con
   * @param id
   * @param name
   * @param desc
   * @param type
   * @param tableName
   * @param channelRef
   */
  public Timeserie(final Connection con, int id, String name, String desc, String type,
      String tableName, int channelRef )
  {
    super( con, id, false );

    m_name = name;
    m_description = desc;
    m_type = type;
    m_dataTableName = tableName;
    m_channelRef = channelRef;
  }

  public String getDataTableName( )
  {
    return m_dataTableName;
  }

  public String getName( )
  {
    return m_name;
  }

  /**
   * returns the mindate of the wrapped timeseries
   * 
   * @return min date, or null if error occurs
   */
  public Date getRealBegin( )
  {
    try
    {
      Statement st = m_con.createStatement();

      ResultSet set = st.executeQuery( "SELECT MIN(TSTIME) FROM "
          + m_dataTableName );

      set.next();

      Date d = set.getDate( 1 );

      set.close();
      st.close();

      m_con.commit();

      return d;
    }
    catch( SQLException e )
    {
      e.printStackTrace();

      try
      {
        m_con.rollback();
      }
      catch( SQLException e1 )
      {
        e1.printStackTrace();
      }

      return null;
    }
  }

  /**
   * returns the max date of the wrapped timeseries
   * 
   * @return max date, or null if error occurs
   */
  public Date getRealEnd( )
  {
    try
    {
      Statement st = m_con.createStatement();

      ResultSet set = st.executeQuery( "SELECT MAX(TSTIME) FROM "
          + m_dataTableName );

      set.next();

      Date d = set.getDate( 1 );

      set.close();
      st.close();

      m_con.commit();

      return d;
    }
    catch( SQLException e )
    {
      e.printStackTrace();

      try
      {
        m_con.rollback();
      }
      catch( SQLException e1 )
      {
        e1.printStackTrace();
      }

      return null;
    }
  }

  /**
   * Looks up for a timeserie in the database according to the given
   * dataTableName.
   * @param con
   * @param dataTableName
   * @return
   * @throws SQLException
   */
  public static Timeserie findTimeserie( final Connection con, final String dataTableName )
      throws SQLException
  {
    final String sql = "SELECT TSID FROM TS_TIMESERIES WHERE DATATABLENAME = ?";

    final PreparedStatement stmt = con.prepareStatement( sql );

    stmt.setString( 1, dataTableName );

    final ResultSet rs = stmt.executeQuery();

    rs.next();

    Timeserie ts = null;

    //System.out.println("Resultset get Object: "+rs.getObject(1));

    if( rs.getObject( 1 ) != null )
      ts = new Timeserie( con, rs.getInt( 1 ) );

    stmt.close();

    con.commit();

    return ts;
  }

  public Channel getChannel( )
  {
    if( m_channel == null )
      m_channel = new Channel( m_con, m_channelRef );

    return m_channel;
  }

  /**
   * exports the timeserie owned by this wrapper to a file
   * 
   * @param filename
   *          pathname of the file to create
   * @param from
   *          date from which to export (if null export from beginning)
   * @param to
   *          date up to export (if null export until end)
   * @param separator
   *          string representing the separator between the tokens: date, value,
   *          and flag. (if null default is comma)
   * @param dateFormatPattern
   *          string such as in SimpleDateFormat representing the format of the
   *          date. (if null uses the locale default)
   * 
   * @return amount of lines written if successfull, otherwise -1
   */
  public int ExportToFile( String filename, Date from, Date to,
      String separator, String dateFormatPattern )
  {
    try
    {
      int line = 0;

      Statement st = m_con.createStatement();

      // get the name of the timeseries table
      String tabname = m_dataTableName;

      /*
       * prepare statement for extracting desired timeseries and create a temp
       * table with these timeseries
       */
      String str = "SELECT TSTIME, VALUE, FLAG FROM " + tabname;

      String tmp_stmt = "";

      if( from != null )
      {
        /* the where clause contains just one from-to time range */

        // time-from
        tmp_stmt = "( TSTIME >= '" + from + "'";

        // look if time-to is specified
        if( to == null )
        {
          // no time-to, so close the parenthesis and stop processing
          tmp_stmt += ")";
        }
        else
        {
          // add time-to specification
          tmp_stmt += (" AND TSTIME <= '" + to + "')");
        }
      }
      else
      {
        if( to != null )
        {
          tmp_stmt = "TSTIME <= '" + to + "'";
        }
      }

      // look if some where clause has been created
      if( tmp_stmt != "" )
      {
        // add where clause
        str += (" WHERE " + tmp_stmt);
      }

      ResultSet set = st.executeQuery( str );

      try
      {
        if( separator == null )
        {
          separator = ",";
        }

        FileWriter fw = new FileWriter( filename );

        SimpleDateFormat sdf = null;

        if( dateFormatPattern != null )
        {
          sdf = new SimpleDateFormat( dateFormatPattern );
        }
        else
        {
          sdf = new SimpleDateFormat();
        }

        while( set.next() )
        {

          Date datum = new Date( set.getTime( 1 ).getTime()
              + set.getDate( 1 ).getTime() );
          fw.write( sdf.format( datum ) );
          fw.write( separator );
          fw.write( set.getString( 2 ) );
          fw.write( separator );
          fw.write( set.getString( 3 ) + '\n' );

          line++;
        }

        fw.close();
      }
      catch( IOException e )
      {
        e.printStackTrace();
      }

      set.close();
      st.close();

      m_con.commit();

      return line;
    }
    catch( SQLException e )
    {
      e.printStackTrace();

      try
      {
        m_con.rollback();
      }
      catch( SQLException e1 )
      {
        e1.printStackTrace();
      }

      return -1;
    }
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    return m_name + " (" + m_dataTableName + ")";
  }

  /**
   * @see com.bce.datacenter.db.persistent.Persistent#dbRead()
   */
  protected void dbRead( )
  {
    try
    {
      PreparedStatement stmt = m_con
          .prepareStatement(
              "SELECT NAME, DESCRIPTION, TYPE, CHANNEL_REF, DATATABLENAME FROM TS_TIMESERIES WHERE TSID = ?" );

      stmt.setInt( 1, m_ID );

      ResultSet rs = stmt.executeQuery();

      rs.next();

      m_name = rs.getString( 1 );
      m_description = rs.getString( 2 );
      m_type = rs.getString( 3 );
      m_channelRef = rs.getInt( 4 );
      m_dataTableName = rs.getString( 5 );

      rs.close();
      stmt.close();

      m_con.commit();
    }
    catch( SQLException e )
    {
      e.printStackTrace();

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

  /**
   * Fast load of all timeseries for a channel
   * @param con
   * @param channelRef
   * @return list of all timeseries for a channel
   */
  protected static List dbReadAll( final Connection con, int channelRef )
  {
    Vector v = new Vector();

    try
    {
      PreparedStatement stmt = con
          .prepareStatement(
              "SELECT TSID, NAME, DESCRIPTION, TYPE, DATATABLENAME FROM TS_TIMESERIES WHERE CHANNEL_REF = ? ORDER BY NAME" );

      stmt.setInt( 1, channelRef );

      ResultSet rs = stmt.executeQuery();

      while( rs.next() )
      {
        Timeserie c = new Timeserie( con, rs.getInt( 1 ), rs.getString( 2 ), rs
            .getString( 3 ), rs.getString( 4 ), rs.getString( 5 ), channelRef );

        v.add( c );
      }

      rs.close();
      stmt.close();

      con.commit();
    }
    catch( SQLException e )
    {
      e.printStackTrace();

      try
      {
        con.rollback();
      }
      catch( SQLException e1 )
      {
        e1.printStackTrace();
      }
    }

    return v;
  }

  public boolean equals( Object object )
  {
    if( object == null )
      return false;
    if( !(object instanceof Timeserie) )
      return false;
    return ((Timeserie) object).getID() == getID();
  }
}