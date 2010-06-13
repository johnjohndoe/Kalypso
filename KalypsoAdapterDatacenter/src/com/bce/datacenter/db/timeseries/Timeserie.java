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
import java.util.TimeZone;
import java.util.Vector;

import com.bce.datacenter.db.persistent.Persistent;

/**
 * Wraps all kind of timeseries that can be owned by a channel or a computation. Channels own work, original, and
 * computed timeseries. NOTE: TimeserieWrapper are not stored as is into the database, they just represent some common
 * business logic upon timeseries.
 * 
 * @author Marc Schlienger
 * @see Channel
 */
public class Timeserie extends Persistent
{
  private final static TimeserieTupple[] EMPTY_TUPPLE = new TimeserieTupple[0];

  private String m_dataTableName;

  private String m_description;

  private String m_name;

  private String m_type;

  private int m_channelRef;

  private static SimpleDateFormat m_sqlDateFormat = new SimpleDateFormat( "MM/dd/yyyy HH:mm:ss z" );
  static
  {
    m_sqlDateFormat.setTimeZone( TimeZone.getTimeZone( "GMT" ) );
  }

  private Channel m_channel = null;

  /**
   * Constructor
   * 
   * @param id
   *          internal db identifier
   */
  public Timeserie( final Connection con, final int id )
  {
    super( con, id, true );
  }

  /**
   * Constructor with parameters
   */
  public Timeserie( final Connection con, final int id, final String name, final String desc, final String type, final String tableName, final int channelRef )
  {
    super( con, id, false );

    m_name = name;
    m_description = desc;
    m_type = type;
    m_dataTableName = tableName;
    m_channelRef = channelRef;
  }

  public String getType( )
  {
    return m_type;
  }

  public String getDescription( )
  {
    return m_description;
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
      final Statement st = m_con.createStatement();

      final ResultSet set = st.executeQuery( "SELECT MIN(TSTIME) FROM " + m_dataTableName );

      set.next();

      final Date d = set.getDate( 1 );

      set.close();
      st.close();

      m_con.commit();

      return d;
    }
    catch( final SQLException e )
    {
      e.printStackTrace();

      try
      {
        m_con.rollback();
      }
      catch( final SQLException e1 )
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
      final Statement st = m_con.createStatement();

      final ResultSet set = st.executeQuery( "SELECT MAX(TSTIME) FROM " + m_dataTableName );

      set.next();

      final Date d = set.getDate( 1 );

      set.close();
      st.close();

      m_con.commit();

      return d;
    }
    catch( final SQLException e )
    {
      e.printStackTrace();

      try
      {
        m_con.rollback();
      }
      catch( final SQLException e1 )
      {
        e1.printStackTrace();
      }

      return null;
    }
  }

  /**
   * Looks up for a timeserie in the database according to the given dataTableName.
   */
  public static Timeserie findTimeserie( final Connection con, final String dataTableName ) throws SQLException
  {
    final String sql = "SELECT TSID FROM TS_TIMESERIES WHERE DATATABLENAME = ?";

    final PreparedStatement stmt = con.prepareStatement( sql );

    stmt.setString( 1, dataTableName );

    final ResultSet rs = stmt.executeQuery();

    rs.next();

    Timeserie ts = null;

    // System.out.println("Resultset get Object: "+rs.getObject(1));

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
   *          string representing the separator between the tokens: date, value, and flag. (if null default is comma)
   * @param dateFormatPattern
   *          string such as in SimpleDateFormat representing the format of the date. (if null uses the locale default)
   * @return amount of lines written if successfull, otherwise -1
   */
  public int ExportToFile( final String filename, final Date from, final Date to, String separator, final String dateFormatPattern )
  {
    try
    {
      int line = 0;
      final Statement st = m_con.createStatement();
      final ResultSet set = st.executeQuery( createQuery( from, to ) );
      try
      {
        if( separator == null )
        {
          separator = ",";
        }

        final FileWriter fw = new FileWriter( filename );

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

          final Date datum = new Date( set.getTime( 1 ).getTime() + set.getDate( 1 ).getTime() );
          fw.write( sdf.format( datum ) );
          fw.write( separator );
          fw.write( set.getString( 2 ) );
          fw.write( separator );
          fw.write( set.getString( 3 ) + '\n' );

          line++;
        }

        fw.close();
      }
      catch( final IOException e )
      {
        e.printStackTrace();
      }

      set.close();
      st.close();

      m_con.commit();

      return line;
    }
    catch( final SQLException e )
    {
      e.printStackTrace();

      try
      {
        m_con.rollback();
      }
      catch( final SQLException e1 )
      {
        e1.printStackTrace();
      }

      return -1;
    }
  }

  /**
   * createsQuery for this Timeseries
   * 
   * @param from
   * @param to
   */
  private String createQuery( final java.util.Date from, final java.util.Date to )
  {
    // get the name of the timeseries table
    final String tabname = m_dataTableName;

    /*
     * prepare statement for extracting desired timeseries and create a temp table with these timeseries
     */
    String str = "SELECT TSTIME, VALUE, FLAG FROM " + tabname;

    String tmp_stmt = "";

    if( from != null )
    {
      /* the where clause contains just one from-to time range */

      // time-from
      tmp_stmt = "( TSTIME >= '" + m_sqlDateFormat.format( from ) + "'";

      // look if time-to is specified
      if( to == null )
      {
        // no time-to, so close the parenthesis and stop processing
        tmp_stmt += ")";
      }
      else
      {
        // add time-to specification
        tmp_stmt += (" AND TSTIME <= '" + m_sqlDateFormat.format( to ) + "')");
      }
    }
    else
    {
      if( to != null )
      {
        tmp_stmt = "TSTIME <= '" + m_sqlDateFormat.format( to ) + "'";
      }
    }

    // look if some where clause has been created
    if( tmp_stmt != "" )
    {
      // add where clause
      str += (" WHERE " + tmp_stmt);
    }
    return str;
  }

  /**
   * Returns values in the form of an array of TimeserieTupples
   */
  public TimeserieTupple[] getValues( final java.util.Date from, final java.util.Date to ) throws SQLException
  {
    try
    {
      final Statement st = m_con.createStatement();
      // TODO: Berücksichtigt die Zeitzone nicht richtig - es sollte die Datenbankzeitzone gewählt werden!!!
      final ResultSet set = st.executeQuery( createQuery( from, to ) );

      final Vector<TimeserieTupple> list = new Vector<TimeserieTupple>();

      // TODO Performanz verbessern!!!
      while( set.next() )
      {
        final java.util.Date datum = set.getTimestamp( 1 );
        final Double value = new Double( set.getDouble( 2 ) );
        final String status = set.getString( 3 );

        list.add( new TimeserieTupple( datum, value, status ) );
      }

      set.close();
      st.close();

      m_con.commit();

      return list.toArray( new TimeserieTupple[list.size()] );
    }
    catch( final SQLException e )
    {
      e.printStackTrace();

      m_con.rollback();

      return EMPTY_TUPPLE;
    }
  }

  /**
   * Sets the values in the db
   */
  public void setValues( final TimeserieTupple[] tupples )
  {
    // TODO implement setValues( final TimeserieTupple[] tupples )
    throw new UnsupportedOperationException( "setValues() tupples= " + tupples );
  }

  @Override
  public String toString( )
  {
    return m_name + " (" + m_dataTableName + ")";
  }

  /**
   * @see com.bce.datacenter.db.persistent.Persistent#dbRead()
   */
  @Override
  protected void dbRead( )
  {
    try
    {
      final PreparedStatement stmt = m_con.prepareStatement( "SELECT NAME, DESCRIPTION, TYPE, CHANNEL_REF, DATATABLENAME FROM TS_TIMESERIES WHERE TSID = ?" );

      stmt.setInt( 1, m_ID );

      final ResultSet rs = stmt.executeQuery();

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
    catch( final SQLException e )
    {
      e.printStackTrace();

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

  /**
   * Fast load of all timeseries for a channel
   * 
   * @return list of all timeseries for a channel
   */
  protected static List<Timeserie> dbReadAll( final Connection con, final int channelRef )
  {
    final Vector<Timeserie> v = new Vector<Timeserie>();

    try
    {
      final PreparedStatement stmt = con.prepareStatement( "SELECT TSID, NAME, DESCRIPTION, TYPE, DATATABLENAME FROM TS_TIMESERIES WHERE CHANNEL_REF = ? ORDER BY NAME" );

      stmt.setInt( 1, channelRef );

      final ResultSet rs = stmt.executeQuery();

      while( rs.next() )
      {
        final Timeserie c = new Timeserie( con, rs.getInt( 1 ), rs.getString( 2 ), rs.getString( 3 ), rs.getString( 4 ), rs.getString( 5 ), channelRef );

        v.add( c );
      }

      rs.close();
      stmt.close();

      con.commit();
    }
    catch( final SQLException e )
    {
      e.printStackTrace();

      try
      {
        con.rollback();
      }
      catch( final SQLException e1 )
      {
        e1.printStackTrace();
      }
    }

    return v;
  }

  @Override
  public boolean equals( final Object object )
  {
    if( object == null )
      return false;
    if( !(object instanceof Timeserie) )
      return false;
    return ((Timeserie) object).getID() == getID();
  }
}