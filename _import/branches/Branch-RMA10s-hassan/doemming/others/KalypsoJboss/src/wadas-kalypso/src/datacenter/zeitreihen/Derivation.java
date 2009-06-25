package datacenter.zeitreihen;

import java.sql.*;

import datacenter.persistent.*;

/**
 * Derivation represent specific timeseries for a channel.
 *
 *
 * @author Marc Schlienger
 * @see Channel
 * @see TimeserieWrapper
 */
public class Derivation extends Persistent
{
	/**
	 * some arbitrary name
	 */
	private String m_Name;

	/**
	 * some arbitrary description
	 */
	private String m_Description;;

	/**
	 * not of big use here...
	 */
	private Date m_From;
	/**
	 * not of big use here...
	 */
	private Date m_To;

	/**
	 * name of the table storing the timeseries of this Derivation
	 */
	private String m_TableName;

	/**
	 * name of the source table name from which timeseries have been computed.
	 * Usually this is the Channel work or original timeseries table.
	 */
	private String m_SourceTableName;

	/**
	 * identifier of the owner of the source timeseries
	 */
	private int m_SourceID;

	/**
	 * type of the owner of the source timeseries
	 */
	private String m_SourceType;

	/**
	 * identifier of the Channel to which this Derivation belongs to
	 */
	private int m_ChannelRef;

	/**
	 * identifier of the unit that this Derivation has
	 */
	private int m_UnitRef;

	/**
	 * default constructor
	 */
	public Derivation()
	{
		super();
	}

	/**
	 * constructor for creating a Derivation from existing one from database
	 *
	 * @param id	the identifier of the database stored Derivation
	 */
	public Derivation(int id)
	{
		super(id);
	}


	/**************************************************************************************
	ACCESSORS
	**************************************************************************************/

	public String GetName()
	{
		return m_Name;
	}

	public String toString()
	{
		return "Derivation: " + m_Name;
	}

	public String GetDescription()
	{
		return m_Description;
	}

	public Date GetTo()
	{
		return m_To;
	}

	public Date GetFrom()
	{
		return m_From;
	}

	public int GetUnit()
	{
		return m_UnitRef;
	}

	/**
	 * @return the name of the table in which the timeseries for this object are located
	 */
	public String GetTableName()
	{
		return m_TableName;
	}

	/**
	 * @return a channel object, to which this derivation belongs to
	 */
	public Channel GetChannel()
	{
		try{

		Channel channel = new Channel(m_ChannelRef);
		channel.PLoad();
		return channel;

		}catch(PersistentException e){ e.printStackTrace(System.out); return null;}
	}


	/**
	 * returns a wrapper for the timeseries owned by this Derivation
	 *
	 * @return	TimeserieWrapper of the timeseries owned by this Derivation
	 */
	public TimeserieWrapper GetTSWrapper()
	{
		return new TimeserieWrapper(	m_TableName,
 										"TS_C_DERIVATION",
										m_ID,
										m_Name );
	}


	/**
	 * inits the members reading from database
	 */
	protected void DBRead()
	{
		try
		{
			String str = "SELECT NAME, CHANNEL_REF, SOURCE_NAME, SOURCE_REF, ";
			str += "SOURCE_TYPE, MY_SOURCE, TIMEFROM, TIMETO FROM TS_DERIVATION ";
			str += "WHERE SID = ?";

			m_stmt = Database.getConnection().prepareStatement(str);
			m_stmt.setInt(1, m_ID);

			ResultSet set = m_stmt.executeQuery();

			set.next();

			m_Name = set.getString(1);
			m_ChannelRef = set.getInt(2);
			m_SourceTableName = set.getString(3);
			m_SourceID = set.getInt(4);
			m_SourceType = set.getString(5);
			m_TableName = set.getString(6);
			m_From = set.getDate(7);
			m_To = set.getDate(8);

			set.close();
		}
		catch(Exception e)
		{
			e.printStackTrace(System.out);
		}

		super.DBRead();
	}
}
