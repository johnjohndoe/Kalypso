package datacenter.zeitreihen;

import java.sql.*;
import java.util.Vector;

import datacenter.persistent.*;
import datacenter.tree.Container;


/**
 * Channel is a specific DataObject owned by a Container.
 *
 * @author Marc Schlienger
 * @see datacenter.persistent.Persistent
 * @see datacenter.tree.Container
 * @see Derivation
 * @see TimeserieWrapper
 */
public class Channel extends Persistent
{
	/** reference to container object
	 */
	private int m_co;
	/** name of this Derivation
	 */
	private String m_Name;
	/** arbitrary description
	 */
	private String m_Description;
	/** separate identifier used for user own needs
	 */
	private String m_Identifier;

	/** array of derivations, used as a way of caching
	 */
	private Derivation[] m_ders = null;

	/**
	 * constructor for existing object to build from database
	 */
	public Channel(int id)
	{
		super(id);
	}

	/**
	 * @return name of the table in which the "work" timeseries are stored
	 */
	public String GetWorkName()
	{
		return "TS_TSWORK_" + m_Identifier;
	}


	/**
	 * @return name of the table in which the "original" timeseries are stored
	 */
	public String GetOriginalName()
	{
		return "TS_TSORIGINAL_" + m_Identifier;
	}

	/**
	 * builds a wrapper for the original timeseries owned by this channel
	 *
	 * @return TimeserieWrapper of the original timeseries
	 */
	public TimeserieWrapper GetTSOriginal()
	{
		return new TimeserieWrapper( GetOriginalName(),
						 "ORIGINAL",
						 m_ID,
				 		 m_Name + " (org)" );
	}

	/**
	 * builds a wrapper for the work version of the timeseries owned by this channel
	 *
	 * @return TimeserieWrapper of the work timeseries
	 */
	public TimeserieWrapper GetTSWork()
	{
		return new TimeserieWrapper( GetWorkName(),
						 "WORK",
						 m_ID,
				 		 m_Name + " (work)" );
	}

	/**
	 * @return reference to Container
	 */
	public int GetContainerObjectRef()
	{
		return m_co;
	}

	public String GetName()
	{
		return m_Name;
	}

	public String toString()
	{
		return "Channel: " + m_Name;
	}

	public String GetDescription()
	{
		return m_Description;
	}

	public String GetIdentifier()
	{
		return m_Identifier;
	}

	/**
	 * builds an array of Derivation that this Channel owns.
	 *
	 * @param refresh	set it to true if you want to repoll the database, else it will use the cached ones.
	 * @return array of Derivation
	 */
	public Derivation[] GetDerivations()
	{
		if(m_ders != null)
			return m_ders;

		try
		{
			Vector v = new Vector();

			m_stmt = Database.getConnection().prepareStatement("SELECT SID FROM TS_DERIVATION WHERE CHANNEL_REF = ? ORDER BY NAME");
			m_stmt.setInt(1, m_ID);

			ResultSet set = m_stmt.executeQuery();

			while( set.next() )
			{
				Derivation c = new Derivation(set.getInt(1));
				c.PLoad();
				v.add(c);
			}

			set.close();
			Database.getConnection().commit();

			m_ders = (Derivation[]) v.toArray(new Derivation[0]);
		}
		catch(Exception e)
		{
			e.printStackTrace(System.out);
		}

		return m_ders;
	}


	/**
	 * read from db to init members
	 */
	protected void DBRead()
	{
		try
		{
			m_stmt = Database.getConnection().prepareStatement("SELECT NAME, DESCRIPTION, IDENTIFIER, COREF FROM TS_CHANNEL WHERE ID = ?");
			m_stmt.setInt(1, m_ID);

			ResultSet set = m_stmt.executeQuery();

			set.next();

			m_Name = set.getString(1);
			m_Description = set.getString(2);
			m_Identifier = set.getString(3);
			m_co = set.getInt(4);

			set.close();
		}
		catch(Exception e)
		{
			e.printStackTrace(System.out);
		}

		super.DBRead();
	}
}
