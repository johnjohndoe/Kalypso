
package datacenter.tree;

import java.sql.*;
import java.util.Vector;

import datacenter.persistent.*;
import datacenter.zeitreihen.*;

/**
 * A Container holds data. In Flomatis it normaly holds descriptional data, two-dimensional
 * process data, and one-dimensional process data.
 *
 * For the sake of this interface to Flomatis, we only provide the one-dimensional process data
 * functionality, that is the timeseries.
 *
 * A Container can have zero, one, or more channel objects.
 *
 * @see datacenter.persistent.Persistent
 * @see datacenter.tree.Level
 * @see datacenter.zeitreihen.Channel
 */
public class Container extends Persistent
{
	/** name of this container, usually shown in the list view of containers a Level has
	 */
	private String m_Name;
	/** arbitrary description
	 */
	private String m_Description;
	/** reference to parent Level
	 */
	private int m_LevelRef;
	/** reference to type, unused outside of flomatis
	 */
	private int m_COTypeRef;
	/** cached Channels that belong to this Container
	 */
	private Channel[] m_Channels = null;
	/** identifier of the default container object type
	 * used within this interface for the kalypso software.
	 * it is important that there is some type with this
	 * identifier, else it won't work.
	 */
	private static final int DEFAULTCOTYPEREF = 1;


	/**
	 * constructor for building a Container from the db
	 */
	public Container(int id)
	{
		super(id);
	}

	public String GetObjectName()
	{
		return m_Name;
	}

	public String GetObjectDesc()
	{
		return m_Description;
	}

	public String toString()
	{
		return "Container: " + m_Name;
	}

	public int GetCOType()
	{
		return m_COTypeRef;
	}

	/**
	 * @return parent Level
	 */
	public Level GetLevel()
	{
		Level l = new Level(m_LevelRef);

		try{

			l.PLoad();

			return l;

		}catch(PersistentException e)
		{
			e.printStackTrace(System.out);
			return null;
		}
	}

	/**
	 * builds an array of the contained data objects (channel)
	 *
	 * @return array of Channel
	 */
	public Channel[] GetChannels()
	{
		if( m_Channels != null)
			return m_Channels;

		try
		{
			Vector v = new Vector();

			m_stmt = Database.getConnection().prepareStatement("SELECT ID	FROM TS_CHANNEL	WHERE COREF = ? ORDER BY NAME");
			m_stmt.setInt(1, m_ID);

			ResultSet set = m_stmt.executeQuery();

			while( set.next() == true )
			{
				Channel c = new Channel(set.getInt(1));
				c.PLoad();

				v.add(c);
			}

			set.close();
			Database.getConnection().commit();

			m_Channels = (Channel[])v.toArray(new Channel[0]);
		}
		catch(Exception e)
		{
			e.printStackTrace(System.out);
		}

		return m_Channels;
	}

	/**
	 * read from db and init members
	 */
	public void DBRead()
	{
		try
		{
			m_stmt = Database.getConnection().prepareStatement("SELECT LVLID, OBJNAME, OBJDESCRIPTION, OBJTYPE FROM DC_CONTAINEROBJECT WHERE COID = ?");
			m_stmt.setInt(1, m_ID);

			ResultSet set = m_stmt.executeQuery();

			set.next();

			m_LevelRef = set.getInt(1);
			m_Name = set.getString(2);
			m_Description = set.getString(3);
			m_COTypeRef = set.getInt(4);

			set.close();
		}
		catch(Exception e)
		{
			e.printStackTrace(System.out);
		}

		super.DBRead();
	}
}
