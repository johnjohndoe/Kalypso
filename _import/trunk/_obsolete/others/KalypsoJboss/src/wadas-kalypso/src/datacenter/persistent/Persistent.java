package datacenter.persistent;

import java.sql.PreparedStatement;

/**
 * Common base class for all persistent objects.
 * <br>
 * A call to PSynchronise() takes care of either performing a INSERT INTO if the
 * object is new, or a UPDATE ... VALUES(...) if the object already exists.
 *
 * @author Marc Schlienger
 */
public abstract class Persistent
{
	public static final int INVALID = 10;
	public static final int LOADED = 20;
	public static final int NEW = 30;
	public static final int MODIFIED = 40;
	public static final int DELETED = 50;

	/**
	 * database identifier
	 */
	protected int m_ID;

	/**
	 * status of this object
	 * - invalid
	 * - loaded
	 * - new
	 * - modified
	 * - deleted
	 */
	protected int m_Status;

	protected PreparedStatement m_stmt = null;

	/**
	 * default constructor provided as convenience. Does nothing.
	 */
	public Persistent()
	{
	}

	/**
	 * database based constructor knowing id of this object.
	 */
	public Persistent(int id)
	{
		this();

		m_ID = id;
		m_Status = Persistent.INVALID;
	}

	/**
	 * read this object from the database, init members with database information
	 */
	protected void DBRead()
	{
		m_Status = Persistent.LOADED;
	}

	/**
	 * update database
	 */
	protected void DBUpdate()
	{
		m_Status = Persistent.LOADED;
	}

	/**
	 * delete this object from the database
	 */
	protected void DBDelete()
	{
		m_Status = Persistent.INVALID;
	}

	/**
	 * insert this object into the database
	 */
	protected void DBCreate()
	{
		m_Status = Persistent.LOADED;
	}

	/**
	 * convenience method for loading objects from database, equivalent to read.
	 */
	public void PLoad() throws PersistentException
	{
		if(m_Status == Persistent.INVALID)
		{
			DBRead();
		}
		else
		{
			throw new PersistentException("ERROR - DL_C_Persistent.PLoad() - called for an object which status is not invalid.");
		}
	}

	/**
	 * object has been modified, call this method for consistency checks
	 */
	protected void PModify() throws PersistentException
	{
		if(m_Status == Persistent.INVALID)
		{
			throw new PersistentException(m_ID);
		}
		else if(m_Status == Persistent.DELETED)
		{
			throw new PersistentException(m_ID);
		}
		else if(m_Status != Persistent.NEW)
		{
			m_Status = Persistent.MODIFIED;
		}
	}

	/**
	 * method called when object is new and can be created into database
	 */
	protected void PCreate()
	{
		m_Status = Persistent.NEW;
	}

	/**
	 * method called when object needs to be deleted
	 */
	public void PDelete() throws PersistentException
	{
		if(m_Status == Persistent.NEW)
		{
			m_Status = INVALID;
		}
		else if(m_Status == Persistent.INVALID)
		{
			throw new PersistentException(m_ID);
		}
		else
		{
			m_Status = Persistent.DELETED;
		}
	}

	/**
	 * call this method when object gets invalid (concerning synchronisation with database)
	 */
	protected void PInvalidate()
	{
		m_Status = Persistent.INVALID;
	}

	/**
	 * synchronise this object with the database
	 */
	public void PSynchronise()
	{
		switch(m_Status)
		{
			case Persistent.NEW:
			{
				DBCreate();
				break;
			}

			case Persistent.MODIFIED:
			{
				DBUpdate();
				break;
			}

			case Persistent.DELETED:
			{
				DBDelete();
				break;
			}
		}
	}

	/**
	 * @return object identifier
	 */
	public int PGetID()
	{
		return m_ID;
	}

	/**
	 * @return object status
	 */
	protected int PGetStatus()
	{
		return m_Status;
	}
}
