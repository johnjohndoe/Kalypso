package com.bce.datacenter.ingres;

/**
 * Common base class for all persistent objects
 *
 * @author Marc Schlienger
 */
public abstract class Persistent
{
	/** database identifier */
	protected final int m_ID;

	/**
	 * database based constructor knowing id of this object. Will call dbRead to
	 * fetch this from the db. 
	 *
	 * @param id database identifier
	 */
	public Persistent( final int id, final boolean fetchFromDB )
	{
		m_ID = id;

		if( fetchFromDB )
			dbRead(  );
	}
	
	/**
	 * Returns the db identifier
	 *
	 * @return
	 */
	public int getID(  )
	{
		return m_ID;
	}

	/**
	 * read this object from the database, init members with database information
	 */
	protected abstract void dbRead(  );
	
}
