package com.bce.datacenter.db.persistent;

import java.sql.Connection;

/**
 * Common base class for all persistent objects
 * 
 * @author Marc Schlienger
 */
public abstract class Persistent
{
  /** database identifier */
  protected final int m_ID;
  
  /** connection to db */
  protected final Connection m_con;

  /**
   * database based constructor knowing id of this object. Will call dbRead to
   * fetch this from the db.
   * 
   * @param con
   * @param id
   *          database identifier
   * @param fetchFromDB
   */
  public Persistent( final Connection con, final int id, final boolean fetchFromDB )
  {
    m_con = con;
    m_ID = id;

    if( fetchFromDB )
      dbRead();
  }

  /**
   * Returns the db identifier
   * 
   * @return Returns the db identifier
   */
  public int getID( )
  {
    return m_ID;
  }

  /**
   * read this object from the database, init members with database information
   */
  protected abstract void dbRead( );
}