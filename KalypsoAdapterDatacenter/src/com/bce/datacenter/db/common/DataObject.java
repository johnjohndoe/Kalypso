package com.bce.datacenter.db.common;

import java.sql.Connection;

import com.bce.datacenter.db.persistent.Persistent;

/**
 * Represents a DataObject
 * 
 * @author schlienger
 */
public abstract class DataObject extends Persistent
{
  /** arbitrary description */
  protected String m_description;

  /** name of this Derivation */
  protected String m_name;

  /** reference to some owner (ex: a level) */
  protected int m_ownerRef;

  public DataObject( final Connection con, int id )
  {
    super( con, id, true );
  }

  public DataObject( final Connection con, int id, String name, String description )
  {
    super( con, id, false );

    m_name = name;
    m_description = description;
  }

  public String getDescription( )
  {
    return m_description;
  }

  public String getName( )
  {
    return m_name;
  }

  public Persistent getOwner( )
  {
    return DataObjectFactory.getOwner( m_con, m_ownerRef );
  }

  @Override
  public String toString( )
  {
    return m_name;
  }
}