package com.bce.datacenter.kalypso;

import com.bce.datacenter.ingres.Persistent;


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

    /**
     * Constructor
     *
     * @param id db id
     */
    public DataObject( int id )
    {
        super( id, true );
    }

    public DataObject( int id, String name, String description )
    {
        super( id, false );

        m_name = name;
        m_description = description;
    }

    /**
     * returns the description
     *
     * @return
     */
    public String getDescription(  )
    {
        return m_description;
    }

    /**
     * returns the name
     *
     * @return
     */
    public String getName(  )
    {
        return m_name;
    }

    public Persistent getOwner(  )
    {
        return DataObjectFactory.getOwner( m_ownerRef );
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    public String toString(  )
    {
        return m_name;
    }
}
