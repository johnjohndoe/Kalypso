package org.kalypso.observation.table;

import org.kalypso.commons.tuple.IRowKey;
import org.kalypso.commons.tuple.impl.SimpleKey;

/**
 * Represent a simple row key based on a unique value
 */
public final class MTRMRow extends SimpleKey implements IRowKey
{
  final Object m_value;

  public MTRMRow( final Object value )
  {
    super( value.toString() );
    
    m_value = value;
  }
  
  public Object getValue( )
  {
    return m_value;
  }
}