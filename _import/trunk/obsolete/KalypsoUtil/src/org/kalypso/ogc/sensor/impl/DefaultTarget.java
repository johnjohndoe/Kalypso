package org.kalypso.ogc.sensor.impl;

import org.kalypso.ogc.sensor.ITarget;
import org.kalypso.zml.TargetPropertyType;

/**
 * A simple implementation of ITarget.
 * 
 * @author schlienger
 */
public class DefaultTarget implements ITarget
{
  private final String m_source;
  private final String m_type;
  private final String m_identifier;

  public DefaultTarget( final TargetPropertyType tpt )
  {
    this( tpt.getSource(), tpt.getType(), tpt.getObjectid() );
  }
  
  public DefaultTarget( final String source, final String type, final String identifier )
  {
    m_source = source;
    m_type = type;
    m_identifier = identifier;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITarget#getSource()
   */
  public String getSource()
  {
    return m_source;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITarget#getType()
   */
  public String getType()
  {
    return m_type;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITarget#getIdentifier()
   */
  public String getIdentifier()
  {
    return m_identifier;
  }
}
