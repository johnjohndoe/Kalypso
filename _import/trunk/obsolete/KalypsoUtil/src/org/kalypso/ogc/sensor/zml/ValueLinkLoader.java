package org.kalypso.ogc.sensor.zml;

import java.util.List;

import org.kalypso.zml.AxisType;
import org.kalypso.zml.AxisType.ValueLinkType;

/**
 * @author schlienger
 *  
 */
public class ValueLinkLoader implements IZmlValuesLoader
{
  private final ValueLinkType m_valueLink;

  public ValueLinkLoader( final AxisType.ValueLinkType valueLink )
  {
    m_valueLink = valueLink;
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.IZmlValuesLoader#load(org.kalypso.ogc.sensor.zml.ZmlAxis)
   */
  public List load( ZmlAxis axis )
  {
    return null;
  }
}
