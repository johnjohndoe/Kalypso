package org.kalypso.ogc.sensor.diagview;


/**
 * A default curve for an observation axis.
 * 
 * @author schlienger
 *
 */
public class DefaultCurve implements ICurve
{
  private final IAxisMapping[] m_mappings;
  private final String m_name;

  public DefaultCurve( final String name, final IAxisMapping[] mappings )
  {
    m_name = name;
    m_mappings = mappings;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.ICurve#getName()
   */
  public String getName()
  {
    return m_name;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.ICurve#getMappings()
   */
  public IAxisMapping[] getMappings()
  {
    return m_mappings;
  }
}
