package org.kalypso.ogc.sensor.diagview;

import org.kalypso.ogc.sensor.IAxis;

/**
 * Default implementation of <code>IAxisMapping</code>.
 * 
 * @author schlienger
 *
 */
public class AxisMapping implements IAxisMapping
{
  private final IAxis m_oAxis;
  private final IDiagramAxis m_dAxis;

  public AxisMapping( final IAxis oAxis, final IDiagramAxis dAxis )
  {
    m_oAxis = oAxis;
    m_dAxis = dAxis;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IAxisMapping#getObservationAxis()
   */
  public IAxis getObservationAxis()
  {
    return m_oAxis;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IAxisMapping#getDiagramAxis()
   */
  public IDiagramAxis getDiagramAxis()
  {
    return m_dAxis;
  }
}
