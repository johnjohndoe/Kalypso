package org.kalypso.ogc.sensor.diagview;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ObservationUtilities;

/**
 * @author schlienger
 */
public class DiagramCurve implements IDiagramCurve
{
  private final String m_name;

  private final Properties m_mappings;

  private final IObservation m_obs;

  private final IDiagramTemplate m_template;

  /**
   *  
   */
  public DiagramCurve( final String name, final IObservation obs, final Properties mappings, final IDiagramTemplate template )
  {
    m_name = name;
    m_mappings = mappings;
    m_obs = obs;
    m_template = template;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramCurve#getName()
   */
  public String getName()
  {
    return m_name;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramCurve#getMappings()
   */
  public IAxisMapping[] getMappings()
  {
    List ms = new ArrayList();
    
    for( Iterator it = m_mappings.keySet().iterator(); it.hasNext(); )
    {
      String obsAxis = (String)it.next();
      String diagAxis = m_mappings.getProperty( obsAxis );
     
      ms.add( new AxisMapping( ObservationUtilities.findAxis( m_obs, obsAxis ), m_template.findAxis( diagAxis ) ) );
    }
    
    return (IAxisMapping[])ms.toArray( new IAxisMapping[0] );
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationProvider#getObservation()
   */
  public IObservation getObservation()
  {
    return m_obs;
  }
}