package org.kalypso.ogc.sensor.diagview.impl;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.diagview.IAxisMapping;
import org.kalypso.ogc.sensor.diagview.IDiagramCurve;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;

/**
 * Default implementation of the <code>IDiagramCurve</code> interface.
 * 
 * @author schlienger
 */
public class DiagramCurve implements IDiagramCurve
{
  private final String m_name;

  private final Properties m_mappings;

  private IObservation m_obs = null;

  private final IDiagramTemplate m_template;

  /**
   * Constructor
   * 
   * @param name
   * @param obs optional, can be null, but diag template cannot be shown until setObservation is called for each curve.
   * @param mappings
   * @param template
   */
  public DiagramCurve( final String name, final IObservation obs, final Properties mappings,
      final IDiagramTemplate template )
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
    final List ms = new ArrayList();

    for( final Iterator it = m_mappings.keySet().iterator(); it.hasNext(); )
    {
      final String obsAxis = (String)it.next();
      final String diagAxis = m_mappings.getProperty( obsAxis );

      ms.add( new AxisMapping( ObservationUtilities.findAxisByName( m_obs.getAxisList(), obsAxis ), m_template
          .findAxis( diagAxis ) ) );
    }

    return (IAxisMapping[])ms.toArray( new IAxisMapping[0] );
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramCurve#getObservation()
   */
  public IObservation getObservation()
  {
    return m_obs;
  }
  
  /**
   * @param obs The obs to set.
   */
  public void setObservation( IObservation obs )
  {
    m_obs = obs;
  }
}