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
import org.kalypso.util.runtime.IVariableArguments;

/**
 * @author schlienger
 */
public class DiagramCurve implements IDiagramCurve
{
  private final String m_name;

  private final Properties m_mappings;

  private final IObservation m_obs;

  private final IDiagramTemplate m_template;

  private IVariableArguments m_args = null;

  /**
   * Constructor
   * 
   * @param name
   * @param obs
   * @param mappings
   * @param template
   * @param args
   */
  public DiagramCurve( final String name, final IObservation obs, final Properties mappings,
      final IDiagramTemplate template, final IVariableArguments args )
  {
    m_name = name;
    m_mappings = mappings;
    m_obs = obs;
    m_template = template;
    m_args = args;
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

      ms.add( new AxisMapping( ObservationUtilities.findAxisByName( m_obs.getAxisList(), obsAxis ), m_template
          .findAxis( diagAxis ) ) );
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

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramCurve#getArguments()
   */
  public IVariableArguments getArguments()
  {
    return m_args;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramCurve#setArguments(org.kalypso.util.runtime.IVariableArguments)
   */
  public void setArguments( IVariableArguments args )
  {
    m_args = args;
  }
}