package org.kalypso.ogc.sensor.diagview.impl;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.diagview.IAxisMapping;
import org.kalypso.ogc.sensor.diagview.IDiagramAxis;
import org.kalypso.ogc.sensor.diagview.IDiagramCurve;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplateTheme;

/**
 * Default implementation of the <code>IDiagramCurve</code> interface.
 * 
 * @author schlienger
 */
public class DiagramCurve implements IDiagramCurve
{
  private final String m_name;
  private final Properties m_mappings;
  private final IDiagramTemplate m_template;
  private IDiagramTemplateTheme m_theme;

  /**
   * Constructor
   * 
   * @param name
   * @param theme
   * @param mappings
   * @param template
   */
  public DiagramCurve( final String name, final IDiagramTemplateTheme theme, final Properties mappings,
      final IDiagramTemplate template )
  {
    m_name = name;
    m_mappings = mappings;
    m_theme = theme;
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
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    return m_name;
  }
  
  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramCurve#getMappings()
   */
  public IAxisMapping[] getMappings()
  {
    final List ms = new ArrayList();

    final IAxis[] obsAxes = m_theme.getObservation().getAxisList();
    
    for( final Iterator it = m_mappings.keySet().iterator(); it.hasNext(); )
    {
      final String strObsAxis = (String)it.next();
      final String strDiagAxis = m_mappings.getProperty( strObsAxis );

      final IAxis obsAxis = ObservationUtilities.findAxisByName( obsAxes , strObsAxis );
      final IDiagramAxis diagAxis = m_template.getDiagramAxis( strDiagAxis );

      ms.add( new AxisMapping( obsAxis, diagAxis ) );
    }

    return (IAxisMapping[])ms.toArray( new IAxisMapping[0] );
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramCurve#getTheme()
   */
  public IDiagramTemplateTheme getTheme( )
  {
    return m_theme;
  }
}