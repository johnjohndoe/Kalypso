package org.kalypso.ogc.sensor.diagview.impl;

import java.awt.Paint;

import org.kalypso.ogc.sensor.diagview.IAxisMapping;
import org.kalypso.ogc.sensor.diagview.IDiagramCurve;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplateTheme;
import org.kalypso.ogc.sensor.template.TemplateEvent;

/**
 * Default implementation of the <code>IDiagramCurve</code> interface.
 * 
 * @author schlienger
 */
public class DiagramCurve implements IDiagramCurve
{
  private String m_name;

  private final IDiagramTemplate m_template;

  private final IAxisMapping[] m_mappings;

  private IDiagramTemplateTheme m_theme;

  private final Paint m_paint;

  private boolean m_shown = true;

  /**
   * Constructor
   * 
   * @param name
   * @param paint
   * @param theme
   * @param mappings
   * @param template
   */
  public DiagramCurve( final String name, final Paint paint,
      final IDiagramTemplateTheme theme, final IAxisMapping[] mappings,
      final IDiagramTemplate template )
  {
    m_name = name;
    m_paint = paint;
    m_mappings = mappings;
    m_theme = theme;
    m_template = template;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramCurve#getName()
   */
  public String getName( )
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
  public IAxisMapping[] getMappings( )
  {
    //    final List ms = new ArrayList();
    //
    //    final IAxis[] obsAxes = m_theme.getObservation().getAxisList();
    //    
    //    for( final Iterator it = m_mappings.keySet().iterator(); it.hasNext(); )
    //    {
    //      final String strObsAxis = (String)it.next();
    //      final String strDiagAxis = m_mappings.getProperty( strObsAxis );
    //
    //      final IAxis obsAxis = ObservationUtilities.findAxisByName( obsAxes ,
    // strObsAxis );
    //      final IDiagramAxis diagAxis = m_template.getDiagramAxis( strDiagAxis );
    //
    //      ms.add( new AxisMapping( obsAxis, diagAxis ) );
    //    }
    //
    //    return (IAxisMapping[])ms.toArray( new IAxisMapping[0] );
    return m_mappings;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramCurve#getTheme()
   */
  public IDiagramTemplateTheme getTheme( )
  {
    return m_theme;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramCurve#setName(java.lang.String)
   */
  public void setName( String name )
  {
    m_name = name;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramCurve#getPaint()
   */
  public Paint getPaint( )
  {
    return m_paint;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramCurve#isShown()
   */
  public boolean isShown( )
  {
    return m_shown;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramCurve#setShown(boolean)
   */
  public void setShown( boolean shown )
  {
    // only set if necessary
    if( shown != m_shown )
    {
      m_shown = shown;

      m_template.fireTemplateChanged( new TemplateEvent( this,
          TemplateEvent.TYPE_SHOW_STATE ) );
    }
  }
}