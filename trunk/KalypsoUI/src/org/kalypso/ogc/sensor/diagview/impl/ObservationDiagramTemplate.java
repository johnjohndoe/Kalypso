package org.kalypso.ogc.sensor.diagview.impl;

import java.util.Date;
import java.util.NoSuchElementException;
import java.util.Properties;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.diagview.IDiagramAxis;
import org.kalypso.ogc.sensor.diagview.IDiagramCurve;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.template.ITemplateEventListener;
import org.kalypso.ogc.sensor.template.TemplateEvent;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * A default <code>IDiagramTemplate</code> that works directly with an <code>IObservation</code>.
 * 
 * @author schlienger
 */
public class ObservationDiagramTemplate implements IDiagramTemplate
{
  private final DefaultDiagramTemplate m_template;

  private final DiagramAxis m_dateAxis;

  private final DiagramAxis m_valueAxis;

  public ObservationDiagramTemplate()
  {
    m_dateAxis = new DiagramAxis( "d", "xs:date", "Datum", "", IDiagramAxis.DIRECTION_HORIZONTAL,
        IDiagramAxis.POSITION_BOTTOM, false );
    m_valueAxis = new DiagramAxis( "v", "xs:double", "Wert", "", IDiagramAxis.DIRECTION_VERTICAL,
        IDiagramAxis.POSITION_LEFT, false );

    m_template = new DefaultDiagramTemplate( "", "", true );
    m_template.addAxis( m_dateAxis );
    m_template.addAxis( m_valueAxis );
  }

  /**
   * Sets the observation used by this template.
   * 
   * @param obs
   * @param args
   */
  public void setObservation( final IObservation obs, final IVariableArguments args )
  {
    m_template.removeAllCurves();
    m_template.setTitle( obs.getName() );
    
    final IAxis[] valueAxis = ObservationUtilities.findAxisByClass( obs.getAxisList(), Number.class );
    final IAxis dateAxis = ObservationUtilities.findAxisByClass( obs.getAxisList(), Date.class )[0];

    for( int i = 0; i < valueAxis.length; i++ )
    {
      if( !KalypsoStatusUtils.isStatusAxis( valueAxis[i] ) )
      {
        final Properties mappings = new Properties();
        mappings.setProperty( dateAxis.getLabel(), "d" );
        mappings.setProperty( valueAxis[i].getLabel(), "v" );

        final DiagramCurve curve = new DiagramCurve( valueAxis[i].getLabel(), obs, mappings, this, args );

        m_template.addCurve( curve );
      }
    }
  }

  public void addAxis( IDiagramAxis axis )
  {
    m_template.addAxis( axis );
  }

  public void addCurve( IDiagramCurve curve )
  {
    m_template.addCurve( curve );
  }

  public void addTemplateEventListener( ITemplateEventListener l )
  {
    m_template.addTemplateEventListener( l );
  }

  public IDiagramAxis findAxis( String id ) throws NoSuchElementException
  {
    return m_template.findAxis( id );
  }

  public void fireTemplateChanged( TemplateEvent evt )
  {
    m_template.fireTemplateChanged( evt );
  }

  public IDiagramAxis[] getAxisList()
  {
    return m_template.getAxisList();
  }

  public IDiagramCurve[] getCurveList()
  {
    return m_template.getCurveList();
  }

  public String getLegendName()
  {
    return m_template.getLegendName();
  }

  public String getTitle()
  {
    return m_template.getTitle();
  }

  public boolean isShowLegend()
  {
    return m_template.isShowLegend();
  }

  public void removeAllCurves()
  {
    m_template.removeAllCurves();
  }

  public void removeAxis( IDiagramAxis axis )
  {
    m_template.removeAxis( axis );
  }

  public void removeCurve( IDiagramCurve curve )
  {
    m_template.removeCurve( curve );
  }

  public void removeTemplateEventListener( ITemplateEventListener l )
  {
    m_template.removeTemplateEventListener( l );
  }

  public void setLegendName( String name )
  {
    m_template.setLegendName( name );
  }

  public void setShowLegend( boolean show )
  {
    m_template.setShowLegend( show );
  }

  public void setTitle( String title )
  {
    m_template.setTitle( title );
  }

  public String toString()
  {
    return m_template.toString();
  }
}