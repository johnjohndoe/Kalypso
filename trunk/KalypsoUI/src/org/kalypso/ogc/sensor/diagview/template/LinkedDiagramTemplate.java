package org.kalypso.ogc.sensor.diagview.template;

import java.net.URL;
import java.util.Iterator;
import java.util.NoSuchElementException;

import org.kalypso.ogc.sensor.diagview.IDiagramAxis;
import org.kalypso.ogc.sensor.diagview.IDiagramCurve;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.impl.DiagramAxis;
import org.kalypso.ogc.sensor.diagview.impl.DiagramTemplate;
import org.kalypso.ogc.sensor.template.ITemplateEventListener;
import org.kalypso.ogc.sensor.template.ObservationTemplateHelper;
import org.kalypso.ogc.sensor.template.TemplateEvent;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.template.obsdiagview.TypeAxis;

/**
 * @author schlienger
 */
public class LinkedDiagramTemplate implements IDiagramTemplate
{
  private final DiagramTemplate m_template;

  private final URL m_context;

  public LinkedDiagramTemplate( final ObsdiagviewType obsDiagView,
      final URL context )
  {
    m_context = context;

    final String title = obsDiagView.getTitle();

    final ObsdiagviewType.LegendType l = obsDiagView.getLegend();
    final String legendTitle = l == null ? "" : l.getTitle();
    final boolean showLegend = l == null ? false : l.isVisible();

    m_template = new DiagramTemplate( title, legendTitle, showLegend );

    for( final Iterator it = obsDiagView.getAxis().iterator(); it.hasNext(); )
    {
      final TypeAxis baseAxis = (TypeAxis) it.next();

      m_template.addAxis( new DiagramAxis( baseAxis ) );
    }

    for( final Iterator it = obsDiagView.getCurve().iterator(); it.hasNext(); )
    {
      final ObsdiagviewType.CurveType baseCurve = (ObsdiagviewType.CurveType) it
          .next();

      ObservationTemplateHelper.createCurve( baseCurve, this, context );
    }
  }

  public boolean equals( Object obj )
  {
    return m_template.equals( obj );
  }

  public IDiagramAxis[] getAxisList( )
  {
    return m_template.getAxisList();
  }

  public IDiagramCurve[] getCurveList( )
  {
    return m_template.getCurveList();
  }

  public String getLegendName( )
  {
    return m_template.getLegendName();
  }

  public String getTitle( )
  {
    return m_template.getTitle();
  }

  public int hashCode( )
  {
    return m_template.hashCode();
  }

  public boolean isShowLegend( )
  {
    return m_template.isShowLegend();
  }

  public String toString( )
  {
    return m_template.toString();
  }

  public void addAxis( IDiagramAxis axis )
  {
    m_template.addAxis( axis );
  }

  public void addCurve( IDiagramCurve curve )
  {
    m_template.addCurve( curve );
  }

  public void removeAxis( IDiagramAxis axis )
  {
    m_template.removeAxis( axis );
  }

  public void removeCurve( IDiagramCurve curve )
  {
    m_template.removeCurve( curve );
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

  public IDiagramAxis findAxis( String id ) throws NoSuchElementException
  {
    return m_template.findAxis( id );
  }

  public void addTemplateEventListener( ITemplateEventListener l )
  {
    m_template.addTemplateEventListener( l );
  }

  public void fireTemplateChanged( TemplateEvent evt )
  {
    m_template.fireTemplateChanged( evt );
  }

  public void removeTemplateEventListener( ITemplateEventListener l )
  {
    m_template.removeTemplateEventListener( l );
  }

  public void removeAllCurves( )
  {
    m_template.removeAllCurves();
  }
}