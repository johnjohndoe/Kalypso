package org.kalypso.ogc.sensor.diagview.impl;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import org.kalypso.ogc.sensor.diagview.IDiagramAxis;
import org.kalypso.ogc.sensor.diagview.IDiagramCurve;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.ogc.sensor.template.AbstractTemplateEventProvider;
import org.kalypso.ogc.sensor.template.TemplateEvent;

/**
 * @author schlienger
 */
public class DefaultDiagramTemplate extends AbstractTemplateEventProvider implements IDiagramTemplate
{
  private String m_title;

  private String m_legendName;

  private boolean m_showLegend;

  private final List m_axisList = new ArrayList();

  private final List m_curveList = new ArrayList();

  public DefaultDiagramTemplate( final String title, final String legendName, final boolean showLegend )
  {
    m_title = title;
    m_legendName = legendName;
    m_showLegend = showLegend;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#getTitle()
   */
  public String getTitle()
  {
    return m_title;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#getLegendName()
   */
  public String getLegendName()
  {
    return m_legendName;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#isShowLegend()
   */
  public boolean isShowLegend()
  {
    return m_showLegend;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#getAxisList()
   */
  public IDiagramAxis[] getAxisList()
  {
    return (IDiagramAxis[])m_axisList.toArray( new IDiagramAxis[0] );
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#getCurveList()
   */
  public IDiagramCurve[] getCurveList()
  {
    return (IDiagramCurve[])m_curveList.toArray( new IDiagramCurve[0] );
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#setTitle(java.lang.String)
   */
  public void setTitle( String title )
  {
    m_title = title;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#setLegendName(java.lang.String)
   */
  public void setLegendName( String name )
  {
    m_legendName = name;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#setShowLegend(boolean)
   */
  public void setShowLegend( boolean show )
  {
    m_showLegend = show;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#addAxis(org.kalypso.ogc.sensor.diagview.IDiagramAxis)
   */
  public void addAxis( IDiagramAxis axis )
  {
    m_axisList.add( axis );
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#removeAxis(org.kalypso.ogc.sensor.diagview.IDiagramAxis)
   */
  public void removeAxis( IDiagramAxis axis )
  {
    m_axisList.remove( axis );
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#addCurve(org.kalypso.ogc.sensor.diagview.IDiagramCurve)
   */
  public void addCurve( IDiagramCurve curve )
  {
    m_curveList.add( curve );
    
    fireTemplateChanged( new TemplateEvent( this, curve, TemplateEvent.TYPE_ADD) );
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#removeCurve(org.kalypso.ogc.sensor.diagview.IDiagramCurve)
   */
  public void removeCurve( IDiagramCurve curve )
  {
    m_curveList.remove( curve );
    
    fireTemplateChanged( new TemplateEvent( this, curve, TemplateEvent.TYPE_REMOVE ) );
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#findAxis(java.lang.String)
   */
  public IDiagramAxis findAxis( String id )
  {
    for( Iterator it = m_axisList.iterator(); it.hasNext(); )
    {
      IDiagramAxis axis = (IDiagramAxis)it.next();
      
      if( axis.getIdentifier().equals( id ) )
        return axis;      
    }
    
    throw new NoSuchElementException( "Axis with id not found. ID= " + id );
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#removeAllCurves()
   */
  public void removeAllCurves()
  {
    m_curveList.clear();
    
    fireTemplateChanged( new TemplateEvent( this, null, TemplateEvent.TYPE_REMOVE_ALL) );
  }
}