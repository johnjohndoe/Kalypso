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
 * Default implementation of the <code>IDiagramTemplate</code> interface.
 * 
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
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#getDiagramAxes()
   */
  public List getDiagramAxes( )
  {
    return m_axisList;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#getCurves()
   */
  public List getCurves( )
  {
    return m_curveList;
  }

  public void setTitle( String title )
  {
    m_title = title;
  }

  public void setLegendName( String name )
  {
    m_legendName = name;
  }

  public void setShowLegend( boolean show )
  {
    m_showLegend = show;
  }

  public void addAxis( IDiagramAxis axis )
  {
    m_axisList.add( axis );
  }

  public void removeAxis( IDiagramAxis axis )
  {
    m_axisList.remove( axis );
  }

  public void addCurve( IDiagramCurve curve )
  {
    m_curveList.add( curve );
    
    fireTemplateChanged( new TemplateEvent( this, curve, TemplateEvent.TYPE_ADD) );
  }

  public void removeCurve( IDiagramCurve curve )
  {
    m_curveList.remove( curve );
    
    fireTemplateChanged( new TemplateEvent( this, curve, TemplateEvent.TYPE_REMOVE ) );
  }

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

  public void removeAllCurves()
  {
    m_curveList.clear();
    
    fireTemplateChanged( new TemplateEvent( this, null, TemplateEvent.TYPE_REMOVE_ALL) );
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#dispose()
   */
  public void dispose( )
  {
    m_axisList.clear();
    m_curveList.clear();
  }
}