package org.kalypso.ogc.sensor.diagview.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.IObservationListener;
import org.kalypso.ogc.sensor.diagview.IDiagramAxis;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplateTheme;
import org.kalypso.ogc.sensor.template.AbstractTemplateEventProvider;
import org.kalypso.ogc.sensor.template.TemplateEvent;

/**
 * Default implementation of the <code>IDiagramTemplate</code> interface.
 * 
 * @author schlienger
 */
public class DefaultDiagramTemplate extends AbstractTemplateEventProvider implements IDiagramTemplate, IObservationListener
{
  private String m_title;

  private String m_legendName;

  private boolean m_showLegend;

  private final Map m_axesMap = new Hashtable();

  private final Map m_themesMap = new Hashtable();

  /**
   * Constructor
   * 
   * @param title
   * @param legendName
   * @param showLegend
   */
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
   * @see java.lang.Object#toString()
   */
  public String toString( )
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
  public Collection getDiagramAxes( )
  {
    return m_axesMap.values();
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#getCurves()
   */
  public Collection getCurves( )
  {
    final Collection allCurves = new ArrayList();
    for( final Iterator iter = m_themesMap.values().iterator(); iter.hasNext(); )
    {
      final Collection curves = (Collection) iter.next();
      allCurves.addAll( curves );
    }
    
//    return m_themesMap.values();
    return allCurves;
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

  /**
   * Adds the given axis.
   * 
   * @param axis
   */
  public void addAxis( IDiagramAxis axis )
  {
    m_axesMap.put( axis.getIdentifier(), axis );
  }

  /**
   * Removes the given axis or throws IllegalArgumentException when no such axis.
   * 
   * @param axis
   * @throws IllegalArgumentException
   */
  public void removeAxis( IDiagramAxis axis ) throws IllegalArgumentException
  {
    if( m_axesMap.get( axis.getIdentifier() ) != null )
      m_axesMap.remove( axis.getIdentifier() );
    else
      throw new IllegalArgumentException( "Axis could not be found: " + axis );
  }
  
  /**
   * Removes all axes
   */
  public void removeAllAxes()
  {
    m_axesMap.clear();
  }

  public void addTheme( final IDiagramTemplateTheme theme )
  {
    m_themesMap.put( theme, theme.getCurves() );

    // register as listener on the observation
    theme.getObservation().addListener( this );
    
    final Iterator it = theme.getCurves().iterator();
    while( it.hasNext() )
      fireTemplateChanged( new TemplateEvent( this, it.next(), TemplateEvent.TYPE_ADD) );
  }
  
  public void removeTheme( IDiagramTemplateTheme theme )
  {
    final Iterator it = theme.getCurves().iterator();
    while( it.hasNext() )
      fireTemplateChanged( new TemplateEvent( this, it.next(), TemplateEvent.TYPE_REMOVE ) );
    
    // unregister listener
    theme.getObservation().removeListener( this );
    
    m_themesMap.remove( theme );
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#getDiagramAxis(java.lang.String)
   */
  public IDiagramAxis getDiagramAxis( final String diagAxisId )
  {
    return (IDiagramAxis) m_axesMap.get( diagAxisId );
  }

  /**
   * Removes all the themes and fires event
   */
  public void removeAllThemes()
  {
    clearObsListener();
    
    m_themesMap.clear();
    
    m_axesMap.clear();
    
    fireTemplateChanged( new TemplateEvent( this, null, TemplateEvent.TYPE_REMOVE_ALL) );
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#dispose()
   */
  public void dispose( )
  {
    clearObsListener();
    
    m_axesMap.clear();
    
    final Iterator it = m_themesMap.keySet().iterator();
    while( it.hasNext() )
      ((IDiagramTemplateTheme) it.next()).dispose();
    
    m_themesMap.clear();
  }
  
  /**
   * Removes this from the listeners for the observations of this' themes
   */
  private void clearObsListener( )
  {
    for( final Iterator it = getThemes().iterator(); it.hasNext(); )
    {
      final IDiagramTemplateTheme theme = (IDiagramTemplateTheme) it.next();
      theme.getObservation().removeListener( this );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramTemplate#getThemes()
   */
  public Collection getThemes( )
  {
    return m_themesMap.keySet();
  }

  /**
   * Finds a theme for the given observation.
   * 
   * @param obs
   * @return theme or null if not found
   */
  public IDiagramTemplateTheme findTheme( final IObservation obs )
  {
    final Iterator it = getThemes().iterator();
    while( it.hasNext() )
    {
      final IDiagramTemplateTheme theme = (IDiagramTemplateTheme) it.next();

      if( theme.getObservation().equals( obs ) )
        return theme;
    }

    return null;
  }
  
  /**
   * @see org.kalypso.ogc.sensor.IObservationListener#observationChanged(org.kalypso.ogc.sensor.IObservation)
   */
  public void observationChanged( IObservation obs )
  {
    final IDiagramTemplateTheme theme = findTheme( obs );

    if( theme != null )
      fireTemplateChanged( new TemplateEvent( theme, TemplateEvent.TYPE_REFRESH ) );
  }
}