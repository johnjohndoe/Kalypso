/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.diagview.impl;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.proxy.IProxyFactory;
import org.kalypso.ogc.sensor.template.AbstractViewTemplate;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.template.obsdiagview.TypeAxis;
import org.kalypso.template.obsdiagview.TypeObservation;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * @author schlienger
 */
public class DiagViewTemplate extends AbstractViewTemplate
{
  private String m_title;

  private String m_legendName;

  private boolean m_showLegend;

  private final Map m_axesMap = new Hashtable();

  private IProxyFactory m_factory = null;

  public DiagViewTemplate( )
  {
    this( "", "", false );
  }

  public DiagViewTemplate( final String title, final String legendName,
      final boolean showLegend )
  {
    m_title = title;
    m_legendName = legendName;
    m_showLegend = showLegend;
  }

  /**
   * @see org.kalypso.ogc.sensor.template.AbstractViewTemplate#dispose()
   */
  public void dispose( )
  {
    m_factory = null;
    m_axesMap.clear();

    super.dispose();
  }
  
  /**
   * @see org.kalypso.ogc.sensor.template.AbstractViewTemplate#removeAllThemes()
   */
  public void removeAllThemes( )
  {
    m_axesMap.clear();
    
    super.removeAllThemes();
  }

  public String getTitle( )
  {
    return m_title;
  }

  public String toString( )
  {
    return m_title;
  }

  public String getLegendName( )
  {
    return m_legendName;
  }

  public boolean isShowLegend( )
  {
    return m_showLegend;
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

  public Collection getDiagramAxes( )
  {
    return m_axesMap.values();
  }

  public void addAxis( DiagramAxis axis )
  {
    m_axesMap.put( axis.getIdentifier(), axis );
  }

  /**
   * Removes the given axis or throws IllegalArgumentException when no such
   * axis.
   * 
   * @param axis
   * @throws IllegalArgumentException
   */
  public void removeAxis( DiagramAxis axis ) throws IllegalArgumentException
  {
    if( m_axesMap.get( axis.getIdentifier() ) != null )
      m_axesMap.remove( axis.getIdentifier() );
    else
      throw new IllegalArgumentException( "Axis could not be found: " + axis );
  }

  public void removeAllAxes( )
  {
    m_axesMap.clear();
  }

  public DiagramAxis getDiagramAxis( final String diagAxisId )
  {
    return (DiagramAxis) m_axesMap.get( diagAxisId );
  }

  public Collection getCurves( )
  {
    final Collection allCurves = new ArrayList();
    for( final Iterator iter = getThemes().iterator(); iter.hasNext(); )
      allCurves.addAll( ((DiagViewTheme) iter.next()).getCurves() );

    return allCurves;
  }

  /**
   * Also removes the axes, and sets the title according to the observation
   * 
   * @see org.kalypso.ogc.sensor.template.AbstractViewTemplate#setObservation(org.kalypso.ogc.sensor.IObservation,
   *      org.kalypso.util.runtime.IVariableArguments)
   */
  public void setObservation( final IObservation obs,
      final IVariableArguments args )
  {
    removeAllAxes();
    setTitle( obs.getName() );

    super.setObservation( obs, args );
  }

  /**
   * Adds an observation and its values as columns to this template.
   * 
   * @param obs
   * @param args
   */
  public void addObservation( final IObservation obs,
      final IVariableArguments args )
  {
    final DiagViewTheme theme = new DiagViewTheme( this, null, args );

    // the theme should be created using the default properties of the obs
    theme.setUseDefault( true );
    // and following type should be ignored
    theme.setIgnoreType( getIgnoreType() );

    addTheme( theme );

    theme.setObservation( obs );
  }

  /**
   * Sets the base template
   * 
   * @param obsDiagView
   * @param context
   */
  public void setBaseTemplate( final ObsdiagviewType obsDiagView,
      final URL context )
  {
    setTitle( obsDiagView.getTitle() );
    setLegendName( obsDiagView.getLegend() == null ? "" : obsDiagView
        .getLegend().getTitle() );
    setShowLegend( obsDiagView.getLegend() == null ? false : obsDiagView
        .getLegend().isVisible() );

    // axes spec is optional
    if( obsDiagView.getAxis() != null )
    {
      for( final Iterator it = obsDiagView.getAxis().iterator(); it.hasNext(); )
      {
        final TypeAxis baseAxis = (TypeAxis) it.next();

        addAxis( new DiagramAxis( baseAxis ) );
      }
    }

    final List list = obsDiagView.getObservation();
    for( final Iterator it = list.iterator(); it.hasNext(); )
    {
      final TypeObservation tobs = (TypeObservation) it.next();

      final DiagViewTheme theme = new DiagViewTheme( this, null, tobs );
      addTheme( theme );

      final PoolableObjectType key = new PoolableObjectType(
          tobs.getLinktype(), tobs.getHref(), context );
      theme.loadObservation( key );
    }
  }

  /**
   * Convenienve method for adding an observation to this template.
   * 
   * @param themeName
   *          used as part of the col name if not null
   * @param context
   * @param href
   * @param linktype
   * @param ignoreExceptions
   * @param args
   */
  public void addObservation( final String themeName, final URL context,
      final String href, final String linktype, final boolean ignoreExceptions,
      final IVariableArguments args )
  {
    // create key according to observation link
    final PoolableObjectType key = new PoolableObjectType( linktype, href,
        context, ignoreExceptions );

    final DiagViewTheme theme = new DiagViewTheme( this, themeName, args );

    // the theme should be created using the default properties of the obs
    theme.setUseDefault( true );
    // and following type should be ignored
    theme.setIgnoreType( getIgnoreType() );

    addTheme( theme );

    theme.loadObservation( key );
  }

  /**
   * Sets the proxy factory to use when observations are resolved. Clients can
   * set a custom factory so that observations can be extended by functionality
   * provided in the proxy-observation.
   * <p>
   * By default, the factory for this class is null. At every time, you can
   * reset the factory to null, in that case no factory will be used.
   * 
   * @param factory
   *          [null allowed]
   */
  public void setProxyFactory( final IProxyFactory factory )
  {
    m_factory = factory;
  }

  public IProxyFactory getProxyFactory( )
  {
    return m_factory;
  }
}