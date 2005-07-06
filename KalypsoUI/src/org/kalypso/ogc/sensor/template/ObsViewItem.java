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
package org.kalypso.ogc.sensor.template;

import org.kalypso.contribs.eclipse.ui.IViewable;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.IObservationListener;
import org.kalypso.ogc.sensor.request.IRequest;

/**
 * Default implementation of the <code>ITableViewColumn</code> interface
 * 
 * @author schlienger
 */
public abstract class ObsViewItem implements IObsProviderListener, IObservationListener, IViewable
{
  private final ObsView m_view;

  private final IObsProvider m_obsProvider;

  private IObservation m_observation;

  private boolean m_shown = true;

  private String m_name = "";

  public ObsViewItem( final ObsView view, final IObsProvider obsProvider, final String name )
  {
    m_obsProvider = obsProvider;
    m_view = view;
    m_name = name;
    m_observation = obsProvider.getObservation();
    m_observation.addListener( this );
    obsProvider.addListener( this );
  }

  public void dispose()
  {
    m_obsProvider.removeListener( this );
    if( m_observation != null )
      m_observation.removeListener( this );
    m_obsProvider.dispose();
  }

  public String getName()
  {
    return m_name;
  }

  public String toString()
  {
    return getName();
  }

  public void setName( String name )
  {
    m_name = name;

    getView().refresh( this );
  }

  public ObsView getView()
  {
    return m_view;
  }

  public boolean isShown()
  {
    return m_shown;
  }

  public void setShown( boolean shown )
  {
    if( shown != m_shown )
    {
      m_shown = shown;

      getView().refresh( this );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.template.IObsProviderListener#obsProviderChanged()
   */
  public void obsProviderChanged()
  {
    if( m_observation != null )
      m_observation.removeListener( this );

    m_observation = m_obsProvider.getObservation();

    if( m_observation != null )
      m_observation.addListener( this );

    observationChanged( m_observation );
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationListener#observationChanged(org.kalypso.ogc.sensor.IObservation)
   */
  public void observationChanged( final IObservation obs )
  {
    m_view.refresh( this );
  }

  /**
   * @see org.kalypso.ogc.sensor.template.IObsProvider#getObservation()
   */
  public IObservation getObservation()
  {
    return m_obsProvider.getObservation();
  }

  /**
   * @see org.kalypso.ogc.sensor.template.IObsProvider#getArguments()
   */
  public IRequest getArguments()
  {
    return m_obsProvider.getArguments();
  }

  public boolean isLoading()
  {
    return m_obsProvider.isLoading();
  }
}