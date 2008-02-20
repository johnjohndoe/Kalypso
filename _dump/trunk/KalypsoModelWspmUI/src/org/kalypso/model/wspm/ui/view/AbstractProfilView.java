/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.ui.view;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilListener;
import org.kalypso.model.wspm.core.result.IStationResult;

/**
 * @author belger
 */
public abstract class AbstractProfilView implements IProfilListener, IProfilView, IProfilViewDataListener
{
  private final ProfilViewData m_viewdata;

  protected final IProfil m_profile;

  private Control m_control;

  private final IStationResult[] m_results;

  public AbstractProfilView( final IProfil profile, final ProfilViewData viewdata, final IStationResult[] results )
  {
    m_profile = profile;
    m_viewdata = viewdata;
    m_results = results == null ? new IStationResult[0] : results;

    if( m_profile != null )
      m_profile.addProfilListener( this );

    if( m_viewdata != null )
      m_viewdata.addProfilViewDataListener( this );

  }

  public AbstractProfilView( final IProfil profile, final ProfilViewData viewdata )
  {
    this( profile, viewdata, new IStationResult[0] );
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.IProfilView#dispose()
   */
  public void dispose( )
  {
    if( m_profile != null )
      m_profile.removeProfilListener( this );

    if( m_viewdata != null )
      m_viewdata.removeProfilViewDataListener( this );
  }

  protected abstract Control doCreateControl( final Composite parent, final int style );

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.IProfilView#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  public final Control createControl( final Composite parent, final int style )
  {
    m_control = doCreateControl( parent, style );
    return m_control;
  }

  public final Control getControl( )
  {
    return m_control;
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.IProfilView#getProfil()
   */
  public final IProfil getProfil( )
  {
    return m_profile;
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.IProfilView#getViewData()
   */
  public final ProfilViewData getViewData( )
  {
    return m_viewdata;
  }

  public IStationResult[] getResults( )
  {
    return m_results;
  }

  public void onProfilViewDataChanged( )
  {
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilListener#onProblemMarkerChanged(org.kalypso.model.wspm.core.profil.IProfil)
   */
  public void onProblemMarkerChanged( final IProfil source )
  {
  }
}
