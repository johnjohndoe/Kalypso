/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
import org.eclipse.ui.contexts.IContextActivation;
import org.eclipse.ui.contexts.IContextService;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilListener;
import org.kalypso.model.wspm.core.result.IStationResult;

/**
 * @author belger
 */
public abstract class AbstractProfilView implements IProfilListener, IProfilView, IProfilViewDataListener
{
  private final ProfilViewData m_viewdata;

  protected final IProfilEventManager m_pem;

  private Control m_control;

  private final IStationResult[] m_results;

  public AbstractProfilView( final IProfilEventManager pem, final ProfilViewData viewdata, final IStationResult[] results )
  {
    m_pem = pem;
    m_viewdata = viewdata;
    m_results = results == null ? new IStationResult[0] : results;

    if( m_pem != null )
      m_pem.addProfilListener( this );

    if( m_viewdata != null )
      m_viewdata.addProfilViewDataListener( this );

  }

  public AbstractProfilView( final IProfilEventManager pem, final ProfilViewData viewdata )
  {
    this( pem, viewdata, new IStationResult[0] );
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.IProfilView#dispose()
   */
  public void dispose( )
  {
    if( m_pem != null )
      m_pem.removeProfilListener( this );

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
//  KIM 
//    IContextService contextService = (IContextService) parent.getSite()
//    .getService(IContextService.class);
//  IContextActivation contextActivation = contextService.activateContext("org.kalypso.model.wspm.ui.view.table.swt.context");
   //KIM 
    
  }

  public final Control getControl( )
  {
    if( m_control == null )
      throw new IllegalStateException( "createControl not yet called" );

    return m_control;
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.IProfilView#getProfil()
   */
  public final IProfil getProfil( )
  {
    return m_pem == null ? null : m_pem.getProfil();
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

  public IProfilEventManager getProfilEventManager( )
  {
    return m_pem;
  }
}
