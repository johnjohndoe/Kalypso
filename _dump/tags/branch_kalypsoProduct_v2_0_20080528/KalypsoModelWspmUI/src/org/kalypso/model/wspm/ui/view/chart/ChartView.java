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
package org.kalypso.model.wspm.ui.view.chart;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.view.AbstractProfilPart;
import org.kalypso.model.wspm.ui.view.AbstractProfilViewPart2;
import org.kalypso.model.wspm.ui.view.chart.action.ProfilChartViewActionBarContributor;
import org.kalypso.observation.result.IComponent;

/**
 * @author Gernot Belger
 */
public class ChartView extends AbstractProfilViewPart2
{
  public static final String ID = "org.kalypso.model.wspm.ui.view.chart.ChartView"; //$NON-NLS-1$

  private final AbstractProfilPart m_profilPart = new AbstractProfilPart();

  public ChartView( )
  {
    super();
  }

  public ChartView( final ProfilChartViewActionBarContributor actionContributor )
  {
    super( actionContributor );
  }

  /**
   * @see com.bce.profil.eclipse.view.AbstractProfilViewPart2#createContent(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContent( final Composite parent )
  {
    final Control control = m_profilPart.createPartControl( parent );

    m_profilPart.setProfil( getProfil() );

    return control;
  }

  /**
   * @see com.bce.profil.eclipse.view.AbstractProfilViewPart2#saveState()
   */
  @Override
  protected void saveState( )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @see com.bce.eind.core.profil.IProfilListener#onProfilChanged(com.bce.eind.core.profil.changes.ProfilChangeHint,
   *      com.bce.eind.core.profil.IProfilChange[])
   */
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    // do nothing the ProfilChartView is itself a listener on the profile
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilListener#onProblemMarkerChanged(org.kalypso.model.wspm.core.profil.IProfil)
   */
  public void onProblemMarkerChanged( final IProfil source )
  {
    // do nothing the ProfilChartView is itself a listener on the profile
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#getAdapter(java.lang.Class)
   */
  @SuppressWarnings("unchecked")//$NON-NLS-1$
  @Override
  public Object getAdapter( final Class adapter )
  {
    final Object adapted = m_profilPart.getAdapter( adapter );
    if( adapted != null )
      return adapted;

    return super.getAdapter( adapter );
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.IProfilViewDataListener#onProfilViewDataChanged()
   */
  public void onProfilViewDataChanged( )
  {
    // probably nothing to do
    if( (m_profilPart.getViewData() != null) && (m_profilPart.getProfil() != null) )
    {
      final IComponent[] markerTypes = m_profilPart.getProfil().getPointMarkerTypes();
      for( final IComponent markerTyp : markerTypes )
      {
        m_profilPart.getViewData().setMarkerVisibility( markerTyp.getId(), true );
      }
    }
  }
}
