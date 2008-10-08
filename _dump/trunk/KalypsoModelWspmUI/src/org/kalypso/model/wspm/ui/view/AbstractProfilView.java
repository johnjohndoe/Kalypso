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
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.chart.ui.IChartPart;
import org.kalypso.chart.ui.editor.mousehandler.AxisDragHandlerDelegate;
import org.kalypso.chart.ui.editor.mousehandler.PlotDragHandlerDelegate;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilListener;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.result.IStationResult;

import de.openali.odysseus.chart.framework.view.impl.ChartComposite;

/**
 * @author belger
 * @author kimwerner
 */

public abstract class AbstractProfilView implements IProfilListener, IProfilView, IChartPart

{

  protected final IProfil m_profile;

  private Control m_control;

  private final IStationResult[] m_results;

  public AbstractProfilView( final IProfil profile )
  {
    this( profile, null );
  }

  public AbstractProfilView( final IProfil profile, final IStationResult[] results )
  {
    m_profile = profile;
    m_results = results == null ? new IStationResult[0] : results;

    if( m_profile != null )
      m_profile.addProfilListener( this );

  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.IProfilView#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  public final Control createControl( final Composite parent, final int style )
  {
    m_control = doCreateControl( parent, null, style );
    return m_control;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.IProfilView#createControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  @Override
  public Control createControl( Composite parent, FormToolkit toolkit )
  {
    m_control = doCreateControl( parent, toolkit, parent.getStyle() );
    return m_control;
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.IProfilView#dispose()
   */
  public void dispose( )
  {
    if( m_profile != null )
      m_profile.removeProfilListener( this );
  }

  protected abstract Control doCreateControl( final Composite parent, FormToolkit toolkit, final int style );

  /**
   * @see org.kalypso.chart.ui.IChartPart#getChartComposite()
   */
  @Override
  public ChartComposite getChartComposite( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class< ? > clazz )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getAxisDragHandler()
   */
  public AxisDragHandlerDelegate getAxisDragHandler( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  public final Control getControl( )
  {
    return m_control;
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getPlotDragHandler()
   */
  public PlotDragHandlerDelegate getPlotDragHandler( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.IProfilView#getProfil()
   */
  public final IProfil getProfil( )
  {
    return m_profile;
  }

  public IStationResult[] getResults( )
  {
    return m_results;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilListener#onProblemMarkerChanged(org.kalypso.model.wspm.core.profil.IProfil)
   */
  public void onProblemMarkerChanged( final IProfil source )
  {
    // instances must overwrite this method
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilListener#onProfilChanged(org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint,
   *      org.kalypso.model.wspm.core.profil.IProfilChange[])
   */
  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
    // TODO Auto-generated method stub

  }
}