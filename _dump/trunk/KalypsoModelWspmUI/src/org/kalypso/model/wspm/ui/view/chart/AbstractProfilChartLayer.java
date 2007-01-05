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
package org.kalypso.model.wspm.ui.view.chart;

import org.eclipse.swt.graphics.Point;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;

import de.belger.swtchart.EditInfo;
import de.belger.swtchart.axis.AxisRange;
import de.belger.swtchart.layer.AbstractChartLayer;

public abstract class AbstractProfilChartLayer extends AbstractChartLayer implements IProfilChartLayer
{
  private final ProfilChartView m_chartView;
  private boolean m_initialVisibility = true;

  public AbstractProfilChartLayer( final ProfilChartView chartView, final AxisRange domainRange, final AxisRange valueRange )
  {
    super( domainRange, valueRange );

    m_chartView = chartView;
  }

  public AbstractProfilChartLayer( final ProfilChartView chartView, final AxisRange domainRange, final AxisRange valueRange, final boolean initalVisibility )
  {
    super( domainRange, valueRange );
    
    m_chartView = chartView;
    m_initialVisibility = initalVisibility;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getInitialVisibility()
   */
  public boolean getInitialVisibility( )
  {
    return m_initialVisibility;
  }
  
  public final void edit( final Point point, final Object data )
  {
    editProfil( point, data );

    getViewData().setActiveLayer( this );
  }

  /**
   * @return Returns the profil.
   */
  protected final IProfil getProfil( )
  {
    return getProfilEventManager().getProfil();
  }

  public IProfilEventManager getProfilEventManager( )
  {
    return m_chartView.getProfilEventManager();
  }

  /**
   * @return Returns the viewData.
   */
  protected final ProfilViewData getViewData( )
  {
    return m_chartView.getViewData();
  }

  protected abstract void editProfil( Point point, Object data );

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.layer.IProfilChartLayer#createLayerPanel(org.kalypso.model.wspm.core.profil.IProfilEventManager,
   *      org.kalypso.model.wspm.ui.profil.view.ProfilViewData)
   */
  public IProfilView createLayerPanel( IProfilEventManager pem, ProfilViewData viewData )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getHoverInfo(org.eclipse.swt.graphics.Point)
   */
  public EditInfo getHoverInfo( Point point )
  {
    return null;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#onProfilChanged(org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint,
   *      org.kalypso.model.wspm.core.profil.IProfilChange[])
   */
  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintDrag(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper,
   *      org.eclipse.swt.graphics.Point, java.lang.Object)
   */
  public void paintDrag( GCWrapper gc, Point editing, Object hoverData )
  {
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintLegend(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  public void paintLegend( GCWrapper gc )
  {
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#setActivePoint(java.lang.Object)
   */
  public void setActivePoint( Object data )
  {
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.layer.IProfilChartLayer#getProfilChartView()
   */
  public ProfilChartView getProfilChartView( )
  {
    return m_chartView;
  }
}
