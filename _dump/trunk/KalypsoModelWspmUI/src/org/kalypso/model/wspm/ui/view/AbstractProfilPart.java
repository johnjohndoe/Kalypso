/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.commands.operations.IOperationHistory;
import org.eclipse.core.runtime.PlatformObject;
import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.PlatformUI;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.model.wspm.ui.profil.operation.ProfilUndoContext;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartViewProvider;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartViewProviderListener;
import org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;
import org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum;
import org.kalypso.model.wspm.ui.view.chart.color.DefaultProfilColorRegistryFactory;
import org.kalypso.model.wspm.ui.view.legend.ChartLegend;

/**
 * The common code for showing a {@link IProfile} as a chart.
 * 
 * @author Gernot Belger
 */
public class AbstractProfilPart extends PlatformObject implements IProfilChartViewProvider
{
  private final List<IProfilChartViewProviderListener> m_listener = new ArrayList<IProfilChartViewProviderListener>();

  private final ProfilViewData m_viewdata = new ProfilViewData();

  protected final Runnable m_updateControlRunnable = new Runnable()
  {
    public void run( )
    {
      updateControl();
    }
  };

  protected final Runnable m_repaintRunnable = new Runnable()
  {
    public void run( )
    {
      if( m_chartview != null )
        m_chartview.getChart().redraw();
    }
  };

  private Composite m_control;

  private ColorRegistry m_profilColorRegistry;

  protected ProfilChartView m_chartview;

  private IProfil m_profile;

  private IProfilLayerProvider m_layerProvider;

  public Control createPartControl( final Composite parent )
  {
    m_control = new Composite( parent, SWT.NONE );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.marginWidth = 0;
    gridLayout.marginHeight = 0;
    m_control.setLayout( gridLayout );
    m_profilColorRegistry = DefaultProfilColorRegistryFactory.createColorRegistry( parent.getDisplay() );
//TODO: KIM auskommentieren überprüfen
    //updateControl();

    return m_control;
  }

  public synchronized void dispose( )
  {
    if( m_chartview != null )
    {
      m_chartview.dispose();
      m_chartview = null;
    }

    m_viewdata.dispose();

    m_profilColorRegistry = null;

    //TODO: KIM undo implementieren
    
    if( m_profile != null )
    {
      // die undo queue für dieses profil löschen
      final IOperationHistory operationHistory = getUndoHistory();
      operationHistory.dispose( getUndoContext(), true, true, true );
    }
    m_profile = null;
  }

  public void updateControl( )
  {
//TODO:KIM viewdata verhalten überprüfen
    if( m_chartview != null )
    {
      m_chartview.saveState( m_viewdata.getChartMemento() );
  m_chartview.dispose();
      m_chartview = null;
    }
  //TODO:KIM viewdata verhalten überprüfen
    if( m_control == null || m_control.isDisposed() )
      return;

    final Control[] children = m_control.getChildren();
    for( final Control c : children )
     c.dispose();

    if( m_profile == null )
    {
      final Label label = new Label( m_control, SWT.CENTER );
      label.setText( Messages.AbstractProfilPart_0 );
      final GridData gridData = new GridData();
      gridData.grabExcessHorizontalSpace = true;
      gridData.horizontalAlignment = SWT.FILL;
      gridData.horizontalIndent = 10;
      gridData.grabExcessVerticalSpace = true;
      gridData.verticalAlignment = SWT.CENTER;
      gridData.verticalIndent = 10;

      label.setLayoutData( gridData );
    }
    else
    {
      // final ArrayList<String> kommentare = (ArrayList<String>) m_pem.getProfil().getProperty(
      // PROFIL_PROPERTY.KOMMENTAR );

      // setContentDescription( (kommentare == null) ? "" : kommentare.toString() );
      m_chartview = new ProfilChartView( m_profile,  m_profilColorRegistry );
      m_chartview.setLayerProvider( m_layerProvider );
      m_chartview.createControl( m_control, SWT.BORDER );
      m_chartview.restoreState( m_viewdata.getChartMemento() );

      // final Control chartControl = m_chartview.getControl();
      // chartControl.setMenu( m_menuManager.createContextMenu( chartControl ) );
    }

    m_control.layout();

    fireOnProfilChartViewChanged();
  }

  
  //TODO: Kim undoContext nur für IProfile ??
  public ProfilUndoContext getUndoContext( )
  {
    return m_profile == null ? null : new ProfilUndoContext( m_profile );
  }

  public IOperationHistory getUndoHistory( )
  {
    return PlatformUI.getWorkbench().getOperationSupport().getOperationHistory();
  }
  //TODO: Kim undoContext nur für IProfile ??
  public void setFocus( )
  {
    if( m_control != null )
      m_control.setFocus();
  }

  public synchronized void setProfil( final IProfil profile )
  {
    // die undo queue für dieses profil löschen
    if( m_profile != null )
    {
      final IOperationHistory operationHistory = getUndoHistory();
      operationHistory.dispose( getUndoContext(), true, true, true );
    }

    m_profile = profile;

    if( m_control != null && !m_control.isDisposed() )
    {
      // BUGFIX: use Async exec, because we got a deadlock here
      m_control.getDisplay().asyncExec( m_updateControlRunnable );
    }
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#getAdapter(java.lang.Class)
   */
  @SuppressWarnings("unchecked")//$NON-NLS-1$
  @Override
  public Object getAdapter( final Class adapter )
  {
    if( adapter == ProfilChartView.class )
      return m_chartview;

    if( adapter == IProfilChartViewProvider.class )
      return this;

    return super.getAdapter( adapter );
  }

  public ProfilViewData getViewData( )
  {
    //TODO: KIM überprüfen 
    return m_viewdata;
  }

  public IProfil getProfil( )
  {
    return m_profile;
  }

  
  //TODO: kim remove this, only used in ChartEditor
  public ChartLegend createChartLegend( final Composite control, final int style )
  {
    if( m_chartview == null || m_chartview.getChart() == null )
      return null;

    final ChartLegend chartLegend = new ChartLegend( control, style, m_chartview.getChart(), false );
    chartLegend.restoreState( m_viewdata.getLegendMemento() );
    return chartLegend;
  }

  /**
   * @param chartlegend
   */
//TODO: kim remove this, only used in ChartEditor
  public void saveLegend( final ChartLegend chartlegend )
  {
    if( chartlegend != null )
      chartlegend.saveState( m_viewdata.getLegendMemento() );
  }
//TODO: kim remove this, only used in ChartEditor
//  public void runChartAction( final ProfilChartActionsEnum chartAction )
//  {
//    if( m_chartview != null )
//      m_chartview.runChartAction( chartAction );
//  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.IProfilChartViewProvider#getProfilChartView()
   */
  public ProfilChartView getProfilChartView( )
  {
    return m_chartview;
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.IProfilChartViewProvider#addProfilChartViewProviderListener(org.kalypso.model.wspm.ui.profil.view.chart.IProfilChartViewProviderListener)
   */
  public void addProfilChartViewProviderListener( final IProfilChartViewProviderListener l )
  {
    m_listener.add( l );
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.IProfilChartViewProvider#removeProfilChartViewProviderListener(org.kalypso.model.wspm.ui.profil.view.chart.IProfilChartViewProviderListener)
   */
  public void removeProfilChartViewProviderListener( final IProfilChartViewProviderListener l )
  {
    m_listener.remove( l );
  }

  private void fireOnProfilChartViewChanged( )
  {
    for( final IProfilChartViewProviderListener l : m_listener )
      l.onProfilChartViewChanged( getProfilChartView() );
  }

}