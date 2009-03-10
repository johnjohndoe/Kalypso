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

import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.chart.ui.IChartPart;
import org.kalypso.chart.ui.editor.commandhandler.ChartHandlerUtilities;
import org.kalypso.chart.ui.editor.mousehandler.AxisDragHandlerDelegate;
import org.kalypso.chart.ui.editor.mousehandler.PlotDragHandlerDelegate;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilListener;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.view.AbstractProfilViewPart;

import de.openali.odysseus.chart.framework.util.ChartUtilities;
import de.openali.odysseus.chart.framework.view.impl.ChartComposite;

/**
 * @author kimwerner
 */
public class ChartView extends AbstractProfilViewPart implements IChartPart, IProfilListener
{

  public static final String ID = "org.kalypso.model.wspm.ui.view.chart.ChartView"; //$NON-NLS-1$

  private ProfilChartView m_chart = null;

  private FormToolkit m_toolkit = null;

  private Form m_form = null;
  
  

  /**
   * @see com.bce.profil.eclipse.view.AbstractProfilViewPart2#createContent(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContent( final Composite parent )
  {
    if( m_toolkit == null )
    {
      m_toolkit = new FormToolkit( parent.getDisplay() );
    }
    if( m_form == null )
    {
      m_form = m_toolkit.createForm( parent );
      m_form.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
      final GridLayout gridLayout = new GridLayout();
      gridLayout.marginWidth = 0;
      gridLayout.marginHeight = 0;
      m_form.setLayout( gridLayout );
      m_form.getBody().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
      m_form.getBody().setLayout( new GridLayout() );
      m_toolkit.decorateFormHeading( m_form );
    }
    if( m_chart == null )
    {
      m_chart = new ProfilChartView();
      m_chart.createControl( m_form.getBody() );
    }
    if( getProfil() == null )
    {
      m_form.setMessage( org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.view.chart.ChartView.0"), IMessageProvider.INFORMATION ); //$NON-NLS-1$
      m_chart.setProfil( null );
    }
    else
    {
      m_form.setMessage( null );
      m_chart.setProfil( getProfil() );
      ChartUtilities.maximize( m_chart.getChart().getChartModel() );
      m_form.getBody().layout();
    }
    m_form.getBody().setFocus();
    return getChartComposite();

  }

  /**
   * @see org.kalypso.model.wspm.ui.view.AbstractProfilViewPart#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_form != null && !m_form.isDisposed() )
    {
      m_form.dispose();
      m_form = null;
    }
    if( m_toolkit != null )
    {
      m_toolkit.dispose();
      m_toolkit = null;
    }

    if( m_chart != null )
    {
      ChartHandlerUtilities.updateElements( m_chart );
      m_chart.dispose();
      m_chart = null;
    }

    super.dispose();
  }

  /**
   * @see com.bce.eind.core.profil.IProfilListener#onProfilChanged(com.bce.eind.core.profil.changes.ProfilChangeHint,
   *      com.bce.eind.core.profil.IProfilChange[])
   */
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    if( hint.isPointPropertiesChanged() )
      m_chart.updateLayer();
  }

  public ChartComposite getChart( )
  {
    return m_chart.getChartComposite();
  }

 

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilListener#onProblemMarkerChanged(org.kalypso.model.wspm.core.profil.IProfil)
   */
  public void onProblemMarkerChanged( final IProfil source )
  {
    //
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#getAdapter(java.lang.Class)
   */
  @SuppressWarnings("unchecked")//$NON-NLS-1$
  @Override
  public Object getAdapter( final Class adapter )
  {

    if( IChartPart.class.equals( adapter ) )
    {
      return m_chart;
    }

    return super.getAdapter( adapter );
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getAxisDragHandler()
   */
  public AxisDragHandlerDelegate getAxisDragHandler( )
  {
    if( m_chart == null )
      return null;
    return m_chart.getAxisDragHandler();
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getChartComposite()
   */
  public ChartComposite getChartComposite( )
  {
    if( m_chart == null )
      return null;
    return m_chart.getChartComposite();
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getPlotDragHandler()
   */
  public PlotDragHandlerDelegate getPlotDragHandler( )
  {
    if( m_chart == null )
      return null;
    return m_chart.getPlotDragHandler();
  }

}