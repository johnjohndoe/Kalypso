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
package org.kalypso.ui.rrm.internal.timeseries.view.edit;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.jface.dialog.EnhancedTitleAreaDialog;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;

/**
 * @author Dirk Kuch
 */
public class EditTimeseriesDialog extends EnhancedTitleAreaDialog
{
  private final ITreeNodeModel m_model;

  private final ITimeseries m_timeseries;

  private static final String DIALOG_SCREEN_SIZE = "edit.time.series.dialog.screen.size"; //$NON-NLS-1$

  public EditTimeseriesDialog( final Shell shell, final ITreeNodeModel model, final ITimeseries timeseries )
  {
    super( shell );
    m_model = model;
    m_timeseries = timeseries;

    setShellStyle( SWT.CLOSE | SWT.MAX | SWT.TITLE | SWT.BORDER | SWT.APPLICATION_MODAL | SWT.RESIZE );
    setHelpAvailable( false );
  }

  @Override
  protected final Control createDialogArea( final Composite parent )
  {
    getShell().setText( "Edit Timeseries" );
// setDialogTitle();

    setMessage( "Edit Timeseries" );

    final FormToolkit toolkit = new FormToolkit( parent.getDisplay() );

    final Composite base = toolkit.createComposite( parent, SWT.NULL );
    base.setLayout( new GridLayout() );

    final Point screen = getScreenSize( DIALOG_SCREEN_SIZE );

    final GridData data = new GridData( GridData.FILL, GridData.FILL, true, true );
    data.widthHint = screen.x;
    data.heightHint = screen.y;
    base.setLayoutData( data );
//
    base.addControlListener( new ControlAdapter()
    {
      @Override
      public void controlResized( final ControlEvent e )
      {
        setScreenSize( DIALOG_SCREEN_SIZE, base.getSize() );
      }
    } );

    final EditTimeseriesChartComposite chart = new EditTimeseriesChartComposite( base );
    chart.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

//    m_chartPart = new ZmlDiagramChartPartComposite( this, getClass().getResource( "templates/diagram.kod" ) ); //$NON-NLS-1$

//
// /* first row */
// final SashForm form = new SashForm( base, SWT.HORIZONTAL );
// form.setLayout( new FillLayout() );
// form.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true, 2, 0 ) );
//
// final Composite leftPane = toolkit.createComposite( form );
// leftPane.setLayout( Layouts.createGridLayout() );
//
// final Composite middlePane = toolkit.createComposite( form );
// middlePane.setLayout( Layouts.createGridLayout() );
//
// final Composite rightPane = toolkit.createComposite( form );
// rightPane.setLayout( Layouts.createGridLayout() );
//
// final RepositoryLayoutPart repositoryLayoutPart = new RepositoryLayoutPart( leftPane, m_structure, m_page, m_context
// );
// repositoryLayoutPart.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
//
// m_chartLayoutPart = new ChartLayoutPart( middlePane, m_structure, m_context );
// m_chartLayoutPart.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
//
// m_tableLayoutPart = new TableLayoutPart( rightPane, m_structure, m_context );
// m_tableLayoutPart.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
//
// repositoryLayoutPart.addControlListener( new ControlAdapter()
// {
// @Override
// public void controlResized( final ControlEvent e )
// {
// setWeights( form.getWeights() );
// }
// } );
//
// m_tableLayoutPart.addControlListener( new ControlAdapter()
// {
// @Override
// public void controlResized( final ControlEvent e )
// {
// setWeights( form.getWeights() );
// }
// } );
//
// form.setWeights( getWeights() );
//
// /* second row */
// final InfoLayoutPart infoPart = new InfoLayoutPart( leftPane, m_structure, m_context );
// final GridData infoLayoutData = new GridData( GridData.FILL, GridData.FILL, true, false );
// infoLayoutData.heightHint = 100;
// infoPart.setLayoutData( infoLayoutData );
//
// final CalendarLayoutPart calendarLayoutPart = new CalendarLayoutPart( middlePane, m_structure, m_context );
// final GridData calendarLayoutData = new GridData( GridData.FILL, GridData.FILL, true, false );
// calendarLayoutData.heightHint = 100;
// calendarLayoutPart.setLayoutData( calendarLayoutData );
//
// final TableControlLayoutPart tableControlLayoutPart = new TableControlLayoutPart( rightPane, m_structure,
// m_tableLayoutPart, m_context );
// final GridData tableControlLayoutData = new GridData( GridData.FILL, GridData.FILL, true, false );
// tableControlLayoutData.heightHint = 100;
// tableControlLayoutPart.setLayoutData( tableControlLayoutData );
//
// repositoryLayoutPart.init();
// m_chartLayoutPart.init();
// m_tableLayoutPart.init();
// infoPart.init();
// calendarLayoutPart.init();
// tableControlLayoutPart.init();
//
// /* register event listeners */
// repositoryLayoutPart.addSelectionChangedListener( m_chartLayoutPart );
// repositoryLayoutPart.addSelectionChangedListener( m_tableLayoutPart );
//
// toolkit.adapt( form );
//
// registerObservationListener();
//
// setTableFilter();

    return super.createDialogArea( parent );
  }

}
