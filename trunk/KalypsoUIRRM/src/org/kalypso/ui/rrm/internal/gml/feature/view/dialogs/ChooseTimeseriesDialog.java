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
package org.kalypso.ui.rrm.internal.gml.feature.view.dialogs;

import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.contribs.eclipse.jface.dialog.EnhancedTrayDialog;
import org.kalypso.contribs.eclipse.ui.forms.ToolkitUtils;
import org.kalypso.model.hydrology.timeseries.binding.IStationCollection;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.timeseries.view.StationsByStationsStrategy;
import org.kalypso.ui.rrm.internal.timeseries.view.filter.TimeseriesBrowserSearchViewer;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeContentProvider;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeLabelProvider;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeModel;

/**
 * @author Dirk Kuch
 */
public class ChooseTimeseriesDialog extends EnhancedTrayDialog
{
  private static final String DIALOG_SCREEN_SIZE = "choose.time.series.dialog.screen.size"; //$NON-NLS-1$

  private final IStationCollection m_collection;

  private final String m_parameterType;

  private TreeViewer m_treeViewer;

  private final CommandableWorkspace m_workspace;

  public ChooseTimeseriesDialog( final Shell shell, final CommandableWorkspace workspace, final IStationCollection collection, final String parameterType )
  {
    super( shell );
    m_workspace = workspace;
    m_collection = collection;
    m_parameterType = parameterType;

    setShellStyle( SWT.CLOSE | SWT.MAX | SWT.TITLE | SWT.BORDER | SWT.APPLICATION_MODAL | SWT.RESIZE );
    setHelpAvailable( false );
    setDialogHelpAvailable( false );

  }

  @Override
  protected final Control createDialogArea( final Composite parent )
  {
    final FormToolkit toolkit = ToolkitUtils.createToolkit( parent );
    getShell().setText( "Choose Timeseries" );

    final Composite base = toolkit.createComposite( parent, SWT.NULL );
    base.setLayout( new GridLayout() );

    final Point screen = getScreenSize( DIALOG_SCREEN_SIZE );

    final GridData data = new GridData( GridData.FILL, GridData.FILL, true, true );
    data.widthHint = screen.x;
    data.heightHint = screen.y;
    base.setLayoutData( data );

    base.addControlListener( new ControlAdapter()
    {
      @Override
      public void controlResized( final ControlEvent e )
      {
        setScreenSize( DIALOG_SCREEN_SIZE, base.getSize() );
      }
    } );

    createTreeViewer( base ).setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    createSearchPanel( base, toolkit ).setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    return base;
  }

  private Composite createTreeViewer( final Composite base )
  {
    m_treeViewer = new TreeViewer( base, SWT.FLAT | SWT.SINGLE );
    m_treeViewer.setContentProvider( new TreeNodeContentProvider() );
    m_treeViewer.setLabelProvider( new TreeNodeLabelProvider() );

    final StationsByStationsStrategy strategy = new StationsByStationsStrategy( m_collection );

    final TreeNodeModel input = new TreeNodeModel( strategy, m_workspace, m_treeViewer );
    m_treeViewer.setInput( input );

    return m_treeViewer.getTree();
  }

  private Control createSearchPanel( final Composite parent, final FormToolkit toolkit )
  {
    final Section section = toolkit.createSection( parent, ExpandableComposite.TITLE_BAR | ExpandableComposite.TWISTIE | ExpandableComposite.EXPANDED );
    section.setText( "Suche" );
    section.setLayout( new FillLayout() );

    final TimeseriesBrowserSearchViewer searchPanel = new TimeseriesBrowserSearchViewer( section, toolkit, m_treeViewer );
    searchPanel.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    searchPanel.setParameterType( m_parameterType );

    section.setClient( searchPanel );
    toolkit.adapt( searchPanel );

    return section;
  }
}
