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
package org.kalypso.model.wspm.pdb.ui.internal.content;

import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.services.IEvaluationService;
import org.eclipse.ui.services.IServiceLocator;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.ui.internal.IWaterBodyStructure;
import org.kalypso.model.wspm.pdb.ui.internal.content.filter.StateFilterControl;
import org.kalypso.model.wspm.pdb.ui.internal.content.filter.WaterBodyFilterControl;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.PdbWspmProject;

/**
 * @author Gernot Belger
 */
public class ConnectionViewer extends Composite implements IConnectionViewer
{
  private final IPdbConnection m_connection;

  private ConnectionContentControl m_contentViewer;

  private final PdbWspmProject m_project;

  public ConnectionViewer( final IServiceLocator serviceLocator, final FormToolkit toolkit, final Composite parent, final IPdbConnection connection, final PdbWspmProject project )
  {
    super( parent, SWT.NONE );

    m_connection = connection;
    m_project = project;

    toolkit.adapt( this );
    GridLayoutFactory.fillDefaults().applyTo( this );

    createPdbView( serviceLocator, toolkit, this ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    final StructuredViewer contentViewer = m_contentViewer.getTreeViewer();
    createSearchControls( toolkit, this, contentViewer ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
  }

  private Control createPdbView( final IServiceLocator serviceLocator, final FormToolkit toolkit, final Composite parent )
  {
    final Section section = toolkit.createSection( parent, Section.DESCRIPTION | ExpandableComposite.TITLE_BAR );
    section.setText( "Contents" );
    section.setDescription( "Contents of the cross section database." );
    section.setLayout( new FillLayout() );

    m_contentViewer = new ConnectionContentControl( serviceLocator, toolkit, section, m_connection, m_project );

    section.setClient( m_contentViewer );

    return section;
  }

  private Control createSearchControls( final FormToolkit toolkit, final Composite parent, final StructuredViewer viewer )
  {
    final Section section = toolkit.createSection( parent, ExpandableComposite.TITLE_BAR | Section.DESCRIPTION | ExpandableComposite.TWISTIE | ExpandableComposite.EXPANDED );
    section.setText( "Search" );
    section.setDescription( "Edit search fields to filter visible items." );
    section.setLayout( new FillLayout() );

    final Composite panel = toolkit.createComposite( section );
    GridLayoutFactory.fillDefaults().extendedMargins( 0, 0, 0, 5 ).applyTo( panel );

    section.setClient( panel );

    final Group waterGroup = new Group( panel, SWT.NONE );
    toolkit.adapt( waterGroup );
    waterGroup.setLayout( new FillLayout() );
    waterGroup.setText( "Water Bodies" );
    waterGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    final WaterBodyFilterControl waterFilterControl = new WaterBodyFilterControl( toolkit, waterGroup, this );
    waterFilterControl.setViewer( viewer );

    final Group stateGroup = new Group( panel, SWT.NONE );
    toolkit.adapt( stateGroup );
    stateGroup.setLayout( new FillLayout() );
    stateGroup.setText( "States" );
    stateGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    final StateFilterControl stateFilterControl = new StateFilterControl( toolkit, stateGroup );
    stateFilterControl.setViewer( viewer );

    return section;
  }

  @Override
  public void reload( final ElementSelector elementToSelect )
  {
    if( m_contentViewer != null )
      m_contentViewer.refresh( elementToSelect );
  }

  public void createContextMenu( final IWorkbenchPartSite site )
  {
    final TreeViewer viewer = m_contentViewer.getTreeViewer();

    site.setSelectionProvider( viewer );

    // create context menu for editor
    final MenuManager menuManager = new MenuManager();
    // add additions separator: if not, eclipse whines
    menuManager.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );

    menuManager.addMenuListener( new IMenuListener()
    {
      @Override
      public void menuAboutToShow( final IMenuManager manager )
      {
        // Strange, without this listener, the menu does not show up the first time
      }
    } );

    final Menu menu = menuManager.createContextMenu( viewer.getControl() );

    site.registerContextMenu( "pdbContentTreePopupMenu", menuManager, viewer ); //$NON-NLS-N$

    viewer.getControl().setMenu( menu );

    final IEvaluationService service = (IEvaluationService) site.getService( IEvaluationService.class );
    service.requestEvaluation( ISources.ACTIVE_CURRENT_SELECTION_NAME );

    viewer.getControl().addDisposeListener( new DisposeListener()
    {
      @Override
      public void widgetDisposed( final DisposeEvent e )
      {
        menuManager.dispose();
        site.setSelectionProvider( null );
      }
    } );
  }

  @Override
  public IWaterBodyStructure getStructure( )
  {
    if( m_contentViewer == null )
      return null;

    return m_contentViewer.getStructure();
  }

  @Override
  public IPdbConnection getConnection( )
  {
    return m_connection;
  }

  @Override
  public String getUsername( )
  {
    return m_connection.getSettings().getUsername();
  }

  @Override
  public PdbWspmProject getProject( )
  {
    return m_project;
  }
}