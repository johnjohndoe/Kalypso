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
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.services.IEvaluationService;
import org.eclipse.ui.services.IServiceLocator;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.ui.internal.IWaterBodyStructure;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
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
    final StructuredViewer contentViewer = m_contentViewer.getWatersViewer();
    createSearchControls( toolkit, this, contentViewer ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
  }

  private Control createPdbView( final IServiceLocator serviceLocator, final FormToolkit toolkit, final Composite parent )
  {
    final Section section = toolkit.createSection( parent, Section.DESCRIPTION | ExpandableComposite.TITLE_BAR );
    section.setText( Messages.getString( "ConnectionViewer.0" ) ); //$NON-NLS-1$
    section.setDescription( Messages.getString( "ConnectionViewer.1" ) ); //$NON-NLS-1$
    section.setLayout( new FillLayout() );

    m_contentViewer = new ConnectionContentControl( serviceLocator, toolkit, section, m_connection );

    section.setClient( m_contentViewer );

    return section;
  }

  private Control createSearchControls( final FormToolkit toolkit, final Composite parent, final StructuredViewer viewer )
  {
    final Section section = toolkit.createSection( parent, ExpandableComposite.TITLE_BAR | Section.DESCRIPTION | ExpandableComposite.TWISTIE | ExpandableComposite.EXPANDED );
    section.setText( Messages.getString( "ConnectionViewer.2" ) ); //$NON-NLS-1$
    section.setDescription( Messages.getString( "ConnectionViewer.3" ) ); //$NON-NLS-1$
    section.setLayout( new FillLayout() );

    final ContentSearchViewer searchPanel = new ContentSearchViewer( toolkit, section, viewer, this );
    toolkit.adapt( searchPanel );
    section.setClient( searchPanel );

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
    final TreeViewer watersViewer = m_contentViewer.getWatersViewer();
//    final TreeViewer statesViewer = m_contentViewer.getStatesViewer();

    // FIXME: combined viewer?
    site.setSelectionProvider( watersViewer );

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

    final Menu watersMenu = menuManager.createContextMenu( watersViewer.getControl() );
//    final Menu statesMenu = menuManager.createContextMenu( statesViewer.getControl() );

    site.registerContextMenu( site.getId() + ":waters", menuManager, watersViewer ); //$NON-NLS-1$
    //site.registerContextMenu( site.getId() + ":states", menuManager, statesViewer ); //$NON-NLS-N$

    watersViewer.getControl().setMenu( watersMenu );
//    statesViewer.getControl().setMenu( statesMenu );

    final IEvaluationService service = (IEvaluationService)site.getService( IEvaluationService.class );
    service.requestEvaluation( IEvaluationService.PROP_NOTIFYING /* ISources.ACTIVE_CURRENT_SELECTION_NAME */);

    watersViewer.getControl().addDisposeListener( new DisposeListener()
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

  @Override
  public IStructuredSelection getSelection( )
  {
    return m_contentViewer.getSelection();
  }
}