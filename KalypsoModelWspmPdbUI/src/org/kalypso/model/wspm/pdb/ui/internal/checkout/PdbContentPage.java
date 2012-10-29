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
package org.kalypso.model.wspm.pdb.ui.internal.checkout;

import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.ui.internal.IWaterBodyStructure;
import org.kalypso.model.wspm.pdb.ui.internal.content.ConnectionContentControl;
import org.kalypso.model.wspm.pdb.ui.internal.content.ContentSearchViewer;
import org.kalypso.model.wspm.pdb.ui.internal.content.ElementSelector;
import org.kalypso.model.wspm.pdb.ui.internal.content.IConnectionViewer;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.PdbWspmProject;

/**
 * @author Gernot Belger
 */
public class PdbContentPage extends WizardPage implements IConnectionViewer
{
  private IPdbConnection m_connection;

  private ConnectionContentControl m_contentControl;

  public PdbContentPage( final String pageName )
  {
    super( pageName );

    setTitle( Messages.getString( "PdbContentPage.0" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "PdbContentPage.1" ) ); //$NON-NLS-1$
  }

  @Override
  public IWizardPage getPreviousPage( )
  {
    return null;
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );
    GridLayoutFactory.swtDefaults().applyTo( panel );

    createContentControl( panel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    createSearchControl( panel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    setPageComplete( false );
  }

  private Control createContentControl( final Composite parent )
  {
    m_contentControl = new ConnectionContentControl( null, null, parent, m_connection );

    final TreeViewer treeViewer = m_contentControl.getWatersViewer();
    treeViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleSelectionChanged( (IStructuredSelection)event.getSelection() );
      }
    } );

    return m_contentControl;
  }

  private Control createSearchControl( final Composite panel )
  {
    final TreeViewer treeViewer = m_contentControl.getWatersViewer();
    return new ContentSearchViewer( null, panel, treeViewer, this );
  }

  protected void handleSelectionChanged( final IStructuredSelection selection )
  {
    if( selection.isEmpty() )
    {
      setMessage( Messages.getString( "PdbContentPage.2" ), WARNING ); //$NON-NLS-1$
      setPageComplete( false );
    }
    else
    {
      setMessage( null );
      setPageComplete( true );
    }
  }

  public void setConnection( final IPdbConnection connection )
  {
    m_connection = connection;
  }

  @Override
  public IPdbConnection getConnection( )
  {
    return m_connection;
  }

  @Override
  public void reload( final ElementSelector elementToSelect )
  {
  }

  @Override
  public String getUsername( )
  {
    return m_connection.getSettings().getUsername();
  }

  @Override
  public IWaterBodyStructure getStructure( )
  {
    return m_contentControl.getStructure();
  }

  @Override
  public PdbWspmProject getProject( )
  {
    return null;
  }

  @Override
  public IStructuredSelection getSelection( )
  {
    return m_contentControl.getSelection();
  }
}