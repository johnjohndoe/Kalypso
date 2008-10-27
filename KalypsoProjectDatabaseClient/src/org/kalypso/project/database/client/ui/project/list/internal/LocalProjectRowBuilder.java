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
package org.kalypso.project.database.client.ui.project.list.internal;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.project.database.client.core.project.workspace.DeleteLocalProjectHandler;
import org.kalypso.project.database.client.ui.project.wizard.export.WizardProjectExport;

/**
 * @author Dirk Kuch
 */
public class LocalProjectRowBuilder extends AbstractProjectRowBuilder implements IProjectRowBuilder
{

  protected final IProject m_project;

  public LocalProjectRowBuilder( final IProject project )
  {
    m_project = project;
  }

  /**
   * @see org.kalypso.project.database.client.ui.project.internal.IProjectRowBuilder#render(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  @Override
  public void render( final Composite parent, final FormToolkit toolkit )
  {
    final Composite body = toolkit.createComposite( parent );
    body.setLayout( new GridLayout( 4, false ) );
    body.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final ImageHyperlink lnk = toolkit.createImageHyperlink( body, SWT.NONE );
    lnk.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    lnk.setImage( IMG_LOCAL_PROJECT );
    lnk.setToolTipText( String.format( "Öffne Projekt: %s", m_project.getName() ) );
    lnk.setText( m_project.getName() );

    /* export */
    getExportLink( m_project, body, toolkit );

    // spacer
    toolkit.createLabel( body, "    " ); //$NON-NLS-1$

    /* delete */
    getDeleteLink( m_project, body, toolkit );

  }

  protected static void getDeleteLink( final IProject project, final Composite body, final FormToolkit toolkit )
  {
    final ImageHyperlink lnkDelete = toolkit.createImageHyperlink( body, SWT.NONE );
    lnkDelete.setImage( IMG_DELETE_LOCAL );
    lnkDelete.setToolTipText( String.format( "Lösche Projekt: %s", project.getName() ) );

    lnkDelete.addHyperlinkListener( new HyperlinkAdapter()
    {
      /**
       * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
       */
      @Override
      public void linkActivated( final HyperlinkEvent e )
      {

        if( MessageDialog.openConfirm( lnkDelete.getShell(), "Lösche Projekt", String.format( "Projekt \"%s\" wirklich löschen?", project.getName() ) ) )
        {
          final DeleteLocalProjectHandler handler = new DeleteLocalProjectHandler( project );
          final IStatus status = ProgressUtilities.busyCursorWhile( handler );

          final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
          if( !shell.isDisposed() )
            ErrorDialog.openError( shell, "Löschen fehlgeschlagen", "Fehler beim Löschen des Projektes", status );
        }
      }
    } );

  }

  protected static void getExportLink( final IProject project, final Composite body, final FormToolkit toolkit )
  {
    final ImageHyperlink lnkExport = toolkit.createImageHyperlink( body, SWT.NONE );
    lnkExport.setImage( IMG_EXPORT_LOCAL );
    lnkExport.setToolTipText( String.format( "Exportiere Projekt: %s", project.getName() ) );

    lnkExport.addHyperlinkListener( new HyperlinkAdapter()
    {
      /**
       * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
       */
      @Override
      public void linkActivated( final HyperlinkEvent e )
      {
        final WizardProjectExport wizard = new WizardProjectExport( project );
        wizard.init( PlatformUI.getWorkbench(), new StructuredSelection( project ) );

        final WizardDialog dialog = new WizardDialog( null, wizard );
        dialog.open();
      }
    } );

  }
}
