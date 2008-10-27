/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.jface.dialogs.ErrorDialog;
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
import org.kalypso.project.database.client.core.project.commit.CommitProjectWorker;
import org.kalypso.project.database.client.core.project.lock.acquire.AcquireProjectLockWorker;
import org.kalypso.project.database.client.core.project.lock.release.ReleaseProjectLockWorker;
import org.kalypso.project.database.common.nature.IRemoteProjectPreferences;
import org.kalypso.project.database.common.nature.RemoteProjectNature;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * Row builder for projects, which existing locally and remote
 * 
 * @author Dirk Kuch
 */
public class LoReProjectRowBuilder extends AbstractProjectRowBuilder implements IProjectRowBuilder
{
  protected final IProject m_project;

  protected final KalypsoProjectBean m_bean;

  private final IPreferenceChangeListener m_listener;

  public LoReProjectRowBuilder( final IProject project, final KalypsoProjectBean bean, final IPreferenceChangeListener listener )
  {
    m_project = project;
    m_bean = bean;
    m_listener = listener;
  }

  /**
   * @see org.kalypso.project.database.client.ui.project.internal.IProjectRowBuilder#render(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  @Override
  public void render( final Composite parent, final FormToolkit toolkit )
  {
    try
    {
      /* settings */
      final IProjectNature nature = m_project.getNature( RemoteProjectNature.NATURE_ID );
      final RemoteProjectNature myNature = (RemoteProjectNature) nature;
      final IRemoteProjectPreferences preferences = myNature.getRemotePreferences( m_project, m_listener );

      final Composite body = toolkit.createComposite( parent );
      body.setLayout( new GridLayout( 6, false ) );
      body.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

      final ImageHyperlink lnk = toolkit.createImageHyperlink( body, SWT.NONE );
      lnk.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

      if( preferences.isLocked() )
        lnk.setImage( IMG_LORE_LOCKED );
      else
        lnk.setImage( IMG_LORE_PROJECT );

      lnk.setToolTipText( String.format( "�ffne Projekt: %s", m_project.getName() ) );
      lnk.setText( m_project.getName() );

      // lock project
      createLockHyperlink( body, toolkit, preferences );

      // info
      RemoteProjectRowBuilder.getInfoLink( m_bean, body, toolkit );

      // export
      LocalProjectRowBuilder.getExportLink( m_project, body, toolkit );

      // spacer
      toolkit.createLabel( body, "    " ); //$NON-NLS-1$

      /* delete */
      LocalProjectRowBuilder.getDeleteLink( m_project, body, toolkit );
    }
    catch( final CoreException e1 )
    {
      e1.printStackTrace();
    }
  }

  private void createLockHyperlink( final Composite body, final FormToolkit toolkit, final IRemoteProjectPreferences preferences ) throws CoreException
  {

    if( preferences.isLocked() )
    {
      final ImageHyperlink lnkLock = toolkit.createImageHyperlink( body, SWT.NONE );
      lnkLock.setImage( IMG_LORE_COMMIT_AND_UNLOCK );
      lnkLock.setToolTipText( String.format( "�bertrage Projekt \"%s\" in Modelldaten-Basis und gebe Projekt vom Editieren frei.", m_project.getName() ) );

      lnkLock.addHyperlinkListener( new HyperlinkAdapter()
      {
        /**
         * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
         */
        @Override
        public void linkActivated( final HyperlinkEvent e )
        {
          final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();

          /* commit */
          final CommitProjectWorker commit = new CommitProjectWorker( m_project, m_bean );
          final IStatus commitStatus = ProgressUtilities.busyCursorWhile( commit );

          if( !shell.isDisposed() )
            ErrorDialog.openError( shell, "Fehler", "Aktualisieren des Projektes ist fehlgeschlagen.", commitStatus );

          if( !commitStatus.isOK() )
            return;

          /* release */
          final ReleaseProjectLockWorker handler = new ReleaseProjectLockWorker( m_project, m_bean, preferences );
          final IStatus status = ProgressUtilities.busyCursorWhile( handler );

          if( !shell.isDisposed() )
            ErrorDialog.openError( shell, "Fehler", "Freigeben des Projektes ist fehlgeschlagen.", status );
        }
      } );
    }
    else
    {
      final ImageHyperlink lnkLock = toolkit.createImageHyperlink( body, SWT.NONE );
      lnkLock.setImage( IMG_LORE_LOCK );
      lnkLock.setToolTipText( String.format( "Sperre Projekt \"%s\" zum Editieren.", m_project.getName() ) );

      lnkLock.addHyperlinkListener( new HyperlinkAdapter()
      {
        /**
         * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
         */
        @Override
        public void linkActivated( final HyperlinkEvent e )
        {
          final AcquireProjectLockWorker handler = new AcquireProjectLockWorker( m_project, m_bean, preferences );
          final IStatus status = ProgressUtilities.busyCursorWhile( handler );

          final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
          if( !shell.isDisposed() )
            ErrorDialog.openError( shell, "Fehler", "Sperren des Projektes zum Editieren ist fehlgeschlagen.", status );
        }
      } );
    }

  }
}
