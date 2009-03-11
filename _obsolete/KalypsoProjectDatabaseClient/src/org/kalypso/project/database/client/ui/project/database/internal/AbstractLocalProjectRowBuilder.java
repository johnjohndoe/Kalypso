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
package org.kalypso.project.database.client.ui.project.database.internal;

import java.util.Properties;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.SameShellProvider;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.DeleteResourceAction;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.afgui.extension.IKalypsoProjectOpenAction;
import org.kalypso.afgui.extension.IProjectDatabaseUiLocker;
import org.kalypso.project.database.client.core.model.interfaces.ILocalProject;
import org.kalypso.project.database.client.i18n.Messages;
import org.kalypso.project.database.client.ui.project.wizard.export.WizardProjectExport;
import org.kalypso.project.database.client.ui.project.wizard.info.LocalInfoDialog;

/**
 * @author Dirk Kuch
 */
public abstract class AbstractLocalProjectRowBuilder extends AbstractProjectRowBuilder
{
  public static Image IMG_LOCAL_PROJECT = new Image( null, AbstractLocalProjectRowBuilder.class.getResourceAsStream( "icons/local.gif" ) ); //$NON-NLS-1$

  public static Image IMG_EXPORT_LOCAL = new Image( null, AbstractLocalProjectRowBuilder.class.getResourceAsStream( "icons/export_local.gif" ) ); //$NON-NLS-1$

  public static Image IMG_REMOTE_INFO = new Image( null, AbstractLocalProjectRowBuilder.class.getResourceAsStream( "icons/info_remote.gif" ) ); //$NON-NLS-1$

  public static Image IMG_DELETE_LOCAL = new Image( null, AbstractLocalProjectRowBuilder.class.getResourceAsStream( "icons/delete_local.gif" ) ); //$NON-NLS-1$

  private final ILocalProject m_local;

  public AbstractLocalProjectRowBuilder( final ILocalProject local, final IKalypsoProjectOpenAction action, final IProjectDatabaseUiLocker locker )
  {
    super( action, locker );
    m_local = local;
  }

  protected ILocalProject getLocalProject( )
  {
    return m_local;
  }

  protected void addProjectOpenListener( final ImageHyperlink lnk )
  {
    lnk.addHyperlinkListener( new HyperlinkAdapter()
    {
      /**
       * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
       */
      @Override
      public void linkActivated( final HyperlinkEvent e )
      {
        new UIJob( "" ) //$NON-NLS-1$
        {
          @Override
          public IStatus runInUIThread( final IProgressMonitor monitor )
          {
            final Properties properties = new Properties();
            properties.setProperty( "project", getLocalProject().getProject().getName() ); //$NON-NLS-1$

            return getOpenAction().open( properties );
          }
        }.schedule();
      }
    } );
  }

  protected void getLocalInfoLink( final Composite body, final FormToolkit toolkit )
  {
    final ImageHyperlink lnkInfo = toolkit.createImageHyperlink( body, SWT.NONE );
    lnkInfo.setToolTipText( String.format( Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.AbstractLocalProjectRowBuilder.6"), getLocalProject().getName() ) ); //$NON-NLS-1$

    lnkInfo.setImage( IMG_REMOTE_INFO );

    lnkInfo.addHyperlinkListener( new HyperlinkAdapter()
    {
      /**
       * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
       */
      @Override
      public void linkActivated( final HyperlinkEvent e )
      {
        try
        {
          getLocker().acquireUiUpdateLock();

          final LocalInfoDialog dialog = new LocalInfoDialog( getLocalProject(), lnkInfo.getShell() );
          dialog.open();
        }
        finally
        {
          getLocker().releaseUiUpdateLock();
        }

      }
    } );
  }

  protected void getExportLink( final Composite body, final FormToolkit toolkit )
  {
    final ImageHyperlink lnkExport = toolkit.createImageHyperlink( body, SWT.NONE );
    lnkExport.setImage( IMG_EXPORT_LOCAL );
    lnkExport.setToolTipText( String.format( Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.AbstractLocalProjectRowBuilder.7"), getLocalProject().getName() ) ); //$NON-NLS-1$

    lnkExport.addHyperlinkListener( new HyperlinkAdapter()
    {
      /**
       * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
       */
      @Override
      public void linkActivated( final HyperlinkEvent e )
      {
        try
        {
          getLocker().acquireUiUpdateLock();

          final WizardProjectExport wizard = new WizardProjectExport( getLocalProject().getProject() );
          wizard.init( PlatformUI.getWorkbench(), new StructuredSelection( getLocalProject().getProject() ) );

          final WizardDialog dialog = new WizardDialog( null, wizard );
          dialog.open();
        }
        finally
        {
          getLocker().releaseUiUpdateLock();
        }
      }
    } );
  }

  protected void getDeleteLink( final Composite body, final FormToolkit toolkit )
  {
    final ImageHyperlink lnkDelete = toolkit.createImageHyperlink( body, SWT.NONE );
    lnkDelete.setImage( IMG_DELETE_LOCAL );
    lnkDelete.setToolTipText( String.format( Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.AbstractLocalProjectRowBuilder.8"), getLocalProject().getName() ) ); //$NON-NLS-1$

    lnkDelete.addHyperlinkListener( new HyperlinkAdapter()
    {
      /**
       * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
       */
      @Override
      public void linkActivated( final HyperlinkEvent e )
      {
        try
        {
          getLocker().acquireUiUpdateLock();

          final DeleteResourceAction action = new DeleteResourceAction( new SameShellProvider( lnkDelete.getShell() ) );
          action.selectionChanged( new StructuredSelection( getLocalProject().getProject() ) );
          action.run();
        }
        finally
        {
          getLocker().releaseUiUpdateLock();
        }
      }
    } );

  }

}
