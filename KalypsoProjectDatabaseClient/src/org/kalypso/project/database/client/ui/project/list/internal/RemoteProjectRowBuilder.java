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
package org.kalypso.project.database.client.ui.project.list.internal;

import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.dialogs.IPageChangedListener;
import org.eclipse.jface.dialogs.PageChangedEvent;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.contribs.eclipse.core.resources.ProjectTemplate;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.client.core.utils.KalypsoProjectBeanHelper;
import org.kalypso.project.database.client.ui.project.wizard.create.WizardCreateProject;
import org.kalypso.project.database.client.ui.project.wizard.info.WizardInfoRemoteProject;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * @author Dirk Kuch
 */
public class RemoteProjectRowBuilder extends AbstractProjectRowBuilder implements IProjectRowBuilder
{
  protected final KalypsoProjectBean m_bean;

  public RemoteProjectRowBuilder( final KalypsoProjectBean bean )
  {
    m_bean = bean;
  }

  /**
   * @see org.kalypso.project.database.client.ui.project.internal.IProjectRowBuilder#render(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  @Override
  public void render( final Composite parent, final FormToolkit toolkit )
  {

    final Composite body = toolkit.createComposite( parent );
    body.setLayout( new GridLayout( 3, false ) );
    body.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final ImageHyperlink lnk = toolkit.createImageHyperlink( body, SWT.NONE );
    lnk.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    lnk.setImage( IMG_REMOTE_PROJECT );
    lnk.setText( m_bean.getName() );

    /* info */
    getInfoLink( m_bean, body, toolkit );

    /* import */
    getImportLink( m_bean, body, toolkit );

  }

  protected static void getImportLink( final KalypsoProjectBean bean, final Composite body, final FormToolkit toolkit )
  {
    final ImageHyperlink lnkImport = toolkit.createImageHyperlink( body, SWT.NONE );
    lnkImport.setImage( IMG_IMPORT_REMOTE );
    lnkImport.setToolTipText( String.format( "Importiere Projekt: %s", bean.getName() ) );

    lnkImport.addHyperlinkListener( new HyperlinkAdapter()
    {
      /**
       * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
       */
      @Override
      public void linkActivated( final HyperlinkEvent e )
      {

        try
        {
          /* sort beans */
          final KalypsoProjectBean[] beans = KalypsoProjectBeanHelper.getSortedBeans( bean );
          final List<ProjectTemplate> templates = new ArrayList<ProjectTemplate>();

          for( final KalypsoProjectBean b : beans )
          {
            final ProjectTemplate template = new ProjectTemplate( String.format( "%s - Version %d", b.getName(), bean.getProjectVersion() ), bean.getUnixName(), bean.getDescription(), null, bean.getUrl() );

            templates.add( template );
          }

          final WizardCreateProject wizard = new WizardCreateProject( templates.toArray( new ProjectTemplate[] {} ), new String[] {} );
          wizard.init( PlatformUI.getWorkbench(), null );

          final WizardDialog2 dialog = new WizardDialog2( null, wizard );
          dialog.setRememberSize( true );

          dialog.addPageChangedListener( new IPageChangedListener()
          {
            public void pageChanged( final PageChangedEvent event )
            {
              final Object page = event.getSelectedPage();

              if( page instanceof IUpdateable )
              {
                final IUpdateable update = (IUpdateable) page;
                update.update();
              }
              else if( page instanceof WizardNewProjectCreationPage )
              {
                final WizardNewProjectCreationPage myPage = (WizardNewProjectCreationPage) page;
                final Composite myParent = (Composite) myPage.getControl();
                final Control[] children = myParent.getChildren();

                /* project name */
                final Composite subChildOne = (Composite) children[0];
                final Control[] subChildrenOne = subChildOne.getChildren();
                subChildrenOne[1].setEnabled( false );

                /* working sets */
                final Composite subChildTwo = (Composite) children[2];
                final Control[] subChildrenTwo = subChildTwo.getChildren();
                final Composite subSubChildTwo = (Composite) subChildrenTwo[0];
                final Control[] subSubChildrenTwo = subSubChildTwo.getChildren();
                subSubChildrenTwo[0].setEnabled( false );
              }
            }
          } );

          dialog.open();
        }
        catch( final MalformedURLException e1 )
        {
          KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e1 ) );
        }
      }
    } );

  }

  protected static void getInfoLink( final KalypsoProjectBean bean, final Composite body, final FormToolkit toolkit )
  {
    final ImageHyperlink lnkInfo = toolkit.createImageHyperlink( body, SWT.NONE );
    lnkInfo.setImage( IMG_REMOTE_INFO );
    lnkInfo.setToolTipText( String.format( "Projektinformationen: %s", bean.getName() ) );

    lnkInfo.addHyperlinkListener( new HyperlinkAdapter()
    {
      /**
       * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
       */
      @Override
      public void linkActivated( final HyperlinkEvent e )
      {
        final WizardInfoRemoteProject wizard = new WizardInfoRemoteProject( bean );

        final WizardDialog dialog = new WizardDialog( null, wizard );
        dialog.open();
      }
    } );

  }
}
