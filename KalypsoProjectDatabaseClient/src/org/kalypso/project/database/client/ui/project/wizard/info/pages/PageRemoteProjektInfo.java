/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraï¿½e 22
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
package org.kalypso.project.database.client.ui.project.wizard.info.pages;

import java.net.MalformedURLException;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.client.core.utils.KalypsoProjectBeanHelper;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * @author Dirk Kuch
 */
public class PageRemoteProjektInfo extends WizardPage
{
  private final KalypsoProjectBean[] m_beans;

  private final KalypsoProjectBean m_bean;

  public PageRemoteProjektInfo( final KalypsoProjectBean bean )
  {
    super( "KalypsoProjectBean" ); //$NON-NLS-1$
    m_bean = bean;

    setTitle( "Projektinformationen" );
    setDescription( "Wählen Sie eine Version aus der Auswahlbox, um nähere Information zu dieser zu erfahren." );

    m_beans = KalypsoProjectBeanHelper.getSortedBeans( bean );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    setPageComplete( false );

    final Composite container = new Composite( parent, SWT.NULL );
    container.setLayout( new GridLayout() );
    setControl( container );

    /* select version */
    final Group groupVersions = new Group( container, SWT.NULL );
    groupVersions.setLayout( new GridLayout() );
    groupVersions.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    groupVersions.setText( String.format( "Version des Projektes: %s", m_bean.getName() ) );

    final ComboViewer viewerVersions = new ComboViewer( groupVersions );
    viewerVersions.getCombo().setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    viewerVersions.setContentProvider( new ArrayContentProvider() );
    viewerVersions.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        if( element instanceof KalypsoProjectBean )
        {
          final KalypsoProjectBean project = (KalypsoProjectBean) element;

          return String.format( "Version: %d vom %tF", project.getProjectVersion(), project.getCreationDate() );
        }

        return super.getText( element );
      }
    } );

    viewerVersions.setInput( m_beans );

    final Group groupDetails = new Group( container, SWT.NULL );
    groupDetails.setLayout( new GridLayout( 2, false ) );
    groupDetails.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    groupDetails.setText( "Details" );

    /* name */
    new Label( groupDetails, SWT.NULL ).setText( "Name:" );

    final Text name = new Text( groupDetails, SWT.BORDER | SWT.READ_ONLY );
    name.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    /* description */
    final Label labelDescription = new Label( groupDetails, SWT.TOP );
    labelDescription.setText( "Beschreibung:" );
    labelDescription.setLayoutData( new GridData( GridData.FILL, GridData.FILL, false, false ) );

    final Text description = new Text( groupDetails, SWT.BORDER | SWT.READ_ONLY | SWT.MULTI | SWT.WRAP );
    description.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    /* version */
    new Label( groupDetails, SWT.NULL ).setText( "Version:" );

    final Text version = new Text( groupDetails, SWT.BORDER | SWT.READ_ONLY );
    version.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    /* type */
    new Label( groupDetails, SWT.NULL ).setText( "Projekttyp:" );

    final Text type = new Text( groupDetails, SWT.BORDER | SWT.READ_ONLY );
    type.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    /* unix name */
    new Label( groupDetails, SWT.NULL ).setText( "Unix Name:" );

    final Text unix = new Text( groupDetails, SWT.BORDER | SWT.READ_ONLY );
    unix.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    /* server url */
    new Label( groupDetails, SWT.NULL ).setText( "Url:" );

    final Text url = new Text( groupDetails, SWT.BORDER | SWT.READ_ONLY );
    url.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    /* change listener */
    viewerVersions.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) viewerVersions.getSelection();
        final Object element = selection.getFirstElement();

        if( element instanceof KalypsoProjectBean )
        {
          try
          {
            final KalypsoProjectBean project = (KalypsoProjectBean) element;

            name.setText( project.getName() );
            description.setText( project.getDescription() );
            version.setText( String.format( "Version %d erstellt am  %tc", project.getProjectVersion(), project.getCreationDate() ) );
            type.setText( project.getProjectType() );
            unix.setText( project.getUnixName() );
            url.setText( project.getUrl().toExternalForm() );
          }
          catch( final MalformedURLException e )
          {
            KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
          }
        }

      }
    } );

    viewerVersions.setSelection( new StructuredSelection( m_bean ) );
  }
}
