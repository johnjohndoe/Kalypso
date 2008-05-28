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
package org.kalypso.ui.wizard.wms.pages;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.jface.dialogs.TrayDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ui.ImageProvider;

/**
 * This dialog is for manageing the favorites of the WMS wizard page.
 * 
 * @author Holger Albert
 */
public class WMSFavoritesDialog extends TrayDialog
{
  /**
   * The last successfull used service URL.
   */
  private String m_lastService;

  /**
   * All favorite services.
   */
  protected List<String> m_lastServices;

  /**
   * All favorite services, which are displayed in the list viewer. This list can be modified by the viewer.
   */
  protected List<String> m_favoriteServices;

  /**
   * This variable stores the current selected service.
   */
  protected String m_selectedService;

  /**
   * The constructor.
   * 
   * @param shell
   *            The parent shell, or null to create a top-level shell.
   * @param lastService
   *            The last successfull used service URL.
   * @param lastServices
   *            The last successfull used services. The favorites, to call them another way.
   */
  public WMSFavoritesDialog( Shell shell, String lastService, List<String> lastServices )
  {
    super( shell );

    /* Initialize. */
    init( lastService, lastServices );
  }

  /**
   * The constructor.
   * 
   * @param parentShell
   *            The object that returns the current parent shell.
   * @param lastService
   *            The last successfull used service URL.
   * @param lastServices
   *            The last successfull used services. The favorites, to call them another way.
   */
  public WMSFavoritesDialog( IShellProvider parentShell, String lastService, List<String> lastServices )
  {
    super( parentShell );

    /* Initialize. */
    init( lastService, lastServices );
  }

  /**
   * This function initializes the dialog.
   * 
   * @param lastService
   *            The last successfull used service URL.
   * @param lastServices
   *            The last successfull used services. The favorites, to call them another way.
   */
  private void init( String lastService, List<String> lastServices )
  {
    /* Initialize. */
    m_lastService = lastService;
    m_lastServices = lastServices;

    /* Copy all services to a list, which is allowed to modify. */
    m_favoriteServices = new LinkedList<String>();
    for( int i = 0; i < m_lastServices.size(); i++ )
      m_favoriteServices.add( m_lastServices.get( i ) );

    /* There is no selected service at the beginning. */
    m_selectedService = null;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( Composite parent )
  {
    /* Set the title. */
    getShell().setText( "WMS Favoriten" );

    /* Create the main composite. */
    Composite panel = (Composite) super.createDialogArea( parent );
    panel.setLayout( new GridLayout( 1, false ) );
    panel.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    /* Selection composite. */
    Composite selectionComposite = new Composite( panel, SWT.NONE );
    selectionComposite.setLayout( new GridLayout( 2, false ) );
    selectionComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* The selection label. */
    Label selectionLabel = new Label( selectionComposite, SWT.NONE );
    selectionLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    selectionLabel.setText( "Ausgew‰hlter WMS Service: " );

    /* The selection service text. */
    final Text selectionServiceText = new Text( selectionComposite, SWT.BORDER );
    GridData selectionServiceData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    selectionServiceData.widthHint = 200;
    selectionServiceData.minimumWidth = 200;
    selectionServiceText.setLayoutData( selectionServiceData );
    selectionServiceText.setText( "" );
    selectionServiceText.setToolTipText( "Dieser Service ist gerade ausgew‰hlt. Wenn Sie den Dialog mit OK verlassen, wird dieser aktiv werden." );
    selectionServiceText.setEditable( false );

    /* The service group. */
    Group serviceGroup = new Group( panel, SWT.NONE );
    serviceGroup.setLayout( new GridLayout( 1, false ) );
    serviceGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    serviceGroup.setText( "Ihre Favoriten" );

    /* The service viewer. */
    final ListViewer serviceViewer = new ListViewer( serviceGroup, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL | SWT.H_SCROLL );
    GridData serviceViewerData = new GridData( SWT.FILL, SWT.FILL, true, true );
    serviceViewerData.heightHint = 200;
    serviceViewerData.minimumHeight = 200;
    serviceViewerData.widthHint = 475;
    serviceViewerData.minimumWidth = 475;
    serviceViewer.getList().setLayoutData( serviceViewerData );

    /* Set the content provider. */
    serviceViewer.setContentProvider( new ArrayContentProvider() );

    /* Set the label provider. */
    serviceViewer.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object)
       */
      @Override
      public Image getImage( Object element )
      {
        Image image = ImageProvider.IMAGE_ICON_GMT.createImage();

        return image;
      }
    } );

    /* Set the input. */
    serviceViewer.setInput( m_favoriteServices );

    /* The delete service button. */
    Button deleteServiceButton = new Button( serviceGroup, SWT.NONE );
    deleteServiceButton.setLayoutData( new GridData( SWT.END, SWT.CENTER, true, false ) );
    deleteServiceButton.setText( "Entfernen" );
    deleteServiceButton.setToolTipText( "Entfernen des aktuell ausgew‰hlten Services aus der Liste der Favoriten." );

    /* Add the required listener here. */

    /* The modify listener for the selection service text. */
    selectionServiceText.addModifyListener( new ModifyListener()
    {
      /**
       * @see org.eclipse.swt.events.ModifyListener#modifyText(org.eclipse.swt.events.ModifyEvent)
       */
      public void modifyText( ModifyEvent e )
      {
        Text source = (Text) e.getSource();
        String text = source.getText();

        if( !text.equals( "" ) )
        {
          m_selectedService = text;
          return;
        }

        m_selectedService = null;
      }
    } );

    /* The selection changed listener for the service viewer. */
    serviceViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      /**
       * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
       */
      public void selectionChanged( SelectionChangedEvent event )
      {
        ISelection selection = event.getSelection();
        if( !(selection instanceof StructuredSelection) )
          return;

        StructuredSelection structuredSelection = (StructuredSelection) selection;

        /* There could be only one element selected. */
        Object firstElement = structuredSelection.getFirstElement();
        if( !(firstElement instanceof String) )
          return;

        String selectedFavorite = (String) firstElement;

        /* Set it as selected. */
        selectionServiceText.setText( selectedFavorite );
      }
    } );

    /* The selection listener for the delete service button. */
    deleteServiceButton.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        ISelection selection = serviceViewer.getSelection();
        if( selection instanceof StructuredSelection )
        {
          /* There could be only one element selected. */
          m_favoriteServices.removeAll( ((StructuredSelection) selection).toList() );

          /* Refresh the viewer. */
          serviceViewer.refresh();

          /* Reset the selected fields, in case, it was the deleted one. They will reset the result members. */
          selectionServiceText.setText( "" );
        }
      }
    } );

    /* Set the default, if possible. */
    if( m_lastService != null )
    {
      if( m_favoriteServices.contains( m_lastService ) )
        serviceViewer.setSelection( new StructuredSelection( m_lastService ) );
    }

    return panel;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#okPressed()
   */
  @Override
  protected void okPressed( )
  {
    /* Update the given favorites list, with the new modified list. */
    for( int i = 0; i < m_lastServices.size(); i++ )
    {
      /* Get the service. */
      String favorite = m_lastServices.get( i );

      /* If it is not on the modified list anymore, remove it from the original list. */
      if( !m_favoriteServices.contains( favorite ) )
        m_lastServices.remove( favorite );
    }

    super.okPressed();
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#cancelPressed()
   */
  @Override
  protected void cancelPressed( )
  {
    /* There should never be a result, if cancel is pressed. */
    m_selectedService = null;

    super.cancelPressed();
  }

  /**
   * This function returns the selected service.
   * 
   * @return The selected service.
   */
  public String getSelectedService( )
  {
    return m_selectedService;
  }
}