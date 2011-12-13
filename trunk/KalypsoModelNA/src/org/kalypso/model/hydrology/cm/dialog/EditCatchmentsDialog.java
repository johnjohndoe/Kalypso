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
package org.kalypso.model.hydrology.cm.dialog;

import org.eclipse.jface.dialogs.TrayDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;

/**
 * This dialog allowes the editing of catchments of a catchment model.
 * 
 * @author Holger Albert
 */
public class EditCatchmentsDialog extends TrayDialog
{
  /**
   * The viewer.
   */
  private TreeViewer m_viewer;

  /**
   * The details group.
   */
  private Group m_detailsGroup;

  /**
   * The constructor.
   * 
   * @param parentShell
   *          The parent shell, or null to create a top-level shell.
   */
  public EditCatchmentsDialog( Shell shell )
  {
    super( shell );

    m_viewer = null;
    m_detailsGroup = null;
  }

  /**
   * The constructor.
   * 
   * @param parentShell
   *          The object that returns the current parent shell.
   */
  public EditCatchmentsDialog( IShellProvider parentShell )
  {
    super( parentShell );

    m_viewer = null;
    m_detailsGroup = null;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( Composite parent )
  {
    /* Set the title. */
    getShell().setText( "Zeitreihenablage konfigurieren" );

    /* Create the main composite. */
    Composite main = (Composite) super.createDialogArea( parent );
    main.setLayout( new GridLayout( 2, true ) );
    GridData mainData = new GridData( SWT.FILL, SWT.FILL, true, true );
    mainData.heightHint = 400;
    mainData.widthHint = 550;
    main.setLayoutData( mainData );

    /* Get the input. */
    // TODO

    /* Create a viewer. */
    /* The selection of this viewer will determine the content of the details composite. */
    m_viewer = new TreeViewer( main, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.SINGLE );
    m_viewer.getTree().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_viewer.getTree().setLinesVisible( true );
    m_viewer.getTree().setHeaderVisible( true );
    // m_viewer.setContentProvider( new TreeNodeContentProvider() );
    // m_viewer.setLabelProvider( new TreeNodeLabelProvider() );

    /* Set the input. */
    // TODO

    /* Add a listener. */
    m_viewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      /**
       * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
       */
      @Override
      public void selectionChanged( SelectionChangedEvent event )
      {
        // TODO
      }
    } );

    /* Create the details group. */
    m_detailsGroup = new Group( main, SWT.NONE );
    m_detailsGroup.setLayout( new GridLayout( 1, false ) );
    m_detailsGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_detailsGroup.setText( "Details" );

    /* Create the content of the details group. */
    createDetailsContent( m_detailsGroup );

    return main;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#isResizable()
   */
  @Override
  protected boolean isResizable( )
  {
    return true;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#okPressed()
   */
  @Override
  protected void okPressed( )
  {
    // TODO

    /* Dispose the dialog. */
    dispose();

    super.okPressed();
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#cancelPressed()
   */
  @Override
  protected void cancelPressed( )
  {
    /* Dispose the dialog. */
    dispose();

    super.cancelPressed();
  }

  /**
   * This function creates the content of the details group.
   * 
   * @param parent
   *          The parent composite.
   */
  private void createDetailsContent( Composite parent )
  {
    // TODO
  }

  /**
   * This function disposes the dialog.
   */
  private void dispose( )
  {
    m_viewer = null;
    m_detailsGroup = null;
  }
}