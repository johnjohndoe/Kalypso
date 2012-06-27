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
package org.kalypso.model.km.internal.ui.kmupdate;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.java.lang.Arrays;
import org.kalypso.contribs.eclipse.jface.viewers.ArrayTreeContentProvider;
import org.kalypso.model.km.internal.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultContentProvider;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLabelProvider;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultQIntervalNode;

/**
 * A dialog which shows the WSPM projects and relevant data.
 * 
 * @author Holger Albert
 */
public class BrowseWspmDialog extends Dialog
{
  private final IWspmResultNode[] m_rootNodes;

  private IWspmResultNode m_selectedNode;

  /**
   * @param parentShell
   *          The parent shell, or null to create a top-level shell.
   * @param rootNodes
   *          The root nodes.
   */
  public BrowseWspmDialog( final Shell parentShell, final IWspmResultNode[] rootNodes )
  {
    super( parentShell );

    m_rootNodes = rootNodes;
  }

  /**
   * @param parentShell
   *          Object that returns the current parent shell.
   * @param rootNodes
   *          The root nodes.
   */
  public BrowseWspmDialog( final IShellProvider parentShell, final IWspmResultNode[] rootNodes )
  {
    super( parentShell );

    m_rootNodes = rootNodes;
  }

  @Override
  protected boolean isResizable( )
  {
    return true;
  }

  @Override
  protected Control createDialogArea( final Composite parent )
  {
    /* Set the title. */
    getShell().setText( Messages.getString( "BrowseWspmDialog_0" ) ); //$NON-NLS-1$

    /* Create the main composite. */
    final Composite main = (Composite) super.createDialogArea( parent );
    main.setLayout( new GridLayout( 1, false ) );
    final GridData mainData = new GridData( SWT.FILL, SWT.FILL, true, true );
    mainData.heightHint = 400;
    mainData.widthHint = 400;
    main.setLayoutData( mainData );

    /* Create a tree viewer. */
    final TreeViewer treeViewer = new TreeViewer( main, SWT.SINGLE | SWT.BORDER );
    treeViewer.getTree().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    WspmResultContentProvider.initTreeViewer( treeViewer );

    if( Arrays.isEmpty( m_rootNodes ) )
    {
      treeViewer.setContentProvider( new ArrayTreeContentProvider() );
      treeViewer.setLabelProvider( new LabelProvider() );
      treeViewer.setInput( new String[] { Messages.getString("BrowseWspmDialog.0") } ); //$NON-NLS-1$
    }
    else
    {
      treeViewer.setContentProvider( new WspmResultContentProvider() );
      treeViewer.setLabelProvider( new WspmResultLabelProvider( treeViewer ) );
      treeViewer.addFilter( new WspmResultViewerFilter() );
      treeViewer.setInput( m_rootNodes );
    }

    /* Add a listener. */
    treeViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final ISelection selection = event.getSelection();
        if( selection.isEmpty() || !(selection instanceof IStructuredSelection) )
          return;

        handleSelectionChanged( (IStructuredSelection) selection );
      }
    } );

    return main;
  }

  protected void handleSelectionChanged( final IStructuredSelection selection )
  {
    final Object firstElement = selection.getFirstElement();
    if( !(firstElement instanceof IWspmResultNode) )
      return;

    m_selectedNode = (IWspmResultNode) firstElement;

    /* Check, if the dialog is allowed to be completed. */
    checkDialogComplete();
  }

  @Override
  protected void createButtonsForButtonBar( final Composite parent )
  {
    super.createButtonsForButtonBar( parent );

    /* Check, if the dialog is allowed to be completed. */
    checkDialogComplete();
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#okPressed()
   */
  @Override
  protected void okPressed( )
  {
    /* HINT: A selection should be already made here. */

    super.okPressed();
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#cancelPressed()
   */
  @Override
  protected void cancelPressed( )
  {
    m_selectedNode = null;

    super.cancelPressed();
  }

  /**
   * This function checks, if the dialog is allowed to be completed.
   */
  protected void checkDialogComplete( )
  {
    /* Get the OK button. */
    final Button okButton = getButton( IDialogConstants.OK_ID );

    /* First of all, it should be allowed to complete. */
    okButton.setEnabled( true );

    if( m_selectedNode == null )
      okButton.setEnabled( false );

    if( !(m_selectedNode instanceof WspmResultQIntervalNode) )
      okButton.setEnabled( false );
  }

  /**
   * This function returns the selected node or null, if cancel was pressed.
   * 
   * @return The selected node or null.
   */
  public IWspmResultNode getSelectedNode( )
  {
    return m_selectedNode;
  }
}