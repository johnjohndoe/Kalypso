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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.km.internal.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultQIntervalNode;

/**
 * FIXME: remove this. Use databinding, we do not need all this terrible overhead.
 *
 * @author Andreas Doemming (original)
 * @author Holger Albert (modified)
 */
public class DirectoryFieldWidget implements ISelectionProvider
{
  /**
   * The listeners.
   */
  private final List<ISelectionChangedListener> m_listeners;

  /**
   * The text field.
   */
  protected Text m_text;

  /**
   * The button.
   */
  private Button m_button;

  private IWspmResultNode[] m_rootNodes;

  private final IWizardContainer m_context;

  public DirectoryFieldWidget( final Composite parent, final IWizardContainer context )
  {
    m_context = context;
    m_listeners = new ArrayList<>();
    m_text = null;
    m_button = null;

    createControls( parent );
  }

  private void createControls( final Composite parent )
  {
    /* Create a label. */
    final Label label1 = new Label( parent, SWT.NONE );
    label1.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );

    label1.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMViewer.1" ) ); //$NON-NLS-1$
    label1.setToolTipText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMViewer.0" ) ); //$NON-NLS-1$

    /* Create a text. */
    m_text = new Text( parent, SWT.BORDER );
    m_text.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_text.setToolTipText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.DirectoryFieldWidget.0" ) ); //$NON-NLS-1$
    m_text.setEditable( false );

    /* Add a listener. */
// m_text.addFocusListener( new FocusAdapter()
// {
// /**
// * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
// */
// @Override
// public void focusLost( final FocusEvent e )
// {
// fireSelectionChangeEvent();
// }
// } );

    /* Create a button. */
    m_button = new Button( parent, SWT.NONE );
    m_button.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.DirectoryFieldWidget.1" ) ); //$NON-NLS-1$
    m_button.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );
    m_button.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleBrowseClicked( parent.getDisplay() );
      }
    } );
  }

  protected void handleBrowseClicked( final Display display )
  {
    final IWspmResultNode[] rootNodes = readNodes();
    final BrowseWspmDialog dialog = new BrowseWspmDialog( display.getActiveShell(), rootNodes );
    final int open = dialog.open();
    if( open != Window.OK )
      return;

    final IWspmResultNode selectedNode = dialog.getSelectedNode();
    if( !(selectedNode instanceof WspmResultQIntervalNode) )
      return;

    final IPath path = (IPath) selectedNode.getObject();
    m_text.setText( path.toString() );

    fireSelectionChangeEvent();
  }

  protected IWspmResultNode[] readNodes( )
  {
    if( m_rootNodes == null )
    {
      final ReadNodesOperation operation = new ReadNodesOperation();
      final IStatus status = RunnableContextHelper.execute( m_context, true, false, operation );
      m_rootNodes = operation.getNodes();
      if( !status.isOK() )
        new StatusDialog( m_context.getShell(), status, Messages.getString( "DirectoryFieldWidget.2" ) ).open(); //$NON-NLS-1$
    }

    return m_rootNodes;
  }

  /**
   * This function fires a selection changed event.
   */
  protected void fireSelectionChangeEvent( )
  {
    final SelectionChangedEvent event = new SelectionChangedEvent( this, getSelection() );
    for( final ISelectionChangedListener listener : m_listeners )
      listener.selectionChanged( event );
  }

  @Override
  public ISelection getSelection( )
  {
    return new StructuredSelection( m_text.getText() );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  @Override
  public void addSelectionChangedListener( final ISelectionChangedListener listener )
  {
    m_listeners.add( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  @Override
  public void removeSelectionChangedListener( final ISelectionChangedListener listener )
  {
    m_listeners.remove( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void setSelection( final ISelection selection )
  {
    if( selection.isEmpty() )
      m_text.setText( "" ); //$NON-NLS-1$

    if( selection instanceof IStructuredSelection )
    {
      final Object firstElement = ((IStructuredSelection) selection).getFirstElement();
      if( firstElement instanceof String )
        m_text.setText( (String) firstElement );
    }
  }

  /**
   * This function enables/disables the controls.
   *
   * @param enabled
   *          True for enabled and false for disabled.
   */
  public void setEnabled( final boolean enabled )
  {
    m_button.setEnabled( enabled );
  }
}