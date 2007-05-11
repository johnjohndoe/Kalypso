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
package org.kalypso.ui.rrm.kmupdate;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ui.rrm.Messages;

/**
 * @author doemming
 */
public class DirectoryFieldWidget implements ISelectionProvider
{

  final Text m_text;

  final boolean m_dirOnly;

  private final List<ISelectionChangedListener> m_listeners = new ArrayList<ISelectionChangedListener>();

  public DirectoryFieldWidget( final String label, final String toolTip, final boolean dirOnly, final Composite parent, int sp1, int sp2, int sp3 )
  {
    m_dirOnly = dirOnly;
    final Label label1 = new Label( parent, SWT.NONE );
    label1.setText( label );
    label1.setToolTipText( toolTip );
    GridData data = new GridData();
    data.horizontalSpan = sp1;
    label1.setLayoutData( data );
    m_text = new Text( parent, SWT.NONE );
    m_text.setToolTipText( Messages.getString("DirectoryFieldWidget.0") ); // TODO always show complete path as tooltip //$NON-NLS-1$
    GridData data2 = new GridData( GridData.FILL_HORIZONTAL );
    data2.horizontalSpan = sp2;
    data2.grabExcessHorizontalSpace = true;
    m_text.setLayoutData( data2 );
    m_text.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusLost( FocusEvent e )
      {
        fireSelectionChangeEvent();
      }
    } );

    final Button button = new Button( parent, SWT.NONE );
    button.setText( Messages.getString("DirectoryFieldWidget.1") ); //$NON-NLS-1$
    GridData data3 = new GridData();
    data3.horizontalSpan = sp3;
    button.setLayoutData( data3 );
    button.addSelectionListener( new SelectionAdapter()
    {
      private String m_lastPath = null;

      @Override
      public void widgetSelected( SelectionEvent e )
      {
        if( m_dirOnly )
        {
          final DirectoryDialog dialog = new DirectoryDialog( parent.getShell() );
          if( m_lastPath != null )
            dialog.setFilterPath( m_lastPath );
          // dialog.setText( );
          String filePath = dialog.open();
          m_text.setText( filePath );
          m_lastPath = filePath;
          fireSelectionChangeEvent();
        }
        else
        {
          FileDialog dialog = new FileDialog( parent.getShell() );
          if( m_lastPath != null )
            dialog.setFilterPath( m_lastPath );
          // dialog.setText( "text" );
          String filePath = dialog.open();
          m_text.setText( filePath );
          m_lastPath = filePath;
          fireSelectionChangeEvent();
        }
      }
    } );
  }

  void fireSelectionChangeEvent( )
  {
    final SelectionChangedEvent event = new SelectionChangedEvent( this, getSelection() );
    for( Iterator iter = m_listeners.iterator(); iter.hasNext(); )
    {
      final ISelectionChangedListener element = (ISelectionChangedListener) iter.next();
      element.selectionChanged( event );
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  public ISelection getSelection( )
  {
    final String value = m_text.getText();
    return new StructuredSelection( value );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void addSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_listeners.add( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void removeSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_listeners.remove( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( ISelection selection )
  {
    if( selection.isEmpty() )
      m_text.setText( "" ); //$NON-NLS-1$
    if( selection instanceof IStructuredSelection )
    {
      final Object firstElement = ((IStructuredSelection) selection).getFirstElement();
      if( firstElement instanceof String )
        m_text.setText( (String) firstElement ); // fire event ? TODO
    }
  }
}
