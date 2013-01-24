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
import org.kalypso.model.km.internal.i18n.Messages;

/**
 * @author doemming
 */
public class DirectoryFieldWidget implements ISelectionProvider
{
  private final Text m_text;

  private final boolean m_dirOnly;

  private final List<ISelectionChangedListener> m_listeners = new ArrayList<ISelectionChangedListener>();

  private final Button m_button;

  public DirectoryFieldWidget( final String label, final String toolTip, final boolean dirOnly, final Composite parent, final int sp1, final int sp2, final int sp3 )
  {
    m_dirOnly = dirOnly;
    final Label label1 = new Label( parent, SWT.NONE );
    label1.setText( label );
    label1.setToolTipText( toolTip );
    final GridData data = new GridData();
    data.horizontalSpan = sp1;
    label1.setLayoutData( data );
    m_text = new Text( parent, SWT.BORDER );
    m_text.setToolTipText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.DirectoryFieldWidget.0" ) ); // TODO always show complete path as tooltip //$NON-NLS-1$
    final GridData data2 = new GridData( GridData.FILL_HORIZONTAL );
    data2.horizontalSpan = sp2;
    data2.grabExcessHorizontalSpace = true;
    m_text.setLayoutData( data2 );
    m_text.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusLost( final FocusEvent e )
      {
        fireSelectionChangeEvent();
      }
    } );

    m_button = new Button( parent, SWT.NONE );
    m_button.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.DirectoryFieldWidget.1" ) ); //$NON-NLS-1$
    final GridData data3 = new GridData();
    data3.horizontalSpan = sp3;
    m_button.setLayoutData( data3 );
    m_button.addSelectionListener( new SelectionAdapter()
    {
      private String m_lastPath = null;

      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        if( m_dirOnly )
        {
          final DirectoryDialog dialog = new DirectoryDialog( parent.getShell() );
          if( m_lastPath != null )
            dialog.setFilterPath( m_lastPath );
          // dialog.setText( );
          final String filePath = dialog.open();
          if( filePath != null )
          {
            m_text.setText( filePath );
            m_lastPath = filePath;
            fireSelectionChangeEvent();
          }
        }
        else
        {
          final FileDialog dialog = new FileDialog( parent.getShell() );
          if( m_lastPath != null )
            dialog.setFilterPath( m_lastPath );
          // dialog.setText( "text" );
          final String filePath = dialog.open();
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
    for( final ISelectionChangedListener iSelectionChangedListener : m_listeners )
    {
      iSelectionChangedListener.selectionChanged( event );
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  @Override
  public ISelection getSelection( )
  {
    final String value = m_text.getText();
    return new StructuredSelection( value );
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
        m_text.setText( (String) firstElement ); // fire event ? TODO
    }
  }

  public void setEnabled( final boolean enabled )
  {
    m_button.setEnabled( enabled );
    m_text.setEnabled( enabled );
  }
}
