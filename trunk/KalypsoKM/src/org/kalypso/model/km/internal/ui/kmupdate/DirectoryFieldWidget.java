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
 * @author Andreas Doemming (original)
 * @author Holger Albert (modified)
 */
public class DirectoryFieldWidget implements ISelectionProvider
{
  /**
   * The listeners.
   */
  private List<ISelectionChangedListener> m_listeners;

  /**
   * A directory should be selected.
   */
  protected boolean m_dirOnly;

  /**
   * The text field.
   */
  protected Text m_text;

  /**
   * The button.
   */
  private Button m_button;

  /**
   * The constructor.
   * 
   * @param dirOnly
   *          A directory should be selected.
   * @param parent
   *          The parent composite.
   * @param label
   *          The text of the label, which is placed before the text field.
   * @param tooltip
   *          The tooltip of the label, which is placed before the text field.
   * @param sp1
   *          The control consists of 3 columns. This is the colspan of the first one. Check your your layout. It must
   *          have at least 3 columns and the number of columns must match the sum of sp1, sp2 and sp3.
   * @param sp2
   *          The control consists of 3 columns. This is the colspan of the second one. Check your your layout. It must
   *          have at least 3 columns and the number of columns must match the sum of sp1, sp2 and sp3.
   * @param sp3
   *          The control consists of 3 columns. This is the colspan of the third one. Check your your layout. It must
   *          have at least 3 columns and the number of columns must match the sum of sp1, sp2 and sp3.
   */
  public DirectoryFieldWidget( boolean dirOnly, Composite parent, String label, String tooltip, int sp1, int sp2, int sp3 )
  {
    m_listeners = new ArrayList<ISelectionChangedListener>();
    m_dirOnly = dirOnly;
    m_text = null;
    m_button = null;

    createControls( parent, label, tooltip, sp1, sp2, sp3 );
  }

  /**
   * This function creates the controls.
   * 
   * @param parent
   *          The parent composite.
   * @param label
   *          The text of the label, which is placed before the text field.
   * @param tooltip
   *          The tooltip of the label, which is placed before the text field.
   * @param sp1
   *          The control consists of 3 columns. This is the colspan of the first one. Check your your layout. It must
   *          have at least 3 columns and the number of columns must match the sum of sp1, sp2 and sp3.
   * @param sp2
   *          The control consists of 3 columns. This is the colspan of the second one. Check your your layout. It must
   *          have at least 3 columns and the number of columns must match the sum of sp1, sp2 and sp3.
   * @param sp3
   *          The control consists of 3 columns. This is the colspan of the third one. Check your your layout. It must
   *          have at least 3 columns and the number of columns must match the sum of sp1, sp2 and sp3.
   */
  private void createControls( final Composite parent, String label, String tooltip, int sp1, int sp2, int sp3 )
  {
    /* Create a label. */
    Label label1 = new Label( parent, SWT.NONE );
    label1.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false, sp1, 1 ) );
    label1.setText( label );
    label1.setToolTipText( tooltip );

    /* Create a text. */
    m_text = new Text( parent, SWT.BORDER );
    m_text.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, sp2, 1 ) );
    m_text.setToolTipText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.DirectoryFieldWidget.0" ) ); //$NON-NLS-1$

    /* Add a listener. */
    m_text.addFocusListener( new FocusAdapter()
    {
      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {
        fireSelectionChangeEvent();
      }
    } );

    /* Create a button. */
    m_button = new Button( parent, SWT.NONE );
    m_button.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.DirectoryFieldWidget.1" ) ); //$NON-NLS-1$
    m_button.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false, sp3, 1 ) );
    m_button.addSelectionListener( new SelectionAdapter()
    {
      /**
       * The last selected path.
       */
      private String m_lastPath = null;

      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        if( m_dirOnly )
        {
          DirectoryDialog dialog = new DirectoryDialog( parent.getShell() );
          if( m_lastPath != null )
            dialog.setFilterPath( m_lastPath );

          String filePath = dialog.open();
          if( filePath != null )
          {
            m_text.setText( filePath );
            m_lastPath = filePath;
            fireSelectionChangeEvent();
          }

          return;
        }

        FileDialog dialog = new FileDialog( parent.getShell() );
        if( m_lastPath != null )
          dialog.setFilterPath( m_lastPath );

        String filePath = dialog.open();
        m_text.setText( filePath );
        m_lastPath = filePath;
        fireSelectionChangeEvent();
      }
    } );
  }

  /**
   * This function fires a selection changed event.
   */
  protected void fireSelectionChangeEvent( )
  {
    SelectionChangedEvent event = new SelectionChangedEvent( this, getSelection() );
    for( ISelectionChangedListener listener : m_listeners )
      listener.selectionChanged( event );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  @Override
  public ISelection getSelection( )
  {
    return new StructuredSelection( m_text.getText() );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  @Override
  public void addSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_listeners.add( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  @Override
  public void removeSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_listeners.remove( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void setSelection( ISelection selection )
  {
    if( selection.isEmpty() )
      m_text.setText( "" ); //$NON-NLS-1$

    if( selection instanceof IStructuredSelection )
    {
      Object firstElement = ((IStructuredSelection) selection).getFirstElement();
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
  public void setEnabled( boolean enabled )
  {
    m_button.setEnabled( enabled );
    m_text.setEnabled( enabled );
  }
}