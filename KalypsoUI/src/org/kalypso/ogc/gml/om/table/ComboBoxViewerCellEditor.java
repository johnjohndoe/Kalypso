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
package org.kalypso.ogc.gml.om.table;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * @author kuch
 */
public class ComboBoxViewerCellEditor extends CellEditor
{
  protected ComboViewer m_viewer;

  public ComboBoxViewerCellEditor( final IContentProvider prContent, final ILabelProvider prLabel, final Object[] input, final Composite parent, final int style )
  {
    super( parent, style );

    setup( prContent, prLabel, input );
  }

  private void setup( final IContentProvider prContent, final ILabelProvider prLabel, final Object[] input )
  {
    m_viewer.setLabelProvider( prLabel );
    m_viewer.setContentProvider( prContent );

    m_viewer.getCombo().addKeyListener( new KeyAdapter()
    {
      // hook key pressed - see PR 14201
      @Override
      public void keyPressed( final KeyEvent e )
      {
        keyReleaseOccured( e );
      }
    } );

    m_viewer.getCombo().addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetDefaultSelected( final SelectionEvent event )
      {

      }

      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        applyEditorValueAndDeactivate();
      }
    } );

    m_viewer.getCombo().addTraverseListener( new TraverseListener()
    {
      public void keyTraversed( final TraverseEvent e )
      {
        if( (e.detail == SWT.TRAVERSE_ESCAPE) || (e.detail == SWT.TRAVERSE_RETURN) )
        {
          e.doit = false;
        }
      }
    } );

    m_viewer.getCombo().addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusLost( final FocusEvent e )
      {
        ComboBoxViewerCellEditor.this.focusLost();
      }
    } );

    m_viewer.setInput( input );
    m_viewer.getCombo().layout();
  }

  /**
   * Applies the currently selected value and deactivates the cell editor
   */
  void applyEditorValueAndDeactivate( )
  {
    fireApplyEditorValue();
    deactivate();
  }

  /**
   * @see org.eclipse.jface.viewers.CellEditor#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createControl( final Composite parent )
  {
    m_viewer = new ComboViewer( parent, getStyle() );

    return m_viewer.getControl();
  }

  /**
   * @see org.eclipse.jface.viewers.CellEditor#doGetValue()
   */
  @Override
  protected Object doGetValue( )
  {
    final ISelection selection = m_viewer.getSelection();

    if( selection instanceof StructuredSelection )
    {
      final StructuredSelection sel = (StructuredSelection) selection;
      final Object element = sel.getFirstElement();

      if( element instanceof String )
      {
        return element;
      }
    }

    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.CellEditor#doSetFocus()
   */
  @Override
  protected void doSetFocus( )
  {
    m_viewer.getCombo().setFocus();
  }

  /**
   * @see org.eclipse.jface.viewers.CellEditor#doSetValue(java.lang.Object)
   */
  @Override
  protected void doSetValue( final Object value )
  {
    if( (value != null) && !"".equals( value ) )
    {
      final StructuredSelection selection = new StructuredSelection( value );
      m_viewer.setSelection( selection );
    }
  }
}
