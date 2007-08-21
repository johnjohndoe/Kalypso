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
package org.kalypso.ui.wizards.results;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

/**
 * @author Thomas Jung
 * 
 */
public class ResultMetaInfoViewer extends Viewer
{
  private final Group m_panel;

  private Object m_input;

  private Text m_textPanel;

  private final IThemeConstructionFactory m_factory;

  public ResultMetaInfoViewer( final Composite parent, final int style, IThemeConstructionFactory factory )
  {
    m_panel = new Group( parent, style );
    m_panel.setLayout( new GridLayout() );

    m_factory = factory;
  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#getControl()
   */
  @Override
  public Control getControl( )
  {
    return m_panel;
  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#getInput()
   */
  @Override
  public Object getInput( )
  {
    return m_input;
  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#getSelection()
   */
  @Override
  public ISelection getSelection( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#refresh()
   */
  @Override
  public void refresh( )
  {
    /* Empty old stuff */
    final Control[] children = m_panel.getChildren();
    for( Control control : children )
      control.dispose();

    /* fill in new stuff */

    // name & description
    m_textPanel = new Text( m_panel, SWT.WRAP | SWT.READ_ONLY );
    m_textPanel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    if( m_input instanceof IResultMeta )
    {
      // TODO: fill with infos
      final IResultMeta result = (IResultMeta) m_input;

      // special result data
      IResultThemeConstructor createThemeCreator = m_factory.createThemeConstructor( result );
      final Composite buttonControl = createThemeCreator.createControl( m_panel );
      if( buttonControl != null )
        buttonControl.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

      m_panel.setText( result.getName() );
      m_textPanel.setText( result.getDescription() );

    }
    m_panel.layout();
  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#setInput(java.lang.Object)
   */
  @Override
  public void setInput( Object input )
  {
    m_input = input;

    refresh();
  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#setSelection(org.eclipse.jface.viewers.ISelection, boolean)
   */
  @Override
  public void setSelection( ISelection selection, boolean reveal )
  {
    throw new UnsupportedOperationException();
  }

}
