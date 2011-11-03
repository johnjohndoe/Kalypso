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
package org.kalypso.kalypsomodel1d2d.sim;

import java.text.DateFormat;
import java.util.Date;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.forms.widgets.FormText;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * @author Thomas Jung
 * 
 */
public class ResultInfoViewer extends Viewer
{

  /*
   * fonts
   */
  private final Font fTextHeader = new Font( Display.getDefault(), "Tahoma", 10, SWT.BOLD ); //$NON-NLS-1$

  private final Font fTextNormal = new Font( Display.getDefault(), "Tahoma", 8, SWT.NONE ); //$NON-NLS-1$

  private final Group m_panel;

  private Object m_input;

  private FormText m_textPanel;

  public ResultInfoViewer( final Composite parent, final int style )
  {
    m_panel = new Group( parent, style );
    m_panel.setLayout( new GridLayout() );
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
    for( final Control control : children )
      control.dispose();

    m_textPanel = new FormText( m_panel, SWT.WRAP | SWT.READ_ONLY );
    m_textPanel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    m_panel.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.sim.ResultInfoViewer.2") ); //$NON-NLS-1$
    m_textPanel.setFont( "header", fTextHeader ); //$NON-NLS-1$
    m_textPanel.setFont( "text", fTextNormal ); //$NON-NLS-1$

    if( m_input instanceof Date )
    {
      final Date result = (Date) m_input;

      final String infoText = getInformationText( result );
      m_textPanel.setText( infoText, true, false );
    }
    m_panel.layout( true );
  }

  public static String getInformationText( final Date resultDate )
  {
    if( resultDate == null )
      return Messages.getString("org.kalypso.kalypsomodel1d2d.sim.ResultInfoViewer.5"); //$NON-NLS-1$

    final StringBuffer buf = new StringBuffer();

    final DateFormat dateFormat = DateFormat.getDateTimeInstance( DateFormat.SHORT, DateFormat.LONG );
    dateFormat.setTimeZone( KalypsoGisPlugin.getDefault().getDisplayTimeZone() );

    /* make string buffer */

    buf.append( "<form>" ); //$NON-NLS-1$
    buf.append( "<p>" ); //$NON-NLS-1$
    buf.append( Messages.getString("org.kalypso.kalypsomodel1d2d.sim.ResultInfoViewer.0") + dateFormat.format( resultDate ) + "</b>" ); //$NON-NLS-1$ //$NON-NLS-2$
    buf.append( "</p>" ); //$NON-NLS-1$
    buf.append( "</form>" ); //$NON-NLS-1$

    return buf.toString();
  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#setInput(java.lang.Object)
   */
  @Override
  public void setInput( final Object input )
  {
    m_input = input;

    refresh();
  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#setSelection(org.eclipse.jface.viewers.ISelection, boolean)
   */
  @Override
  public void setSelection( final ISelection selection, final boolean reveal )
  {
    throw new UnsupportedOperationException();
  }

}
