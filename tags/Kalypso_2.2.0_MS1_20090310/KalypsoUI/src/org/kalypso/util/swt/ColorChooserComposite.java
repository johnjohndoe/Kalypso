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
package org.kalypso.util.swt;

import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * @author Dirk Kuch
 */
public class ColorChooserComposite extends Composite
{
  protected RGB m_color;

  private final String m_label;

  ColorRegistry m_colorRegister = new ColorRegistry();

  private Composite m_body = null;

  public ColorChooserComposite( final Composite parent, final String label, final RGB color )
  {
    super( parent, SWT.NULL );
    m_label = label;
    m_color = color;

    m_colorRegister.put( color.toString(), color );

    this.setLayout( new GridLayout() );

    update();
  }

  /**
   * @see org.eclipse.swt.widgets.Control#update()
   */
  @Override
  public void update( )
  {
    if( this.isDisposed() )
      return;

    if( m_body != null )
    {
      if( !m_body.isDisposed() )
        m_body.dispose();

      m_body = null;
    }

    m_body = new Composite( this, SWT.NULL );
    m_body.setLayout( new GridLayout( 3, false ) );
    m_body.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    /* color */
    final Label lColor = new Label( m_body, SWT.BORDER );
    final GridData gridData = new GridData( GridData.FILL, GridData.FILL, false, false );
    gridData.widthHint = gridData.heightHint = 16;
    lColor.setLayoutData( gridData );

    Color color = m_colorRegister.get( m_color.toString() );
    if( color == null )
    {
      color = new Color( null, m_color );
      m_colorRegister.put( m_color.toString(), m_color );
    }

    lColor.setBackground( color );

    lColor.addMouseListener( new MouseAdapter()
    {
      /**
       * @see org.eclipse.swt.events.MouseAdapter#mouseUp(org.eclipse.swt.events.MouseEvent)
       */
      @Override
      public void mouseUp( final MouseEvent e )
      {
        if( e.button == 1 )// left button
        {
          changeColor( lColor.getShell(), lColor );
        }

      }
    } );

    /* label */
    final Label label = new Label( m_body, SWT.NULL );
    label.setText( m_label );
    label.setLayoutData( new GridData( GridData.FILL, GridData.CENTER, true, false ) );

    /* color chooser */
    final Button bColor = new Button( m_body, SWT.NONE );
    bColor.setText( "..." );

    bColor.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        changeColor( bColor.getShell(), lColor );
      }
    } );

    super.update();
  }

  public RGB getColor( )
  {
    return m_color;
  }

  protected void changeColor( final Shell shell, final Label colorLabel )
  {
    final ColorDialog colorDialog = new ColorDialog( shell );
    colorDialog.setRGB( m_color );

    final RGB rgb = colorDialog.open();
    if( rgb != null )
    {
      m_color = rgb;

      Color color = m_colorRegister.get( m_color.toString() );
      if( color == null )
      {
        color = new Color( null, m_color );
        m_colorRegister.put( m_color.toString(), m_color );
      }

      colorLabel.setBackground( color );
    }
  }

}
