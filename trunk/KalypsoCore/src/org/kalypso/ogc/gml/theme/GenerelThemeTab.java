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
package org.kalypso.ogc.gml.theme;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoThemeListener;
import org.kalypso.ogc.gml.KalypsoThemeEvent;

/**
 * @author Gernot Belger
 */
public class GenerelThemeTab extends Composite implements IKalypsoThemeListener
{
  private final IKalypsoTheme m_theme;

  private Text m_text;

  public GenerelThemeTab( final TabFolder parent, final int style, final IKalypsoTheme theme )
  {
    super( parent, style );

    m_theme = theme;

    m_theme.addKalypsoThemeListener( this );

    setLayout( new GridLayout( 2, false ) );

    /* Name */
    final Label nameLabel = new Label( this, SWT.NONE );
    nameLabel.setText( "Name: " );
    final Text text = new Text( this, SWT.NONE );
    m_text = text;
    m_text.setText( m_theme.getName() );
    m_text.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        theme.setName( text.getText() );
      }
    } );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoThemeListener#kalypsoThemeChanged(org.kalypso.ogc.gml.KalypsoThemeEvent)
   */
  public void kalypsoThemeChanged( final KalypsoThemeEvent event )
  {
    m_text.setText( event.getSource().getName() );
  }

}
