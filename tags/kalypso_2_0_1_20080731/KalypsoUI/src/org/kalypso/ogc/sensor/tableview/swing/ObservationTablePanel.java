/*--------------- Kalypso-Header ------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 --------------------------------------------------------------------------*/

package org.kalypso.ogc.sensor.tableview.swing;

import java.awt.Color;
import java.awt.Dimension;

import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingConstants;

/**
 * @author schlienger
 */
public class ObservationTablePanel extends JPanel
{
  private final ObservationTable m_table;

  /** is created if an observation has a scenario property */
  private final JLabel m_label = new JLabel("", SwingConstants.CENTER ); //$NON-NLS-1$

  public ObservationTablePanel( final ObservationTable table )
  {
    m_table = table;

    //setLayout( new GridLayout( 0, 1 ) );
    setLayout( new BoxLayout( this, BoxLayout.Y_AXIS ) );

    add( m_label );
    add( new JScrollPane( m_table ) );

    table.setPanel( this );
    m_label.setVisible( false );
  }

  public void clearLabel()
  {
    m_label.setText( "" ); //$NON-NLS-1$
    m_label.setVisible( false );
  }

  public boolean isLabelSet()
  {
    return m_label.isVisible();
  }

  public void setLabel( final String txt, final Icon icon, final Color color, final Integer height,
      final Boolean showTxt )
  {
    final boolean bShowText;
    if( showTxt != null )
      bShowText = showTxt.booleanValue();
    else
      bShowText = true;
      
    if( bShowText )
      m_label.setText( txt );
    else
      m_label.setText( "" ); //$NON-NLS-1$
    m_label.setIcon( icon );
    setBackground( color );
    
    if( height != null )
      m_label.setPreferredSize( new Dimension( m_label.getWidth(), height.intValue() ) );

    m_label.setVisible( true );
    doLayout();
  }
}
