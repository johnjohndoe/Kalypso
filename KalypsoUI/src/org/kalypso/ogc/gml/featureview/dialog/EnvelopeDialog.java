/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ogc.gml.featureview.dialog;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.swt.events.DoubleModifyListener;

/**
 * This class builds the dialog for the data input for the GM_Envelope.
 * 
 * @author albert
 */
public class EnvelopeDialog extends Dialog
{
  private Double m_values[];

  public EnvelopeDialog( Shell parent, Double[] values )
  {
    super( parent );

    m_values = new Double[] { values[0], values[1], values[2], values[3] };
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( final Composite parent )
  {
    /* Create a own composite for placing controls. */
    final Composite panel = (Composite) super.createDialogArea( parent );

    /* Configuring the composite. */
    final Shell shell = panel.getShell();
    shell.setText( "Envelope-Daten" );

    /* The label for the input data. */
    Label label = new Label( panel, SWT.NONE );
    label.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    label.setText( "Füllen Sie hier die Rechts- und Hochwerte aus." );
    label.setAlignment( SWT.LEFT );

    /* A new group for the labels and texts. */
    final Group group = new Group( panel, SWT.NONE );
    group.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Configuring the group. */
    group.setLayout( new GridLayout( 2, true ) );

    /* The label for the input data. */
    Label label1 = new Label( group, SWT.NONE );
    label1.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    label1.setText( "Rechtswert MIN" );
    label1.setAlignment( SWT.LEFT );

    /* The text for the input data. */
    Text text1 = new Text( group, SWT.BORDER );
    text1.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    text1.setText( m_values[0].toString() );

    /* The label for the input data. */
    Label label2 = new Label( group, SWT.NONE );
    label2.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    label2.setText( "Rechtswert MAX" );
    label2.setAlignment( SWT.LEFT );

    /* The text for the input data. */
    Text text2 = new Text( group, SWT.BORDER );
    text2.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    text2.setText( m_values[1].toString() );

    /* The label for the input data. */
    Label label3 = new Label( group, SWT.NONE );
    label3.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    label3.setText( "Hochwert MIN" );
    label3.setAlignment( SWT.LEFT );

    /* The text for the input data. */
    Text text3 = new Text( group, SWT.BORDER );
    text3.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    text3.setText( m_values[2].toString() );

    /* The label for the input data. */
    Label label4 = new Label( group, SWT.NONE );
    label4.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    label4.setText( "Hochwert MAX" );
    label4.setAlignment( SWT.LEFT );

    /* The text for the input data. */
    Text text4 = new Text( group, SWT.BORDER );
    text4.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    text4.setText( m_values[3].toString() );

    text1.addModifyListener( new EnvelopeDialogListener( this, 0 ) );
    text2.addModifyListener( new EnvelopeDialogListener( this, 1 ) );
    text3.addModifyListener( new EnvelopeDialogListener( this, 2 ) );
    text4.addModifyListener( new EnvelopeDialogListener( this, 3 ) );

    final Display display = shell.getDisplay();
    final Color badColor = display.getSystemColor( SWT.COLOR_RED );
    final Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );

    final DoubleModifyListener doubleModifyListener = new DoubleModifyListener( goodColor, badColor );

    text1.addModifyListener( doubleModifyListener );
    text2.addModifyListener( doubleModifyListener );
    text3.addModifyListener( doubleModifyListener );
    text4.addModifyListener( doubleModifyListener );

    return panel;
  }

  public Double[] getValues( )
  {
    return m_values;
  }

  public void setValues( Double[] values )
  {
    m_values = values;
  }
}
