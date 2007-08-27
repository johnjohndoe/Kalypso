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

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.kalypsodeegree.graphics.sld.LineColorMapEntry;
import org.kalypsodeegree_impl.graphics.sld.LineColorMap;

/**
 * @author Thomas Jung
 * 
 */
public class LineColorMapEditorComposite extends Composite
{

  private final LineColorMap m_colorMap;

  private LineColorMapEntry m_toEntry;

  private LineColorMapEntry m_fromEntry;

  private double m_stepWidth;

  private double m_minValue;

  private double m_maxValue;

  private double m_fatValue;

  private double m_fatWidth;

  public LineColorMapEditorComposite( final Composite parent, final int style, final LineColorMap colorMap )
  {

    super( parent, style );
    m_colorMap = colorMap;

    createControl();

  }

  private void createControl( )
  {
    setLayout( new GridLayout( 2, true ) );

    Group normalColorMapGroup = new Group( this, SWT.NONE );
    normalColorMapGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    normalColorMapGroup.setLayout( new GridLayout( 1, true ) );
    normalColorMapGroup.setText( "Liniengrundeinstellungen" );

    m_fromEntry = m_colorMap.getColorMap()[0];
    final LineColorMapEntryEditorComposite fromEntryComposite = new LineColorMapEntryEditorComposite( normalColorMapGroup, SWT.NONE, m_fromEntry );
    fromEntryComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    Group propertyGroup = new Group( this, SWT.NONE );
    propertyGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    propertyGroup.setLayout( new GridLayout( 1, true ) );
    propertyGroup.setText( "erweiterte Einstellungen" );

    // step width spinner
    final Spinner stepWidthSpinner = new Spinner( propertyGroup, SWT.NONE );
    stepWidthSpinner.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Label labelWithSpinner = new Label( propertyGroup, SWT.NONE );
    labelWithSpinner.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    labelWithSpinner.setText( "Intervallbreite" );

    // fat step spinner
    final Spinner fatStepSpinner = new Spinner( propertyGroup, SWT.NONE );
    fatStepSpinner.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Label labelFatStepSpinner = new Label( propertyGroup, SWT.NONE );
    labelFatStepSpinner.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    labelFatStepSpinner.setText( "te Linie dick zeichnen" );

    // bold width spinner
    final Spinner boldWidthSpinner = new Spinner( propertyGroup, SWT.NONE );
    boldWidthSpinner.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Label labelboldWidthSpinner = new Label( propertyGroup, SWT.NONE );
    labelboldWidthSpinner.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    labelboldWidthSpinner.setText( "Linenbreite (dick)" );

  }

}
