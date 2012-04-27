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
package org.kalypso.ui.rrm.internal.simulations;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypsodeegree.model.feature.Feature;

/**
 * The simulation calculation feature control.
 * 
 * @author Holger Albert
 */
public class SimulationCalculationFeatureControl extends AbstractFeatureControl
{
  /**
   * The constructor.
   * 
   * @param ftp
   */
  public SimulationCalculationFeatureControl( final IPropertyType ftp )
  {
    super( ftp );
  }

  /**
   * The constructor.
   * 
   * @param feature
   * @param ftp
   */
  public SimulationCalculationFeatureControl( final Feature feature, final IPropertyType ftp )
  {
    super( feature, ftp );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl#createControl(org.eclipse.swt.widgets.Composite,
   *      int)
   */
  @Override
  protected Control createControl( final Composite parent, final int style )
  {
    /* Create the main composite. */
    final Composite main = new Composite( parent, style );
    main.setLayout( new GridLayout( 2, false ) );
    main.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    main.setBackground( parent.getDisplay().getSystemColor( SWT.COLOR_CYAN ) );

    /* Create a label. */
    final Label calculationLabel = new Label( main, SWT.NONE );
    calculationLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    calculationLabel.setText( "Execution status:" );

    /* Create a status composite. */
    final StatusComposite calculationStatusComposite = new StatusComposite( main, SWT.NONE );
    calculationStatusComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    calculationStatusComposite.setStatus( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), "Not verified" ) );

    /* Create a button. */
    final Button calculationButton = new Button( main, SWT.PUSH );
    calculationButton.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );
    calculationButton.setText( "Calculate" );

    // TODO

    /* Create a label. */
    final Label validationLabel = new Label( main, SWT.NONE );
    validationLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    validationLabel.setText( "Validationstatus:" );

    /* Create a status composite. */
    final StatusComposite validationStatusComposite = new StatusComposite( main, SWT.NONE );
    validationStatusComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    validationStatusComposite.setStatus( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), "Not verified" ) );

    // TODO

    return main;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#updateControl()
   */
  @Override
  public void updateControl( )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#isValid()
   */
  @Override
  public boolean isValid( )
  {
    return true;
  }
}