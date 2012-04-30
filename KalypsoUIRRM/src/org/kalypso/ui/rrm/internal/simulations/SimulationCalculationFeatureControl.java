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

import java.io.File;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.model.hydrology.project.RrmSimulation;
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

    try
    {
      /* Create a label. */
      final Label calculationLabel = new Label( main, SWT.NONE );
      calculationLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
      calculationLabel.setText( "Calculation status:" );

      /* Create a status composite. */
      final StatusComposite calculationStatusComposite = new StatusComposite( main, SWT.NONE );
      calculationStatusComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
      calculationStatusComposite.setStatus( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), "Please wait while updating..." ) );

      /* Create a button. */
      final Button calculationButton = new Button( main, SWT.PUSH );
      calculationButton.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );
      calculationButton.setText( "Calculate" );

      /* Create a label. */
      final Label validationLabel = new Label( main, SWT.NONE );
      validationLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
      validationLabel.setText( "Validation status:" );

      /* Create a status composite. */
      final StatusComposite validationStatusComposite = new StatusComposite( main, SWT.NONE );
      validationStatusComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
      validationStatusComposite.setStatus( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), "Please wait while updating..." ) );

      /* Empty label. */
      final Label emptyLabel = new Label( main, SWT.NONE );
      emptyLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );

      /* Get the current simulation. */
      final RrmSimulation simulation = getSimulation();

      /* Get the log file. */
      final IFolder logsFolder = simulation.getLogsResultsFolder();
      final IFile logTxt = logsFolder.getFile( "error.txt" );
      final File logFile = logTxt.getLocation().toFile();

      /* Create the actions. */
      final OpenTextLogAction openTextLogAction = new OpenTextLogAction( "Calculation log", "Displays the calculation log file.", logFile );

      /* Create a image hyperlink. */
      final ImageHyperlink imageHyperlink = ActionHyperlink.createHyperlink( null, main, SWT.NONE, openTextLogAction );
      imageHyperlink.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
      imageHyperlink.setText( openTextLogAction.getText() );

      /* Initialize. */
      initialize();
    }
    catch( final CoreException ex )
    {
      ex.printStackTrace();

      final Label emptyLabel = new Label( main, SWT.WRAP );
      emptyLabel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 ) );
      emptyLabel.setText( ex.getLocalizedMessage() );
    }

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

  private RrmSimulation getSimulation( ) throws CoreException
  {
    /* Get the description of the current simulation. */
    final Feature feature = getFeature();
    final String description = feature.getDescription();

    /* Get the folder of the current simulation. */
    final SzenarioDataProvider dataProvider = ScenarioHelper.getScenarioDataProvider();
    final IContainer scenarioFolder = dataProvider.getScenarioFolder();
    final IFolder calcCasesFolder = scenarioFolder.getFolder( new Path( INaProjectConstants.FOLDER_RECHENVARIANTEN ) );
    final IFolder simulationFolder = calcCasesFolder.getFolder( description );

    return new RrmSimulation( simulationFolder );
  }

  private void initialize( )
  {
    // TODO Read/Create the stati with two non UI jobs...
  }

  private IStatus getCalculationStatus( )
  {
    // TODO
    return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), "The calculation was executed." );
  }

  private IStatus getValidationStatus( )
  {
    try
    {
      final NAControl feature = (NAControl) getFeature();
      final boolean outdated = feature.isOutdated();
      if( outdated )
        return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), "The results are outdated." );

      return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), "The results are uptodate." );
    }
    catch( final Exception ex )
    {
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( "Validation has failed: %s", ex.getLocalizedMessage() ), ex );
    }
  }
}