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

import java.util.Date;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.model.Util;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.ControlModel1D2DCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2DCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.geolog.GeoLog;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * Starting point for running 1d2d simulations.
 * 
 * @author Gernot Belger
 */
public class Model1D2DSimulation implements ISimulation1D2DConstants
{
  /*
   * this is a duplicate of the constant in RMA10CalculationWizard
   */
  private static final String STRING_DLG_TITLE_RMA10S = RMA10CalculationWizard.STRING_DLG_TITLE_RMA10S;

  private final Shell m_shell;

  public Model1D2DSimulation( final Shell shell )
  {
    m_shell = shell;
  }

  /**
   * Starting point of calculation
   * <ul>
   * <li>create temporary directory and make sure it gets deleted after calculation</li>
   * <li>create output directory and make sure it gets synchronized with workspace after calculation</li>
   * </ul>
   */
  public void process( final ICalculationUnit calculationUnit )
  {
    final IGeoLog geoLog = initGeoLog();
    final Date startTime = geoLog.getStartTime();
    final String calcUnitId = calculationUnit.getId();

    final IScenarioDataProvider caseDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();

    try
    {
      /* Initialize result meta */
      initResultMeta( calculationUnit, startTime, caseDataProvider );

      /* Set correct activeControlModel according to selected calcUnit and save */
      final IControlModelGroup controlModelGroup = caseDataProvider.getModel( IControlModelGroup.class.getName() );
      setActiveControlModel( controlModelGroup, calcUnitId );
      // Saves ALL models, this is not really necessary but not really a problem, as only
      // dirty models get saved (and probably here only the control model is dirty).
      caseDataProvider.saveModel( null );

      final RMA10CalculationWizard calcWizard = new RMA10CalculationWizard( caseDataProvider, geoLog );
      final WizardDialog2 calcDialog = new WizardDialog2( m_shell, calcWizard );
      calcDialog.setRememberSize( true );
      if( calcDialog.open() == Window.OK )
      {
        // save log

        // move results and save them

        // TODO: maybe just save log and result stuff depending on wizard result?
        // PROBLEM: if the user cancels during result processing, maybe old result already have been deleted...
      }
    }
    catch( final CoreException e )
    {
      // REMARK: this should only happen if the data cannot be retrieved from the caseDataProvider and so should never
      // happen...
      MessageDialog.openError( m_shell, STRING_DLG_TITLE_RMA10S, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.Model1D2DSimulation.5" ) ); //$NON-NLS-1$
    }
  }

  private void initResultMeta( final ICalculationUnit calculationUnit, final Date startTime, final IScenarioDataProvider caseDataProvider ) throws CoreException
  {
    final IScenarioResultMeta scenarioResultMeta = caseDataProvider.getModel( IScenarioResultMeta.class.getName() );
    final ICalcUnitResultMeta existingCalcUnitMeta = scenarioResultMeta.findCalcUnitMetaResult( calculationUnit.getId() );
    final ICalcUnitResultMeta calcUnitMeta;
    if( existingCalcUnitMeta == null )
      calcUnitMeta = scenarioResultMeta.getChildren().addNew( ICalcUnitResultMeta.QNAME, ICalcUnitResultMeta.class );
    else
      calcUnitMeta = existingCalcUnitMeta;

    calcUnitMeta.setCalcStartTime( startTime );
    calcUnitMeta.setCalcUnit( calculationUnit.getId() );
    calcUnitMeta.setName( calculationUnit.getName() );
    calcUnitMeta.setDescription( calculationUnit.getDescription() );
    calcUnitMeta.setPath( new Path( calculationUnit.getId() ) );
    calcUnitMeta.setCalcEndTime( new Date() );

    // Add geo log to calcMeta as document
    final IDocumentResultMeta logMeta = calcUnitMeta.getChildren().addNew( IDocumentResultMeta.QNAME, IDocumentResultMeta.class );
    logMeta.setName( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.Model1D2DSimulation.7" ) ); //$NON-NLS-1$
    logMeta.setDescription( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.Model1D2DSimulation.8" ) ); //$NON-NLS-1$
    logMeta.setDocumentType( IDocumentResultMeta.DOCUMENTTYPE.log );
    logMeta.setPath( new Path( SIMULATION_LOG_GML ) );
  }

  private IGeoLog initGeoLog( )
  {
    // First level of calculation: initialize logging, then call real calculation stuff
    IGeoLog geoLog = null;
    try
    {
      /* Initialize the logging facilities */
      final ILog model1d2dlog = KalypsoModel1D2DPlugin.getDefault().getLog();
      // Additionally, everything is logged into the log of the KalypsoModel1D2D-Plug-In
      geoLog = new GeoLog( model1d2dlog );
    }
    catch( final GMLSchemaException e )
    {
      MessageDialog.openError( m_shell, STRING_DLG_TITLE_RMA10S, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.Model1D2DSimulation.0" ) + e.toString() ); //$NON-NLS-1$
    }
    return geoLog;
  }

  /**
   * Sets the unit to calculate into the Control-Model as active unit.<br>
   * TODO: this is a bit fishy... Better would be to couple calc-units and its corresponding control-models more
   * closely. Then no search for and/or setting of active unit should be necessary any more.
   */
  private void setActiveControlModel( final IControlModelGroup controlModelGroup, final String calcUnitId ) throws CoreException
  {
    final IControlModel1D2D controlModel = findControlModel( controlModelGroup, calcUnitId );

    final IControlModel1D2DCollection feature = controlModelGroup.getModel1D2DCollection();
    final FeatureChange change = new FeatureChange( feature, feature.getFeatureType().getProperty( ControlModel1D2DCollection.WB1D2DCONTROL_XP_ACTIVE_MODEL ), controlModel.getId() );
    final ChangeFeaturesCommand command = new ChangeFeaturesCommand( feature.getWorkspace(), new FeatureChange[] { change } );
    try
    {
      final CommandableWorkspace commandableWorkspace = Util.getCommandableWorkspace( IControlModelGroup.class );
      commandableWorkspace.postCommand( command );
    }
    catch( final Throwable e )
    {
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.Model1D2DSimulation.11" ) + calcUnitId ) ); //$NON-NLS-1$
    }
  }

  protected static IControlModel1D2D findControlModel( final IControlModelGroup controlModelGroup, final String calcUnitId )
  {
    final IControlModel1D2DCollection model1d2dCollection = controlModelGroup.getModel1D2DCollection();
    for( final IControlModel1D2D controlModel : model1d2dCollection.getControlModels() )
    {
      final ICalculationUnit currentCalcUnit = controlModel.getCalculationUnit();
      if( currentCalcUnit != null )
      {
        if( calcUnitId.equals( currentCalcUnit.getId() ) )
        {
          return controlModel;
        }
      }
    }
    return null;
  }

}
