/*--------------- Kalypso-Header --------------------------------------------------------------------

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

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.kalypsomodel1d2d.sim;

import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulationService;
import org.kalypso.simulation.core.KalypsoSimulationCorePlugin;
import org.kalypso.simulation.core.simspec.Modeldata;
import org.kalypso.simulation.ui.calccase.CalcJobHandler;
import org.kalypso.simulation.ui.calccase.ModelNature;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;
import de.renew.workflow.connector.cases.ICaseDataProvider;
import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * TODO: Do notjust copy.paste everything! No one will ever be able to manage this code ever!
 * 
 * Implements the {@link ISimulation} interface to provide the simulation job for the 1d2d model
 * 
 * @author huebsch <a href="mailto:j.huebsch@tuhh.de">Jessica Huebsch</a>
 * @author Patrice Congo
 */
public class CalculationUnitSimMode1D2DCalcJob
{
  public static IStatus startCalculation( final ICalculationUnit calculationUnit, final IWorkbench workbench )
  {
    // TODO: scenarioFolder etc. should come from outside
    final IHandlerService service = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext currentState = service.getCurrentState();
    final Shell shell = (Shell) currentState.getVariable( ISources.ACTIVE_SHELL_NAME );
    final IFolder scenarioFolder = (IFolder) currentState.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
    final ICaseDataProvider<IFeatureWrapper2> caseDataProvider = (ICaseDataProvider<IFeatureWrapper2>) currentState.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );

    if( !MessageDialog.openConfirm( shell, "Berechnung starten", "Die Berechnung wird jetzt gestartet, dies kann sehr lange dauern." ) )
      return Status.CANCEL_STATUS;

    // TODO: do it via the result-model instead
    // TODO: consider restart correctly
    final IFolder unitFolder = scenarioFolder.getFolder( new Path( "results/" + calculationUnit.getGmlID() ) );
    if( unitFolder.exists() )
    {
      if( !MessageDialog.openConfirm( shell, "Berechnung starten", "Es sind bereits Ergebnisdaten einer vorangegangenen Berechnung vorhanden.\nWenn Sie fortfahren werden diese unwiederruflich gelöscht." ) )
        return Status.CANCEL_STATUS;
    }

    final ICoreRunnableWithProgress runnable = new ICoreRunnableWithProgress()
    {
      /**
       * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
       */
      public IStatus execute( final IProgressMonitor monitor ) throws CoreException
      {
        monitor.beginTask( "Modellrechnung wird durchgeführt", 5 );
        IControlModelGroup model = Util.getModel( IControlModelGroup.class );
        final String calcUnitGmlID = calculationUnit.getGmlID();
        // boolean activeSet = false;
        IControlModel1D2D activeControlModel = null;
        for( IControlModel1D2D controlModel : model.getModel1D2DCollection() )
        {
          ICalculationUnit currentCalcUnit = controlModel.getCalculationUnit();
          if( currentCalcUnit != null )
          {
            if( calcUnitGmlID.equals( currentCalcUnit.getGmlID() ) )
            {
              final CommandableWorkspace commandableWorkspace = Util.getCommandableWorkspace( IControlModelGroup.class );
              final Feature feature = model.getModel1D2DCollection().getWrappedFeature();
              final FeatureChange change = new FeatureChange( feature, feature.getFeatureType().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_XP_ACTIVE_MODEL ), controlModel.getGmlID() );
              final ChangeFeaturesCommand command = new ChangeFeaturesCommand( feature.getWorkspace(), new FeatureChange[] { change } );
              try
              {
                commandableWorkspace.postCommand( command );
              }
              catch( Exception e )
              {
                e.printStackTrace();
                return StatusUtilities.createErrorStatus( "Could not set active control model for: " + calculationUnit.getGmlID() );
              }
              activeControlModel = controlModel;

              caseDataProvider.saveModel( null );
            }
          }
        }
        if( activeControlModel == null )
        {
          return StatusUtilities.createErrorStatus( "Could not found active control model for: " + calculationUnit.getGmlID() );
        }

        try
        {
          final Modeldata modelspec = loadModelspec( scenarioFolder.getProject() );

          final String typeID = modelspec.getTypeID();

          final ISimulationService calcService = KalypsoSimulationCorePlugin.findCalculationServiceForType( typeID );

          monitor.worked( 1 );
          final CalcJobHandler cjHandler = new CalcJobHandler( modelspec, calcService );
          final IStatus simulationResult = cjHandler.runJob( scenarioFolder, new SubProgressMonitor( monitor, 4 ) );

          final IScenarioResultMeta scenarioResultMeta = caseDataProvider.getModel( IScenarioResultMeta.class );
          fillResultModel( unitFolder, calculationUnit, scenarioResultMeta );
          caseDataProvider.saveModel( IScenarioResultMeta.class, null );

          return simulationResult;
        }
        finally
        {
          monitor.done();
        }
      }
    };

    return RunnableContextHelper.execute( workbench.getProgressService(), true, false, runnable );
  }

  protected static void fillResultModel( final IFolder unitFolder, ICalculationUnit calculationUnit, IScenarioResultMeta scenarioResultMeta )
  {
    try
    {
      final IFile metaFile = unitFolder.getFile( "resultMeta.gml" );
      final URL metaURL = ResourceUtilities.createURL( metaFile );
      final GMLWorkspace metaWorkspace = GmlSerializer.createGMLWorkspace( metaURL, null );
      final Feature rootFeature = metaWorkspace.getRootFeature();
      final ICalcUnitResultMeta newCalcunitResultMeta = (ICalcUnitResultMeta) rootFeature.getAdapter( ICalcUnitResultMeta.class );

      scenarioResultMeta.importCalculationUnit( newCalcunitResultMeta );
    }
    catch( MalformedURLException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

  }

  // TODO: load modelspec from binaries, not from the project
  private static Modeldata loadModelspec( final IProject project ) throws CoreException
  {
    try
    {
      final IFolder modelFolder = project.getFolder( ModelNature.MODELLTYP_FOLDER );
      final IFile file = modelFolder.getFile( ModelNature.MODELLTYP_MODELSPEC_XML );
      if( !file.exists() )
        return null;

      final Unmarshaller unmarshaller = ModelNature.JC_SPEC.createUnmarshaller();

      final Modeldata modelData = (Modeldata) unmarshaller.unmarshal( file.getContents() );
      return modelData;

    }
    catch( final JAXBException e )
    {
      e.printStackTrace();

      throw new CoreException( StatusUtilities.statusFromThrowable( e, "Fehler beim Laden der Modell-Spezifikation" ) );
    }
  }

}