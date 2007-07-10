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

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ResultDB;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.simulation.core.ISimulationService;
import org.kalypso.simulation.core.KalypsoSimulationCorePlugin;
import org.kalypso.simulation.core.simspec.Modeldata;
import org.kalypso.simulation.ui.calccase.CalcJobHandler;
import org.kalypso.simulation.ui.calccase.ModelNature;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;

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
  public static IStatus startCalculation( final ICalculationUnit calUnit, final IWorkbench workbench )
  {
    final ICoreRunnableWithProgress runnable = new ICoreRunnableWithProgress()
    {
      /**
       * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
       */
      public IStatus execute( final IProgressMonitor monitor ) throws CoreException
      {
        monitor.beginTask( "Modellrechnung wird durchgeführt", 5 );
        IControlModelGroup model = Util.getModel( IControlModelGroup.class );
        final String calcUnitGmlID = calUnit.getGmlID();
        // boolean activeSet = false;
        IControlModel1D2D activeControlModel = null;
        for( IControlModel1D2D cm : model.getModel1D2DCollection() )
        {
          ICalculationUnit current = cm.getCalculationUnit();
          if( current != null )
          {
            if( calcUnitGmlID.equals( current.getGmlID() ) )
            {
              activeControlModel = cm;
              model.getModel1D2DCollection().setActiveControlModel( cm );
              Util.saveAllModel( workbench, workbench.getActiveWorkbenchWindow() );
              // activeSet = true;
            }
          }
        }
        if( activeControlModel == null )
        {
          return StatusUtilities.createErrorStatus( "Could not found and set active control model for: " + calUnit.getGmlID() );
        }

        // adds scenarion model to the result db
        ResultDB resultDB = KalypsoModel1D2DPlugin.getDefault().getResultDB();
        resultDB.addModelDescriptor( calUnit );
        resultDB.addModelDescriptor( activeControlModel );

        // TODO: scenarioFolder etc. should come from outside
        final IHandlerService service = (IHandlerService) workbench.getService( IHandlerService.class );
        final IEvaluationContext currentState = service.getCurrentState();
        final IFolder scenarioFolder = (IFolder) currentState.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );

        try
        {
          final Modeldata modelspec = loadModelspec( scenarioFolder.getProject() );

          final String typeID = modelspec.getTypeID();

          final ISimulationService calcService = KalypsoSimulationCorePlugin.findCalculationServiceForType( typeID );

          monitor.worked( 1 );
          final CalcJobHandler cjHandler = new CalcJobHandler( modelspec, calcService );
          return cjHandler.runJob( scenarioFolder, new SubProgressMonitor( monitor, 4 ) );
        }
        finally
        {
          monitor.done();
        }
      }
    };

    return RunnableContextHelper.execute( workbench.getProgressService(), true, false, runnable );
  }

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