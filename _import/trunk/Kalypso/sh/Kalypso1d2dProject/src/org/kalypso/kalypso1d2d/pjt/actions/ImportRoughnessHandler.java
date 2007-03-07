package org.kalypso.kalypso1d2d.pjt.actions;

import java.util.HashMap;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.wizards.IWizardDescriptor;
import org.kalypso.kalypso1d2d.pjt.SzenarioSourceProvider;
import org.kalypso.kalypso1d2d.pjt.views.ISzenarioDataProvider;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.ui.wizards.imports.INewWizardKalypsoImport;

import de.renew.workflow.WorkflowCommandHandler;

/**
 * Starts the import roughness wizard
 * 
 * @author Stefan Kurzbach
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportRoughnessHandler extends WorkflowCommandHandler
{
  private static final String ROUGHNESS_IMPORT_WIZARD_ID = "org.kalypso.ui.wizards.imports.roughness.ImportWizard";

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.WorkflowCommandHandler#executeInternal(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  protected IStatus executeInternal( final ExecutionEvent event ) throws CoreException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    IStructuredSelection selection = (IStructuredSelection) context.getVariable( ISources.ACTIVE_CURRENT_SELECTION_NAME );
    if( selection == null )
    {
      final IResource currentFolder = (IFolder) context.getVariable( SzenarioSourceProvider.ACTIVE_SZENARIO_FOLDER_NAME );
      selection = new StructuredSelection( currentFolder );
    }

    final IWorkbenchWindow workbenchWindow = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final IWorkbench workbench = (workbenchWindow).getWorkbench();

    final ISzenarioDataProvider szenarioDataProvider = (ISzenarioDataProvider) context.getVariable( SzenarioSourceProvider.ACTIVE_SZENARIO_DATA_PROVIDER_NAME );
    final ITerrainModel model = (ITerrainModel) szenarioDataProvider.getModel( ITerrainModel.class );

    final IWizardDescriptor wizardDescriptor = workbench.getNewWizardRegistry().findWizard( ROUGHNESS_IMPORT_WIZARD_ID );
    final INewWizardKalypsoImport wizard = (INewWizardKalypsoImport) wizardDescriptor.createWizard();
    final WizardDialog wizardDialog = new WizardDialog( workbenchWindow.getShell(), wizard );
    final IFolder szenarioPath = (IFolder) context.getVariable( SzenarioSourceProvider.ACTIVE_SZENARIO_FOLDER_NAME );

    final IFolder currentFolder = (IFolder) context.getVariable( SzenarioSourceProvider.ACTIVE_SZENARIO_FOLDER_NAME );
    final IRoughnessPolygonCollection roughnessPolygonCollection = model.getRoughnessPolygonCollection();
    final HashMap<String, Object> data = new HashMap<String, Object>();
    data.put( "SzenarioPath", szenarioPath );
    data.put( "Project", currentFolder.getProject() );
    data.put( "IRoughnessPolygonCollection", roughnessPolygonCollection );
    data.put( "RoughnessDatabaseLocation", "/.metadata/roughness.gml" );
    data.put( "ProjectBaseFolder", currentFolder.getFullPath().segment( 0 ) );
    // data.put( "ActiveSimulationModelBaseFolder", currentFolder.getFullPath() );
    wizard.init( workbench, selection );
    wizard.initModelProperties( data );
    
    final String mapFileName = "maps/roughness.gmt";
    final OpenMapViewCommandHandler openMapViewCommandHandler = new OpenMapViewCommandHandler();
    final HashMap<String, String> paramMap = new HashMap<String, String>();
    paramMap.put( OpenMapViewCommandHandler.PARAM_RESOURCE, mapFileName );
    openMapViewCommandHandler.setInitializationData( null, null, paramMap );
    openMapViewCommandHandler.executeInternal( event );
    openMapViewCommandHandler.dispose();
    
    if( wizardDialog.open() == Window.OK )
    {
      return Status.OK_STATUS;
    }
    return Status.CANCEL_STATUS;

  }
}
