package org.kalypso.kalypso1d2d.pjt.actions;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

import javax.xml.bind.JAXBException;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
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
import org.kalypso.commons.command.DefaultCommandManager;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapModell;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.wizard.image.ImportImageSourceWizard;
import org.kalypso.ui.wizards.imports.INewWizardKalypsoImport;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypsodeegree.model.geometry.GM_Envelope;

import de.renew.workflow.WorkflowCommandHandler;

/**
 * Starts the import roughness wizard
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportBaseMapHandler extends WorkflowCommandHandler
{
  private static final String WIZARD_ID = "org.kalypso.ui.wizards.imports.baseMap.ImportBaseMapWizard";

  private ICommandTarget m_commandTarget = new JobExclusiveCommandTarget( new DefaultCommandManager(), new Runnable()
  {

    public void run( )
    {
    }
  } );

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
      final IResource currentFolder = (IFolder) context.getVariable( "activeSimulationModelBaseFolder" );
      selection = new StructuredSelection( currentFolder );
    }
    final IWorkbenchWindow workbenchWindow = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final IWorkbench workbench = (workbenchWindow).getWorkbench();

    final IWizardDescriptor wizardDescriptor = workbench.getNewWizardRegistry().findWizard( WIZARD_ID );
    final INewWizardKalypsoImport wizard = (INewWizardKalypsoImport) wizardDescriptor.createWizard();
    final WizardDialog wizardDialog = new WizardDialog( workbenchWindow.getShell(), wizard );

    final IFolder currentFolder = (IFolder) context.getVariable( "activeSimulationModelBaseFolder" );
    final HashMap<String, Object> data = new HashMap<String, Object>();
    data.put( "ScenarioFolder", currentFolder.getFullPath().toOSString() );
    // data.put( "ActiveSimulationModelBaseFolder", currentFolder.getFullPath() );

    wizard.init( workbench, selection );
    wizard.initModelProperties( data );
    if( wizardDialog.open() == Window.OK )
    {
      try
      {
        final ImportImageSourceWizard addThemeWizard = new ImportImageSourceWizard();
        final WizardDialog addThemeWizardDialog = new WizardDialog( workbenchWindow.getShell(), addThemeWizard );
        addThemeWizard.init( workbench, selection );
        addThemeWizard.setCommandTarget( m_commandTarget );
        final IFile file = currentFolder.getFile( "maps/base.gmt" );
        final IProject project = file.getProject();
        final URL contextUrl = ResourceUtilities.createURL( file );
        final Gismapview gisview = GisTemplateHelper.loadGisMapView( file );
        final IFeatureSelectionManager selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();
        final GisTemplateMapModell mapModell = new GisTemplateMapModell( contextUrl, KalypsoGisPlugin.getDefault().getCoordinatesSystem(), project, selectionManager );        
        mapModell.createFromTemplate( gisview );
        for( IKalypsoTheme theme : mapModell.getAllThemes() )
        {
          mapModell.removeTheme( theme );
        }
        addThemeWizard.setMapModel( mapModell );
        if( addThemeWizardDialog.open() == Window.OK )
        {
          final ByteArrayOutputStream bos = new ByteArrayOutputStream();

          final String srsName = KalypsoGisPlugin.getDefault().getCoordinatesSystem().getName();
          final Gismapview modellTemplate = mapModell.createGismapTemplate( null, srsName );
          modellTemplate.setExtent( gisview.getExtent() );
          GisTemplateHelper.saveGisMapView( modellTemplate, bos, file.getCharset() );

          final ByteArrayInputStream bis = new ByteArrayInputStream( bos.toByteArray() );
          bos.close();
          file.setContents( bis, false, true, null );
          return Status.OK_STATUS;
        }
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
    return Status.CANCEL_STATUS;

  }
}
