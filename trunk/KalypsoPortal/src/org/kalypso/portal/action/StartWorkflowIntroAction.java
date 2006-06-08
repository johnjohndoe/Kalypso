package org.kalypso.portal.action;

import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.Properties;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.XMLMemento;
import org.eclipse.ui.internal.dialogs.NewWizard;
import org.eclipse.ui.internal.intro.impl.IntroPlugin;
import org.eclipse.ui.intro.IIntroSite;
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.ui.MementoUtils;
import org.kalypso.contribs.eclipse.ui.MementoWithUrlResolver;
import org.kalypso.portal.KalypsoPortalPlugin;
import org.kalypso.portal.wizard.LoadProjectFromWorkspaceWizard;
import org.kalypso.workflow.WorkflowContext;
import org.kalypso.workflow.ui.KalypsoWorkFlowPlugin;

public class StartWorkflowIntroAction extends AbstractIntroAction
{
  // stateXMLURL for change perspective
  public final static String INIT_URL = "stateURL";

  // initial project archive url to initalize project
  public final static String INIT_PROJ_URL = "sourceURL";

  // TODO woher kommt diese information oder anders gesagt aus welcher configuration kommt die
  private static final String CALC_CASE_BASE_DIR = "Planvarianten";

  @Override
  public void run( IIntroSite site, Properties params )
  {
    super.run( site, params );
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final String urlInitalPage = (String) params.get( INIT_URL );
    final String urlContentProj = (String) params.get( INIT_PROJ_URL );
    if( urlInitalPage != null && urlInitalPage.length() > 0 )
    {
      final IProject project = selectProject( workbench, urlContentProj );
      if( project != null )
      {
        openPlanerPerspective( urlInitalPage, project );
        IntroPlugin.closeIntro();
      }
    }
  }

  private IProject selectProject( IWorkbench workbench, String initialProjectAsArchiveURLString )
  {
    IProject newProject = null;
    final NewWizard selectionWizard = new NewWizard();

    selectionWizard.init( workbench, StructuredSelection.EMPTY );
    selectionWizard.setCategoryId( "org.kalypso.portal.loadProject.wizard" );

    final IDialogSettings dialogSettings = KalypsoPortalPlugin.getDefault().getDialogSettings();
    selectionWizard.setDialogSettings( dialogSettings );
    final Shell shell = workbench.getActiveWorkbenchWindow().getShell();
    final WizardDialog dialog = new WizardDialog( shell, selectionWizard );

    int open = dialog.open();
    if( open == Window.OK )
    {
      // get project handle
      final IWizardPage currentPage = dialog.getCurrentPage();
      IWizard wizard = null;
      final WorkflowContext wfContext = KalypsoWorkFlowPlugin.getDefault().getDefaultWorkflowContext();
      if( currentPage != null )
      {
        wizard = currentPage.getWizard();
        if( wizard instanceof BasicNewProjectResourceWizard )
        {
          newProject = ((BasicNewProjectResourceWizard) wizard).getNewProject();
          // set modelnature
          // TODO does not work jet, problem is that we are not allowed to alter the project discribtion once the
          // project has been created. So we have to write our own new project wizard and cannot use the eclipse one
          try
          {
            final IProjectDescription description = newProject.getDescription();
            final String[] natures = description.getNatureIds();
            final String[] newNatures = new String[natures.length + 1];
            System.arraycopy( natures, 0, newNatures, 0, natures.length );
            // TODO how do I get the the rrm modelnature from a central place
            newNatures[natures.length] = "org.kalypso.simulation.ui.calccase.ModelNature";
            description.setNatureIds( newNatures );
            newProject.setDescription( description, null );
          }
          catch( CoreException e )
          {
            MessageDialog.openWarning( shell, "Flows Portal Warning", "Aus dem neuen Projekt k�nnen sie keine Berechnungen durchf�hren, initzialisierungs Fehler der Projekt Nature" );
          }
          wfContext.setContextProject( newProject );
          InputStream resourceAsStream = null;
          try
          {
            // copy the resources from the url to the project
            final URL sourceURL = wfContext.resolveURL( initialProjectAsArchiveURLString );
            final File targetDir = newProject.getLocation().toFile();
            resourceAsStream = sourceURL.openStream();
            ZipUtilities.unzip( resourceAsStream, targetDir );
            // refresh on the whole project
            newProject.refreshLocal( IResource.DEPTH_INFINITE, null );
          }
          catch( Exception e )
          {
            e.printStackTrace();
          }
          finally
          {
            IOUtils.closeQuietly( resourceAsStream );
          }

        }
        if( wizard instanceof LoadProjectFromWorkspaceWizard )
        {
          newProject = ((LoadProjectFromWorkspaceWizard) wizard).getProject();
          wfContext.setContextProject( newProject );
        }
        // sets the calc case directory for this project
        final IFolder calcCasefolder = newProject.getFolder( CALC_CASE_BASE_DIR );
        if( calcCasefolder.exists() )
          wfContext.setContextCalcDir( calcCasefolder );
      }

      return newProject;
    }
    return null;
  }

  // private void getProjectFromServer( IWorkbench workbench )
  // {
  // IWorkbenchPage[] pages = workbench.getActiveWorkbenchWindow().getPages();
  // final Shell shell = workbench.getActiveWorkbenchWindow().getShell();
  // IWorkbenchPartSite site = null;
  // try
  // {
  // IViewPart part = pages[0].showView( IPageLayout.ID_OUTLINE );
  // site = part.getSite();
  // }
  // catch( PartInitException e )
  // {
  // e.printStackTrace();
  // }
  //
  // SiteDialog dialog2 = new SiteDialog( shell, site );
  //
  // int returnCode = dialog2.open();
  // if( returnCode == Window.OK )
  // {
  // System.out.println();
  // }
  // }

  private boolean openPlanerPerspective( final String stateURLAsString, final IProject project )
  {
    IMemento replacableMemento = null;
    try
    {
      final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
      final WorkflowContext wfContext = KalypsoWorkFlowPlugin.getDefault().getDefaultWorkflowContext();
      final URL stateURL = wfContext.resolveURL( stateURLAsString );
      final InputStreamReader reader = new InputStreamReader( stateURL.openStream() );
      final XMLMemento originalMemento = XMLMemento.createReadRoot( reader );
      final Properties props = new Properties();
      props.setProperty( MementoWithUrlResolver.PATH_KEY, root.getLocation().toString() );
      props.setProperty( MementoWithUrlResolver.PROJECT_KEY, project.getName() );
      replacableMemento = new MementoWithUrlResolver( originalMemento, props, wfContext );

    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    MementoUtils.restoreWorkbenchPage( replacableMemento );

    return true;
  }
}
