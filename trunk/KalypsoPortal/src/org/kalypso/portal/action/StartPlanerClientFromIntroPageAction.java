package org.kalypso.portal.action;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.util.Geometry;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.WorkbenchException;
import org.eclipse.ui.XMLMemento;
import org.eclipse.ui.internal.IWorkbenchConstants;
import org.eclipse.ui.internal.WorkbenchPage;
import org.eclipse.ui.internal.WorkbenchWindow;
import org.eclipse.ui.internal.dialogs.NewWizard;
import org.eclipse.ui.internal.intro.impl.IntroPlugin;
import org.eclipse.ui.internal.intro.impl.model.IntroModelRoot;
import org.eclipse.ui.internal.registry.PerspectiveDescriptor;
import org.eclipse.ui.internal.registry.PerspectiveRegistry;
import org.eclipse.ui.intro.IIntroPart;
import org.eclipse.ui.intro.IIntroSite;
import org.eclipse.ui.intro.config.CustomizableIntroPart;
import org.eclipse.ui.intro.config.IIntroAction;
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.ui.MementoUtils;
import org.kalypso.contribs.eclipse.ui.MementoWithUrlResolver;
import org.kalypso.contribs.java.net.IUrlResolver2;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.portal.IKalypsoPortalConstants;
import org.kalypso.portal.KalypsoPortalPlugin;
import org.kalypso.portal.dialog.SiteDialog;
import org.kalypso.portal.wizard.LoadProjectFromWorkspaceWizard;

public class StartPlanerClientFromIntroPageAction extends Action implements IIntroAction
{

  private IStructuredSelection m_selection;

  public StartPlanerClientFromIntroPageAction( )
  {
  }

  public void run( IIntroSite site, Properties params )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final String urlInitalPage = (String) params.get( IKalypsoPortalConstants.INIT_URL );
    final String urlContentProj = (String) params.get( IKalypsoPortalConstants.INIT_PROJ_URL );

    final IProject project = getProject( workbench );
    if( project != null )
    {
      KalypsoPortalPlugin.getDefault().setContext( project );
      closeIntroPlugin( workbench );
      URL contUrl = null;
      if( urlInitalPage != null && urlInitalPage.length() > 0 )
        try
        {
          contUrl = new URL( urlContentProj );
          // copy the resources from the url to the workspace
          // (project)
          copyResourcesToProject( project.getLocation(), contUrl );
        }
        catch( MalformedURLException e )
        {
          e.printStackTrace();
        }

      openPlanerPerspective( urlInitalPage, project );
    }

  }

  private IProject getProject( IWorkbench workbench )
  {

    IProject newProject = null;
    final NewWizard selectionWizard = new NewWizard();

    selectionWizard.init( workbench, m_selection );
    selectionWizard.setCategoryId( "org.kalypso.portal.loadProject.wizard" );

    IDialogSettings dialogSettings = KalypsoPortalPlugin.getDefault().getDialogSettings();
    selectionWizard.setDialogSettings( dialogSettings );

    final WizardDialog dialog = new WizardDialog( workbench.getActiveWorkbenchWindow().getShell(), selectionWizard );

    int open = dialog.open();
    if( open == Window.OK )
    {
      // get project handle
      final IWizardPage currentPage = dialog.getCurrentPage();
      IWizard wizard = null;
      if( currentPage != null )
      {
        wizard = currentPage.getWizard();
        if( wizard instanceof BasicNewProjectResourceWizard )
        {
          newProject = ((BasicNewProjectResourceWizard) wizard).getNewProject();
        }
        if( wizard instanceof LoadProjectFromWorkspaceWizard )
        {
          newProject = ((LoadProjectFromWorkspaceWizard) wizard).getProject();
        }
      }
      return newProject;
    }
    return null;
  }

  private void getProjectFromServer( IWorkbench workbench )
  {

    IWorkbenchPage[] pages = workbench.getActiveWorkbenchWindow().getPages();
    final Shell shell = workbench.getActiveWorkbenchWindow().getShell();
    IWorkbenchPartSite site = null;
    try
    {
      IViewPart part = pages[0].showView( IPageLayout.ID_OUTLINE );
      site = part.getSite();
    }
    catch( PartInitException e )
    {
      e.printStackTrace();
    }

    SiteDialog dialog2 = new SiteDialog( shell, site );

    int returnCode = dialog2.open();
    if( returnCode == Window.OK )
    {
      System.out.println();
    }
  }

  private boolean closeIntroPlugin( IWorkbench workbench )
  {
    IIntroPart intro = workbench.getIntroManager().getIntro();
    if( intro == null )
      return false;
//    CustomizableIntroPart cpart = (CustomizableIntroPart) intro;
//    IntroModelRoot modelRoot = IntroPlugin.getDefault().getIntroModelRoot();
//    String pageId = modelRoot.getCurrentPageId();
//    Rectangle bounds = cpart.getControl().getBounds();
//    Rectangle startBounds = Geometry.toDisplay( cpart.getControl().getParent(), bounds );

    return IntroPlugin.closeIntro();
  }

  private boolean openPlanerPerspective( final String urlInitalPage, final IProject project )
  {

    IMemento replacableMemento = null;
    try
    {
      // refresh after copying the files into the workspace
      project.refreshLocal( IResource.DEPTH_INFINITE, null );
      final URL firstMementoUrl = UrlResolverSingleton.resolveUrl( ResourceUtilities.createURL( project ), urlInitalPage );
      final IFile file = ResourceUtilities.findFileFromURL( firstMementoUrl );

      final InputStreamReader reader = new InputStreamReader( file.getContents() );

      final XMLMemento originalMemento = XMLMemento.createReadRoot( reader );
      final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
      final Properties props = new Properties();
      props.setProperty( MementoWithUrlResolver.PATH_KEY, root.getLocation().toString() );
      props.setProperty( MementoWithUrlResolver.PROJECT_KEY, project.getName() );
      replacableMemento = new MementoWithUrlResolver( originalMemento, props, new IUrlResolver2()
      {

        public URL resolveURL( String relative ) throws MalformedURLException
        {
          return UrlResolverSingleton.resolveUrl( ResourceUtilities.createURL( project ), relative );
        }
      } );

    }
    catch( WorkbenchException e )
    {
      e.printStackTrace();
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
    catch( CoreException e )
    {
      e.printStackTrace();
    }

    MementoUtils.restoreWorkbenchPage( replacableMemento );

    return true;
  }

  private void copyResourcesToProject( IPath path, URL resourceUrl )
  {

    InputStream resourceAsStream = null;
    try
    {
      resourceAsStream = resourceUrl.openStream();
      ZipUtilities.unzip( resourceAsStream, path.toFile() );
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
}
