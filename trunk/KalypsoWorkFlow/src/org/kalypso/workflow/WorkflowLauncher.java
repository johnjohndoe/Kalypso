package org.kalypso.workflow;

import java.io.File;
import java.net.URL;

import org.eclipse.core.runtime.IPath;
import org.eclipse.ui.IEditorLauncher;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.PlatformUI;
import org.kalypso.workflow.ui.browser.CommandURLBrowserView;

public class WorkflowLauncher implements IEditorLauncher
{

  public void open( IPath file )
  {

    final String DEFAULT_BROWSER_VIEW = "org.kalypso.workflow.ui.WorkflowBrowserView";
    try
    {
      final IViewPart part = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().showView( DEFAULT_BROWSER_VIEW );
      if( part instanceof CommandURLBrowserView )
      {
        final CommandURLBrowserView browser = (CommandURLBrowserView) part;
        final File inputFile = file.toFile();
        final URL url = inputFile.toURL();
        final String urlAsString = url.toExternalForm();
        browser.setURL( urlAsString );
      }

    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

  }

}
