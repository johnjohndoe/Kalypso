package org.kalypso.model.flood.extension;

import java.io.File;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.swt.graphics.Image;
import org.kalypso.afgui.extension.IKalypsoModule;
import org.kalypso.afgui.extension.IKalypsoModuleEnteringPageHandler;
import org.kalypso.afgui.extension.IKalypsoModuleWelcomePageHandler;
import org.kalypso.afgui.extension.INewProjectWizard;
import org.kalypso.afgui.extension.IProjectDatabaseFilter;
import org.kalypso.afgui.extension.IProjectHandler;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.flood.KalypsoModelFloodPlugin;
import org.kalypso.model.flood.ui.wizards.NewDemoProjectWizard;
import org.kalypso.model.flood.ui.wizards.NewProjectWizard;

import de.renew.workflow.base.IWorkflow;
import de.renew.workflow.connector.WorkflowProjectNature;

public class KalypsoModelFloodModule implements IKalypsoModule
{
  protected static final Image IMG = new Image( null, KalypsoModelFloodModule.class.getResourceAsStream( "icons/flood_no.gif" ) );

  protected static final Image IMG_HOVER = new Image( null, KalypsoModelFloodModule.class.getResourceAsStream( "icons/flood_hover.gif" ) );

  protected static boolean INFO_PAGE_EXTRACTED = false;

  public KalypsoModelFloodModule( )
  {
  }

  @Override
  public IKalypsoModuleWelcomePageHandler getWelcomePageHandler( )
  {
    return new IKalypsoModuleWelcomePageHandler()
    {
      @Override
      public Image getIcon( )
      {
        return IMG;
      }

      @Override
      public String getLabel( )
      {
        return "KalypsoFlood";
      }

      @Override
      public String getTooltip( )
      {
        return "KalypsoFlood öffnen";
      }

      @Override
      public Image getHoverIcon( )
      {
        return IMG_HOVER;
      }
    };
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.extension.IKalypsoModule#getModuleEnteringPage()
   */
  @Override
  public IKalypsoModuleEnteringPageHandler getModuleEnteringPage( )
  {
    return new IKalypsoModuleEnteringPageHandler()
    {
      @Override
      public IProjectDatabaseFilter getDatabaseFilter( )
      {
        return new IProjectDatabaseFilter()
        {
          @Override
          public boolean select( final IProjectHandler handler )
          {
            if( handler.isLocal() )
            {
              try
              {
                final WorkflowProjectNature nature = WorkflowProjectNature.toThisNature( handler.getProject() );
                if( nature == null )
                  return false;

                final IWorkflow workflow = nature.getCurrentWorklist();
                final String uri = workflow.getURI();

                return uri.contains( "org.kalypso.model.flood.WF_KalypsoFlood" );
              }
              catch( final CoreException e )
              {
                KalypsoModelFloodPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
              }
            }

            return false;
          }
        };
      }

      @Override
      public String getHeader( )
      {
        return "KalypsoFlood";
      }

      @Override
      public URL getInfoURL( ) throws MalformedURLException
      {
        if( !INFO_PAGE_EXTRACTED )
        {
          try
          {
            /* info page of plugin */
            final InputStream zipStream = getClass().getResourceAsStream( "infoPage.zip" );
            try
            {
              final IPath stateLocation = KalypsoModelFloodPlugin.getDefault().getStateLocation();
              final File targetDir = new File( stateLocation.toFile(), "infoPage" );
              // final boolean mkdir = dir.mkdir();

              ZipUtilities.unzip( zipStream, targetDir );

              INFO_PAGE_EXTRACTED = true;
            }
            finally
            {
              zipStream.close();
            }
          }
          catch( final Exception e )
          {
            KalypsoModelFloodPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
          }
        }

        final IPath stateLocation = KalypsoModelFloodPlugin.getDefault().getStateLocation();
        final URL baseUrl = stateLocation.toFile().toURI().toURL();

        final URL url = new URL( baseUrl, "infoPage/index.html" );
        return url;
      }

      @Override
      public INewProjectWizard getDemoProjectWizard( )
      {
        return new NewDemoProjectWizard();
      }

      @Override
      public INewProjectWizard getProjectWizard( )
      {
        return new NewProjectWizard();
      }

      @Override
      public String getRemoteCommitType( )
      {
        return "KalypsoFloodModelType";
      }

      @Override
      public IWizard getImportWizard( )
      {
        return null;
      }

      @Override
      public String getImportWizardLabel( )
      {
        return null;
      }
    };
  }

}
