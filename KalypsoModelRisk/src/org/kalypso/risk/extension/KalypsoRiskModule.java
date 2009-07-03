package org.kalypso.risk.extension;

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
import org.kalypso.afgui.extension.IKalypsoProjectOpenAction;
import org.kalypso.afgui.extension.INewProjectWizard;
import org.kalypso.afgui.extension.IProjectDatabaseFilter;
import org.kalypso.afgui.extension.IProjectHandler;
import org.kalypso.afgui.extension.SzenarioProjectOpenAction;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.project.database.client.core.model.interfaces.ILocalProject;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;
import org.kalypso.risk.project.KalypsoRiskDemoProjectWizard;
import org.kalypso.risk.project.KalypsoRiskProjectWizard;

import de.renew.workflow.base.IWorkflow;
import de.renew.workflow.connector.WorkflowProjectNature;

public class KalypsoRiskModule implements IKalypsoModule
{
  protected static final Image IMG = new Image( null, KalypsoRiskModule.class.getResourceAsStream( "icons/risk_no.gif" ) );

  protected static final Image IMG_HOVER = new Image( null, KalypsoRiskModule.class.getResourceAsStream( "icons/risk_hover.gif" ) );

  protected static boolean INFO_PAGE_EXTRACTED = false;

  public KalypsoRiskModule( )
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
        return "KalypsoRisk";
      }

      @Override
      public String getTooltip( )
      {
        return "KalypsoRisk öffnen";
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
            if( handler instanceof ILocalProject )
            {
              try
              {
                final ILocalProject local = (ILocalProject) handler;
                final WorkflowProjectNature nature = WorkflowProjectNature.toThisNature( local.getProject() );
                if( nature == null )
                {
                  return false;
                }

                final IWorkflow workflow = nature.getCurrentWorklist();
                final String uri = workflow.getURI();

                return uri.contains( "http___www.tu-harburg.de_wb_kalypso_risk__WF_KalypsoRisk" ); //$NON-NLS-1$
              }
              catch( final CoreException e )
              {
                KalypsoRiskPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
              }
            }

            // TODO handle remote

            return false;
          }
        };
      }

      @Override
      public String getHeader( )
      {
        return "KalypsoRisk";
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
              final IPath stateLocation = KalypsoRiskPlugin.getDefault().getStateLocation();
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
            KalypsoRiskPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
          }

        }

        final IPath stateLocation = KalypsoRiskPlugin.getDefault().getStateLocation();
        final URL baseUrl = stateLocation.toFile().toURI().toURL();

        final URL url = new URL( baseUrl, "infoPage/index.html" );
        return url;
      }

      @Override
      public INewProjectWizard getDemoProjectWizard( )
      {
        return new KalypsoRiskDemoProjectWizard();
      }

      /**
       * @see org.kalypso.afgui.extension.IKalypsoModuleEnteringPageHandler#hasDemoProjectWizard()
       */
      @Override
      public boolean hasDemoProjectWizard( )
      {
        return true;
      }

      @Override
      public INewProjectWizard getProjectWizard( )
      {
        return new KalypsoRiskProjectWizard();
      }

      @Override
      public String getRemoteCommitType( )
      {
        return "KalypsoRiskModel";
      }

      /**
       * @see org.kalypso.afgui.extension.IKalypsoModuleEnteringPageHandler#hasImportWizard()
       */
      @Override
      public boolean hasImportWizard( )
      {
        return false;
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

      @Override
      public Integer getPriority( )
      {
        return 5;
      }

      @Override
      public IKalypsoProjectOpenAction getProjectOpenAction( )
      {
        return new SzenarioProjectOpenAction();
      }
    };
  }

}
