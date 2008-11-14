package org.kalypso.risk.extension;

import java.io.File;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.swt.graphics.Image;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsosimulationmodel.extension.IKalypsoModule;
import org.kalypso.kalypsosimulationmodel.extension.IKalypsoModuleEnteringPageHandler;
import org.kalypso.kalypsosimulationmodel.extension.IKalypsoModuleWelcomePageHandler;
import org.kalypso.kalypsosimulationmodel.extension.ILocalProjectTemplateDescription;
import org.kalypso.kalypsosimulationmodel.extension.IRemoteProjectTemplateDescription;
import org.kalypso.project.database.common.interfaces.IProjectDatabaseFilter;
import org.kalypso.project.database.common.model.ProjectHandler;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;

import de.renew.workflow.base.Workflow;
import de.renew.workflow.connector.WorkflowProjectNature;

public class KalypsoRiskModule implements IKalypsoModule
{
  protected static final Image IMG_MODEL_RISK = new Image( null, KalypsoRiskModule.class.getResourceAsStream( "icons/button_risk.gif" ) );

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
        return IMG_MODEL_RISK;
      }

      @Override
      public String getLabel( )
      {
        return "Kalypso Risk";
      }

      @Override
      public String getTooltip( )
      {
        return "Kalypso Risk öffnen";
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
          public boolean select( final ProjectHandler handler )
          {
            if( handler.isLocal() )
            {
              try
              {
                final WorkflowProjectNature nature = WorkflowProjectNature.toThisNature( handler.getProject() );
                if( nature == null )
                  return false;

                final Workflow workflow = nature.getCurrentWorklist();
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
        return "Kalypso Risk";
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
      public IRemoteProjectTemplateDescription getRemoteProjectTemplateDescription( )
      {
        return new IRemoteProjectTemplateDescription()
        {
          @Override
          public String getCommitType( )
          {
            return "KalypsoRiskModel";
          }

          @Override
          public String[] getNaturesToBeAdded( )
          {
            return new String[] {};
          }

          @Override
          public String[] getRemoteTemlateIds( )
          {
            return new String[] {};
          }
        };
      }

      @Override
      public ILocalProjectTemplateDescription getLocalProjectTemplateDescription( )
      {
        return new ILocalProjectTemplateDescription()
        {
          @Override
          public String getCategoryId( )
          {
            return "org.kalypso.risk.projectTemplate";
          }

          @Override
          public String getDemoCategoryId( )
          {
            return null;
          }
        };
      }

      @Override
      public IRemoteProjectTemplateDescription getRemoteDemoTemplateDescription( )
      {
        return new IRemoteProjectTemplateDescription()
        {

          @Override
          public String getCommitType( )
          {
            return "KalypsoRiskModel";
          }

          @Override
          public String[] getNaturesToBeAdded( )
          {
            return new String[] {};
          }

          @Override
          public String[] getRemoteTemlateIds( )
          {
            return new String[] {};
          }

        };
      }
    };
  }

}
