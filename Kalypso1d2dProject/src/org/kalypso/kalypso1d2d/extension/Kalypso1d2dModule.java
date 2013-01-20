package org.kalypso.kalypso1d2d.extension;

import java.net.URL;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.afgui.scenarios.SzenarioProjectOpenAction;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DDemoProjectWizard;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DNewProjectWizard;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DProjectNature;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.module.AbstractKalypsoModule;
import org.kalypso.module.IKalypsoModuleProjectOpenAction;
import org.kalypso.module.IKalypsoModuleWelcomePageFrame;
import org.kalypso.module.ISetAsBaseScenarioHandler;
import org.kalypso.module.welcome.INewProjectWizard;
import org.kalypso.module.welcome.INewProjectWizardProvider;

public class Kalypso1d2dModule extends AbstractKalypsoModule
{
  public static final String ID = "Kalypso1d2dModel"; //$NON-NLS-1$

  // public constructor, needed because of declared extension point and java class loader
  public Kalypso1d2dModule( )
  {
  }

  @Override
  public String getHeader( )
  {
    return "Kalypso1D2D"; //$NON-NLS-1$
  }

  @Override
  public URL getInfoURL( )
  {
    return getInfoURL( getClass(), Kalypso1d2dProjectPlugin.getDefault() );
  }

  @Override
  public Integer getPriority( )
  {
    return 3;
  }

  @Override
  public IKalypsoModuleWelcomePageFrame getWelcomePageFrame( )
  {
    return new Kalypso1d2dWelcomePageFrame();
  }

  @Override
  public String getId( )
  {
    return ID;
  }

  @Override
  protected INewProjectWizardProvider getNewProjectWizard( )
  {
    return new INewProjectWizardProvider()
    {
      @Override
      public INewProjectWizard createWizard( )
      {
        return new Kalypso1D2DNewProjectWizard();
      }
    };
  }

  @Override
  protected INewProjectWizardProvider getDemoProjectWizard( )
  {
    return new INewProjectWizardProvider()
    {
      @Override
      public INewProjectWizard createWizard( )
      {
        return new Kalypso1D2DDemoProjectWizard();
      }
    };
  }

  @Override
  public IKalypsoModuleProjectOpenAction getProjectOpenAction( )
  {
    return new SzenarioProjectOpenAction( ID );
  }

  @Override
  public boolean acceptProject( final IProject project ) throws CoreException
  {
    return Kalypso1D2DProjectNature.isOfThisNature( project );
  }

  @Override
  public String getNewProjectCategoryId( )
  {
    return Kalypso1D2DNewProjectWizard.CATEGORY_ID;
  }

  @Override
  public ISetAsBaseScenarioHandler getSetAsBaseScenarioHandler( )
  {
    return null;
  }
}