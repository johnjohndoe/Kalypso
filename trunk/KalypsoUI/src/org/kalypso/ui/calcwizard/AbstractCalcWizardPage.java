package org.kalypso.ui.calcwizard;

import java.util.Properties;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandTarget;
import org.kalypso.util.command.JobExclusiveCommandTarget;

/**
 * @author Belger
 */
public abstract class AbstractCalcWizardPage extends WizardPage implements ICalcWizardPage,
    ICommandTarget
{
  private final ICommandTarget m_commandTarget = new JobExclusiveCommandTarget( null );

  private Properties m_arguments = null;

  private IProject m_project = null;

  public AbstractCalcWizardPage( final String name )
  {
    super( name );
  }

  public Properties getArguments()
  {
    return m_arguments;
  }

  public IProject getProject()
  {
    return m_project;
  }

  /**
   * @see org.kalypso.ui.calcwizard.ICalcWizardPage#init(org.eclipse.core.resources.IProject,
   *      java.lang.String, org.eclipse.jface.resource.ImageDescriptor,
   *      java.util.Properties)
   */
  public void init( final IProject project, final String pagetitle,
      final ImageDescriptor imagedesc, final Properties arguments )
  {
    setTitle( pagetitle );
    setImageDescriptor( imagedesc );
    m_project = project;
    m_arguments = arguments;
  }

  /**
   * @see org.kalypso.util.command.ICommandTarget#postCommand(org.kalypso.util.command.ICommand,
   *      java.lang.Runnable)
   */
  public void postCommand( final ICommand command, final Runnable runnable )
  {
    m_commandTarget.postCommand( command, runnable );
  }
}