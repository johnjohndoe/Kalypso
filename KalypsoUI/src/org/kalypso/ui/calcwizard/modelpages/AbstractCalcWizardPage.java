package org.kalypso.ui.calcwizard.modelpages;

import java.util.Properties;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandTarget;
import org.kalypso.util.command.JobExclusiveCommandTarget;

/**
 * @author Belger
 */
public abstract class AbstractCalcWizardPage extends WizardPage implements IModelWizardPage,
    ICommandTarget
{
  private final ICommandTarget m_commandTarget = new JobExclusiveCommandTarget( null );

  private Properties m_arguments = null;

  private IProject m_project = null;

  private IFolder m_calcFolder = null;
  
  private Properties m_replaceProperties = new Properties();

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
  
  public IFolder getCalcFolder()
  {
    return m_calcFolder;
  }

  /**
   * @see org.kalypso.ui.calcwizard.modelpages.IModelWizardPage#init(org.eclipse.core.resources.IProject, java.lang.String, org.eclipse.jface.resource.ImageDescriptor, java.util.Properties, org.eclipse.core.resources.IFolder)
   */
  public void init( final IProject project, final String pagetitle,
      final ImageDescriptor imagedesc, final Properties arguments, final IFolder calcFolder )
  {
    setTitle( pagetitle );
    setImageDescriptor( imagedesc );
    m_project = project;
    m_arguments = arguments;
    m_calcFolder = calcFolder;
    
    m_replaceProperties.setProperty( "calcdir:", calcFolder.getProjectRelativePath().toString() + "/" );
  }

  /**
   * @see org.kalypso.util.command.ICommandTarget#postCommand(org.kalypso.util.command.ICommand,
   *      java.lang.Runnable)
   */
  public void postCommand( final ICommand command, final Runnable runnable )
  {
    m_commandTarget.postCommand( command, runnable );
  }

  /** Diese Properties werden benutzt, um die Vorlagendateien zu parsen */
  protected Properties getReplaceProperties()
  {
    return m_replaceProperties;
  }
}