/**
 * 
 */
package de.renew.workflow.contexts;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.wizards.IWizardDescriptor;
import org.eclipse.ui.wizards.IWizardRegistry;

/**
 * Opens a wizard.
 * 
 * @author Stefan Kurzbach
 */
public class WizardContextHandler extends AbstractHandler implements IExecutableExtension
{
  public static final String WIZARD_ID = "org.kalypso.kalypso1d2d.pjt.contexts.wizardId"; //$NON-NLS-1$

  public static final String WIZARD_TYPE = "org.kalypso.kalypso1d2d.pjt.contexts.wizardType"; //$NON-NLS-1$

  private EWizardType m_wizardType;

  private String m_wizardId;

  public WizardContextHandler( )
  {
  }

  /**
   * Creates a new {@link WizardContextHandler} that opens the wizard with the given id
   */
  public WizardContextHandler( final String wizardId, final EWizardType wizardType )
  {
    m_wizardId = wizardId;
    m_wizardType = wizardType;
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @SuppressWarnings("unchecked")
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
    final IWorkbench workbench = PlatformUI.getWorkbench();
    if( m_wizardType != null && m_wizardId != null )
    {
      // FIXME: This is not good!!! As these wizards are also registered within the export/import/new extension-points,
      // they appear also in the eclipse-ui, but there, they won't work because of the missing context...
      // This MUST be fixed, else Kalypso-Expert Mode is not usable any more!!!

      final IWizardRegistry wizardRegistry;
      switch( m_wizardType )
      {
        case EXPORT_WIZARD:
          wizardRegistry = workbench.getExportWizardRegistry();
          break;
        case IMPORT_WIZARD:
          wizardRegistry = workbench.getImportWizardRegistry();
          break;
        case NEW_WIZARD:
          wizardRegistry = workbench.getNewWizardRegistry();
          break;
        default:
          wizardRegistry = null;
      }
      final IWizardDescriptor wizardDescriptor = wizardRegistry.findWizard( m_wizardId );
      try
      {
        final IWorkbenchWizard wizard = wizardDescriptor.createWizard();
        final WizardDialog wizardDialog = new WizardDialog( shell, wizard );
        wizard.init( workbench, StructuredSelection.EMPTY );

        if( wizardDialog.open() == Window.OK && wizard instanceof IDialogWithResult )
        {
          // TODO: dubios: first, no one rally implements that, second: javadoc of this methods sais, nothing has to be
          // returned
          return ((IDialogWithResult) wizard).getResult();
        }
        else
        {
          return wizardDialog.getReturnCode();
        }
      }
      catch( final CoreException e )
      {
        throw new ExecutionException( Messages.getString( "WizardContextHandler.1" ) + m_wizardId + Messages.getString( "WizardContextHandler.2" ) + m_wizardType, e ); //$NON-NLS-1$ //$NON-NLS-2$
      }
    }
    return Status.OK_STATUS;
  }

  /**
   * @see org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org.eclipse.core.runtime.IConfigurationElement,
   *      java.lang.String, java.lang.Object)
   */
  public void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data )
  {
    if( data instanceof Map )
    {
      final Map parameterMap = (Map) data;
      m_wizardId = (String) parameterMap.get( WIZARD_ID );
      m_wizardType = EWizardType.valueOf( (String) parameterMap.get( WIZARD_TYPE ) );
    }
  }
}
