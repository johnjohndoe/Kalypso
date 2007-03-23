/**
 * 
 */
package org.kalypso.afgui.workflow;

import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.wizards.IWizardDescriptor;
import org.eclipse.ui.wizards.IWizardRegistry;
import org.kalypso.afgui.workflow.EWizardType;
import org.kalypso.ui.wizards.imports.INewWizardKalypsoImport;

import de.renew.workflow.WorkflowCommandHandler;

/**
 * Opens a wizard.
 * 
 * @author Stefan Kurzbach
 */
public class WizardContextHandler extends WorkflowCommandHandler implements IHandler, IExecutableExtension
{
  public static final String WIZARD_ID = "org.kalypso.kalypso1d2d.pjt.contexts.wizardId"; //$NON-NLS-1$

  public static final String WIZARD_TYPE = "org.kalypso.kalypso1d2d.pjt.contexts.wizardType"; //$NON-NLS-1$

  private EWizardType m_wizardType;

  private String m_wizardId;

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
  @SuppressWarnings("unchecked")//$NON-NLS-1$
  @Override
  protected IStatus executeInternal( final ExecutionEvent event )
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final IWorkbenchWindow workbenchWindow = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final IWorkbench workbench = (workbenchWindow).getWorkbench();
    if( m_wizardType != null && m_wizardId != null )
    {
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
        final WizardDialog wizardDialog = new WizardDialog( workbenchWindow.getShell(), wizard );
        wizard.init( workbench, StructuredSelection.EMPTY );

        if( wizard instanceof INewWizardKalypsoImport )
        {
          ((INewWizardKalypsoImport) wizard).initModelProperties( context );
        }

        if( wizardDialog.open() == Window.OK )
        {
          return Status.OK_STATUS;
        }
        else
        {
          return Status.CANCEL_STATUS;
        }
      }
      catch( CoreException e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
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
    else
    {
      logger.severe( "Could not initialize with data of type " + data.getClass().getName() ); //$NON-NLS-1$
    }
  }
}
