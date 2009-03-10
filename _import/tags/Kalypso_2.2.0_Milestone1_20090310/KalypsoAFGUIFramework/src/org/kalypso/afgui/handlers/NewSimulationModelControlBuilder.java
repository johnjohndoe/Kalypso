package org.kalypso.afgui.handlers;

import java.util.logging.Logger;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.ScenarioHandlingProjectNature;
import org.kalypso.afgui.scenarios.IScenario;
import org.kalypso.afgui.scenarios.ScenarioManager;
import org.kalypso.afgui.scenarios.TaskExecutionAuthority;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;

import de.renew.workflow.base.ITask;
import de.renew.workflow.connector.context.ActiveWorkContext;

/**
 * @author Patrice Congo
 */
public class NewSimulationModelControlBuilder
{

  final static String NEW_NAME_MUST_NOT_BE_EMPTY = Messages.getString( "NewSimulationModelControlBuilder.0" ); //$NON-NLS-1$

  final static String ALLREADY_EXISTS = Messages.getString( "NewSimulationModelControlBuilder.1" ); //$NON-NLS-1$

  final static Logger logger = Logger.getLogger( NewSimulationModelControlBuilder.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.kalypso1d2d.pjt/debug" ) ); //$NON-NLS-1$

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  private String errorMessage;

  private Composite panel;

  private Text parentTFE;

  private Text newModelTFE;

  private Text commentText;

  IUpdateListener updateListener;

  private String newName;

  private final KeyListener keyListener = new KeyListener()
  {

    public void keyPressed( final KeyEvent e )
    {
      // Empty
    }

    public void keyReleased( final KeyEvent e )
    {
      cacheNewName();
      if( updateListener != null )
      {
        updateListener.update();
      }
    }

  };

  private final IScenario m_scenario;

  public NewSimulationModelControlBuilder( final IScenario scenario, final Composite parentComposite )
  {
    m_scenario = scenario;
    createControl( parentComposite );
    cacheNewName();
  }

  private void createControl( final Composite parent )
  {
    panel = new Composite( parent, SWT.FILL );
    final GridLayout gl = new GridLayout();
    gl.numColumns = 2;
    panel.setLayout( gl );
    panel.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    final Label newModelNameLabel = new Label( panel, SWT.NONE );
    newModelNameLabel.setText( Messages.getString( "NewSimulationModelControlBuilder.3" ) ); //$NON-NLS-1$
    newModelTFE = new Text( panel, SWT.BORDER );
    newModelTFE.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    newModelTFE.addKeyListener( keyListener );

    final Label parentLabel = new Label( panel, SWT.NONE );
    parentLabel.setText( Messages.getString( "NewSimulationModelControlBuilder.4" ) ); //$NON-NLS-1$
    parentTFE = new Text( panel, SWT.BORDER );
    parentTFE.setEditable( false );
    parentTFE.setText( getParentDataName() );
    parentTFE.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    final Label commentLabel = new Label( panel, SWT.NONE );
    commentLabel.setText( Messages.getString( "NewSimulationModelControlBuilder.5" ) ); //$NON-NLS-1$
    commentText = new Text( panel, SWT.BORDER | SWT.WRAP | SWT.MULTI );
    final GridData gd = new GridData( GridData.FILL_BOTH );
    gd.verticalSpan = 10;
    commentText.setLayoutData( gd );

    // VerifyListener vl;

  }

  private String getParentDataName( )
  {
    if( m_scenario == null )
    {
      return Messages.getString( "NewSimulationModelControlBuilder.6" ); //$NON-NLS-1$
    }
    return m_scenario.getName();
  }

  public String cacheNewName( )
  {
    newName = newModelTFE.getText();

    newName = (newName == null) ? "" : newName.trim(); //$NON-NLS-1$
    return newName;
  }

  public String getNewName( )
  {
    return newName;
  }

  public String getComment( )
  {
    // FormLayout fl=null;

    final String comment = commentText.getText();
    return comment;
  }

  public boolean isValid( )
  {
    Boolean answer = true;
    final StringBuffer errors = new StringBuffer( 128 );
    if( newName.equals( "" ) ) //$NON-NLS-1$
    {
      answer = answer && false;
      errors.append( NEW_NAME_MUST_NOT_BE_EMPTY );
    }

    errorMessage = errors.toString();
    return answer;
  }

  public Control getControl( )
  {
    return panel;
  }

  public String getErrorMessage( )
  {
    return errorMessage;
  }

  public void setUpdateListerner( @SuppressWarnings("hiding")
  final IUpdateListener updateListener )
  {
    this.updateListener = updateListener;
  }

  public static void startWizard( final Shell shell, final IScenario scenario, final IProject project )
  {
    logger.info( Messages.getString( "NewSimulationModelControlBuilder.10" ) ); //$NON-NLS-1$
    final NewSimulationModelWizardPage wpage = new NewSimulationModelWizardPage( Messages.getString( "NewSimulationModelControlBuilder.11" ), scenario ); //$NON-NLS-1$

    final Wizard iWizard = new Wizard()
    {

      @Override
      public boolean performFinish( )
      {
        final NewSimulationModelWizardPage page = (NewSimulationModelWizardPage) getStartingPage();
        final boolean answer = page.getNewSimulaionControlBuilder().isValid();
        if( !answer )
        {
          page.setErrorMessage( "" ); //$NON-NLS-1$
        }
        return answer;
      }

    };

    iWizard.addPage( wpage );
    // wpage.setTitle("spage");
    final WizardDialog wd = new WizardDialog( shell, iWizard );
    wd.setTitle( Messages.getString( "NewSimulationModelControlBuilder.13" ) ); //$NON-NLS-1$
    // wd.setMessage("Neue Simulationsmodell");
    // wd.setBlockOnOpen(true);
    final int decision = wd.open();
    if( decision == WizardDialog.OK )
    {
      final String name = wpage.getNewSimulaionControlBuilder().getNewName();
      logger.info( "newName=" + name ); //$NON-NLS-1$
      final KalypsoAFGUIFrameworkPlugin plugin = KalypsoAFGUIFrameworkPlugin.getDefault();
      final ActiveWorkContext<IScenario> activeWorkContext = plugin.getActiveWorkContext();
      try
      {
        final ScenarioHandlingProjectNature nature = ScenarioHandlingProjectNature.toThisNature( project );
        final ScenarioManager scenarioManager = (ScenarioManager) nature.getCaseManager();
        final TaskExecutionAuthority taskExecutionAuthority = plugin.getTaskExecutionAuthority();
        final ITask activeTask = plugin.getTaskExecutor().getActiveTask();
        if( taskExecutionAuthority.canStopTask( activeTask ) )
        {
          if( scenario != null )
          {
            activeWorkContext.setCurrentCase( scenarioManager.deriveScenario( name, scenario ) );
          }
          else
          {
            activeWorkContext.setCurrentCase( scenarioManager.createCase( name ) );
          }
        }
      }
      catch( final CoreException e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        ErrorDialog.openError( shell, Messages.getString( "NewSimulationModelControlBuilder.15" ), Messages.getString( "NewSimulationModelControlBuilder.16" ), status ); //$NON-NLS-1$ //$NON-NLS-2$
        KalypsoAFGUIFrameworkPlugin.getDefault().getLog().log( status );
      }
    }
    else
    {
      logger.info( Messages.getString( "NewSimulationModelControlBuilder.17" ) + decision ); //$NON-NLS-1$
    }
  }
}
