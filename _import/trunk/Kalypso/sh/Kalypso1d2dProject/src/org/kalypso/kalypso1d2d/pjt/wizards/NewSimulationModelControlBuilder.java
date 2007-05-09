package org.kalypso.kalypso1d2d.pjt.wizards;

import java.util.logging.Logger;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
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
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.afgui.scenarios.ScenarioManager;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DProjectNature;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypso1d2d.pjt.actions.ScenarioHelper;
import org.kalypso.kalypso1d2d.pjt.i18n.Messages;

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

  private KeyListener keyListener = new KeyListener()
  {

    public void keyPressed( KeyEvent e )
    {
      // Empty
    }

    public void keyReleased( KeyEvent e )
    {
      cacheNewName();
      if( updateListener != null )
      {
        updateListener.update();
      }
    }

  };

  private final Scenario m_scenario;

  public NewSimulationModelControlBuilder( final Scenario scenario, final Composite parentComposite )
  {
    m_scenario = scenario;
    createControl( parentComposite );
    cacheNewName();
  }

  private void createControl( Composite parent )
  {
    panel = new Composite( parent, SWT.FILL );
    GridLayout gl = new GridLayout();
    gl.numColumns = 2;
    panel.setLayout( gl );
    panel.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    Label newModelNameLabel = new Label( panel, SWT.NONE );
    newModelNameLabel.setText( Messages.getString( "NewSimulationModelControlBuilder.4" ) ); //$NON-NLS-1$
    newModelTFE = new Text( panel, SWT.BORDER );
    newModelTFE.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    newModelTFE.addKeyListener( keyListener );

    Label parentLabel = new Label( panel, SWT.NONE );
    parentLabel.setText( Messages.getString( "NewSimulationModelControlBuilder.5" ) ); //$NON-NLS-1$
    parentTFE = new Text( panel, SWT.BORDER );
    parentTFE.setEditable( false );
    parentTFE.setText( getParentDataName() );
    parentTFE.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    Label commentLabel = new Label( panel, SWT.NONE );
    commentLabel.setText( Messages.getString( "NewSimulationModelControlBuilder.6" ) ); //$NON-NLS-1$
    commentText = new Text( panel, SWT.BORDER | SWT.WRAP | SWT.MULTI );
    GridData gd = new GridData( GridData.FILL_BOTH );
    gd.verticalSpan = 10;
    commentText.setLayoutData( gd );

    // VerifyListener vl;

  }

  private String getParentDataName( )
  {
    if( m_scenario == null )
    {
      return "<nicht vorhanden>";
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

    String comment = commentText.getText();
    return comment;
  }

  public boolean isValid( )
  {
    Boolean answer = true;
    StringBuffer errors = new StringBuffer( 128 );
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

  public void setUpdateListerner( @SuppressWarnings("hiding")//$NON-NLS-1$
  IUpdateListener updateListener )
  {
    this.updateListener = updateListener;
  }

  public static void startWizard( final Shell shell, final Scenario scenario )
  {
    logger.info( "starting wizard" ); //$NON-NLS-1$
    final NewSimulationModelWizardPage wpage = new NewSimulationModelWizardPage( "Neues Simulation Model", scenario );

    Wizard iWizard = new Wizard()
    {

      @Override
      public boolean performFinish( )
      {
        NewSimulationModelWizardPage page = (NewSimulationModelWizardPage) getStartingPage();
        boolean answer = page.getNewSimulaionControlBuilder().isValid();
        if( !answer )
        {
          page.setErrorMessage( "" );
        }
        return answer;
      }

    };

    iWizard.addPage( wpage );
    // wpage.setTitle("spage");
    WizardDialog wd = new WizardDialog( shell, iWizard );
    wd.setTitle( Messages.getString( "NewSimulationModelControlBuilder.20" ) ); //$NON-NLS-1$
    // wd.setMessage("Neue Simulationsmodell");
    // wd.setBlockOnOpen(true);
    int decision = wd.open();
    if( decision == WizardDialog.OK )
    {
      String name = wpage.getNewSimulaionControlBuilder().getNewName();
      logger.info( "newName=" + name ); //$NON-NLS-1$
      final ActiveWorkContext activeWorkContext = Kalypso1d2dProjectPlugin.getDefault().getActiveWorkContext();
      try
      {
        final String projectName = ScenarioHelper.getProjectName( scenario );
        final IWorkspace workspace = ResourcesPlugin.getWorkspace();
        final IProject project = workspace.getRoot().getProject( projectName );
        final Kalypso1D2DProjectNature nature = Kalypso1D2DProjectNature.toThisNature( project );
        final ScenarioManager scenarioManager = (ScenarioManager) nature.getCaseManager();
        if( scenario != null )
        {
          activeWorkContext.setCurrentSzenario( scenarioManager.deriveScenario( name, scenario ) );
        }
        else
        {
          activeWorkContext.setCurrentSzenario( scenarioManager.createCase( name ) );
        }
      }
      catch( final CoreException e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        ErrorDialog.openError( shell, "Problem", "Szenario konnte nicht erstellt werden.", status );
        Kalypso1d2dProjectPlugin.getDefault().getLog().log( status );
      }
    }
    else
    {
      logger.info( "Wizard canceled:" + decision ); //$NON-NLS-1$
    }
  }
}
