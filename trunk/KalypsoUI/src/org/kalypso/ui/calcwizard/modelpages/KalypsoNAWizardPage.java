package org.kalypso.ui.calcwizard.modelpages;

import java.lang.reflect.InvocationTargetException;

import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.ui.nature.ModelNature;

/**
 * @author Belger
 */
public class KalypsoNAWizardPage extends AbstractCalcWizardPage implements ModellEventListener
{
  /** Der Titel der Seite */
  public static final String PROP_MAPTITLE = "mapTitle";

  /** Position des Haupt-Sash: Integer von 0 bis 100 */
  public final static String PROP_MAINSASH = "mainSash";

  /** Position des rechten Sash: Integer von 0 bis 100 */
  public final static String PROP_RIGHTSASH = "rightSash";

  /** name der modelspec datei, die verwendet wird */
  public final static String PROP_MODELSPEC = "modelspec";

  /** Ergebnisordner vor Berechnung loeschen ? ["true"|"false"] */
  public final static String PROP_CLEAR_RESULTS = "clearResultFolder";

  /**
   * Basisname der Zeitreihen-Properties. Es kann mehrere Zeitreihen
   * geben-Property geben: eine f?r jede Kurventyp.
   */
  public final static String PROP_TIMEPROPNAME = "timeserie";

  private LayerTableViewer m_viewer;

  public KalypsoNAWizardPage()
  {
    super( "<MapAndTableWizardPage>" );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    try
    {
      final SashForm sashForm = new SashForm( parent, SWT.HORIZONTAL );
      createMapPanel( sashForm );
      createRightPanel( sashForm );

      setControl( sashForm );

      parent.getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          maximizeMap();
        }
      } );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  private void createRightPanel( final SashForm sashForm ) throws NumberFormatException
  {
    final Composite rightPanel = new Composite( sashForm, SWT.NONE );

    final GridLayout gridLayout = new GridLayout();
    rightPanel.setLayout( gridLayout );
    rightPanel.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    final SashForm rightSash = new SashForm( rightPanel, SWT.VERTICAL );
    rightSash.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    createTablePanel( rightSash );
    createDiagramPanel( rightSash );

    final Button button = new Button( rightPanel, SWT.NONE | SWT.PUSH );
    button.setText( "Berechnung durchführen" );

    final int mainWeight = Integer.parseInt( getArguments().getProperty( PROP_MAINSASH, "50" ) );
    final int rightWeight = Integer.parseInt( getArguments().getProperty( PROP_RIGHTSASH, "50" ) );

    sashForm.setWeights( new int[]
    {
        mainWeight,
        100 - mainWeight } );

    rightSash.setWeights( new int[]
    {
        rightWeight,
        100 - rightWeight } );

    // die Karte soll immer maximiert sein
    rightSash.addControlListener( new ControlAdapter()
    {
      public void controlResized( ControlEvent e )
      {
        maximizeMap();
      }
    } );

    button.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        runCalculation();
      }
    } );
  }

  private void createDiagramPanel( final Composite parent )
  {
    initDiagram( parent );
  }

  private void createTablePanel( final Composite parent )
  {
    initFeatureTable( parent );
  }

  private void createMapPanel( final Composite parent ) throws Exception, CoreException
  {
    final Composite mapPanel = new Composite( parent, SWT.NONE );
    mapPanel.setLayout( new GridLayout() );

    final Control mapControl = initMap( mapPanel, MapPanel.WIDGET_SINGLE_SELECT );
    mapControl.setLayoutData( new GridData( GridData.FILL_BOTH ) );
  }

  /**
   * @see org.kalypso.ui.calcwizard.modelpages.IModelWizardPage#performFinish()
   */
  public boolean performFinish()
  {
    try
    {
      // TODO: error handling?
      m_viewer.saveData( new NullProgressMonitor() );
    }
    catch( CoreException e )
    {
      e.printStackTrace();
    }

    return true;
  }

  protected void runCalculation()
  {
    final LayerTableViewer viewer = m_viewer;

    final WorkspaceModifyOperation op = new WorkspaceModifyOperation( null )
    {
      public void execute( final IProgressMonitor monitor ) throws CoreException
      {
        monitor.beginTask( "Berechnung wird durchgeführt", 2000 );
        viewer.saveData( new SubProgressMonitor( monitor, 1000 ) );

        final ModelNature nature = (ModelNature)getCalcFolder().getProject().getNature(
            ModelNature.ID );
        final String modelspec = getArguments().getProperty( PROP_MODELSPEC, null );
        final String clearResults = getArguments().getProperty( PROP_CLEAR_RESULTS, "true" );
        boolean doClearResults=true;
        if("false".equals(clearResults))
          doClearResults=false;
        nature.runCalculation( getCalcFolder(), new SubProgressMonitor( monitor, 1000 ),modelspec,doClearResults);
      }
    };

    try
    {
      getContainer().run( true, true, op );
    }
    catch( final InterruptedException e )
    {
      // canceled
      // TODO error message?
      return;
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();

      final Throwable te = e.getTargetException();
      if( te instanceof CoreException )
      {
        ErrorDialog.openError( getContainer().getShell(), "Fehler",
            "Fehler beim Aufruf der nächsten Wizard-Seite",
            ( (CoreException)e.getTargetException() ).getStatus() );
      }
      else
      {
        // CoreExceptions are handled above, but unexpected runtime exceptions
        // and errors may still occur.
        MessageDialog.openError( getContainer().getShell(), "Interner Fehler",
            "Fehler beim Aufruf der nächsten Wizard-Seite: " + te.getLocalizedMessage() );
      }
    }

    onModellChange( new ModellEvent( null, ModellEvent.SELECTION_CHANGED ) );
  }

  /**
   * @see org.kalypso.ui.calcwizard.modelpages.AbstractCalcWizardPage#getObservationsToShow()
   */
  protected TSLinkWithName[] getObservationsToShow()
  {
    return getObservationsFromMap();
  }
}