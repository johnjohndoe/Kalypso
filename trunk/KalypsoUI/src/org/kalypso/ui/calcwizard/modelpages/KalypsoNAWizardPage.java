package org.kalypso.ui.calcwizard.modelpages;

import org.deegree.model.feature.event.ModellEventListener;
import org.eclipse.core.runtime.CoreException;
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
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.ogc.gml.util.GisTemplateLoadedThread;

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

  /**
   * Basisname der Zeitreihen-Properties. Es kann mehrere Zeitreihen
   * geben-Property geben: eine für jede Kurventyp.
   */
  public final static String PROP_TIMEPROPNAME = "timeserie";

  public KalypsoNAWizardPage()
  {
    super( "<MapAndTableWizardPage>" );
  }

  public void dispose()
  {
    LayerTableViewer layerTable = getLayerTable();
    if( layerTable != null )
      layerTable.removeModellListener( this );
    super.dispose();
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

      postCreateControl();
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

    createButtonPanel( rightPanel );

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
  }

  private void createButtonPanel( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout( 2, false ) );
    
    final Button button = new Button( panel, SWT.NONE | SWT.PUSH );
    button.setText( "Berechnung durchführen" );
    
    button.addSelectionListener( new SelectionAdapter()
        {
          public void widgetSelected( SelectionEvent e )
          {
            runCalculation();
          }
        } );
    
    createIgnoreButtonPanel( panel );
  }

  private void createDiagramPanel( final Composite parent )
  {
    initDiagram( parent );
  }

  private void createTablePanel( final Composite parent )
  {
    initFeatureTable( parent );
    LayerTableViewer layerTable = getLayerTable();
    layerTable.addModellListener( this );
  }

  private void createMapPanel( final Composite parent ) throws Exception, CoreException
  {
    final Composite mapPanel = new Composite( parent, SWT.NONE );
    mapPanel.setLayout( new GridLayout() );

    final Control mapControl = initMap( mapPanel, MapPanel.WIDGET_SINGLE_SELECT );
    mapControl.setLayoutData( new GridData( GridData.FILL_BOTH ) );
  }

  /**
   * @see org.kalypso.ui.calcwizard.modelpages.AbstractCalcWizardPage#getObservationsToShow(boolean)
   */
  protected TSLinkWithName[] getObservationsToShow( final boolean onlySelected )
  {
    return getObservationsFromMap( true, onlySelected );
  }
  
  protected void postCreateControl()
  {
    new GisTemplateLoadedThread( m_mapModell, new Runnable()
    {
      public void run()
      {
//        // erstes feature des aktiven themas selektieren
//        final IKalypsoTheme activeTheme = m_mapModell.getActiveTheme();
//        if( activeTheme instanceof IKalypsoFeatureTheme )
//        {
//          final IKalypsoFeatureTheme kft = (IKalypsoFeatureTheme)activeTheme;
//          final GMLWorkspace workspace = kft.getWorkspace();
//
//          final FeatureList featureList = kft.getFeatureList();
//          if( featureList != null && featureList.size() != 0 )
//          {
//            featureList.accept( new UnselectFeatureVisitor( getSelectionID() ) );
//
//            final String fid = getArguments().getProperty( PROP_FEATURE_TO_SELECT_ID, null );
//
//            final Feature feature = fid == null ? (Feature)featureList.get( 0 ) : workspace.getFeature(fid); 
//            if( feature != null )
//              feature.select( getSelectionID() );
//          }
//        }
        
        // TODO: das eine Feature in der Tabelle selektieren und im diagramm anzeigen!

        refreshDiagram();
        refreshZMLTable();
      }
    } ).start();
  }

}