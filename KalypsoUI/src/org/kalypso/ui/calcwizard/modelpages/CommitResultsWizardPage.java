package org.kalypso.ui.calcwizard.modelpages;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureVisitor;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;

/**
 * @author Belger
 */

public class CommitResultsWizardPage extends AbstractCalcWizardPage implements ModellEventListener
{
  // Beispiel:
  //   <page className="org.kalypso.ui.calcwizard.CommitResultsWizardPage"
  // pageTitle="Kontrolle der Ergebnisse"
  // imageLocation="icons/calcwizard/boden.gif" >
  //        <arg name="mapTemplate" value=".modellTyp/vorlagen/rechenfall/karte2.gmt"/>
  //        <arg name="tableTemplate"
  // value=".modellTyp/vorlagen/rechenfall/table2.gtt"/>
  //        <arg name="timeseriesPropertyNames"
  // value="Wasserstand#Wasserstand_gerechnet"/>
  //        <arg name="mainSash" value="50"/>
  //        <arg name="rightSash" value="40"/>
  //        <arg name="CommitTextTemplate" value="Vorhersageergebnis Spreemodell
  // berechnet mit Kalypso"/>
  //    </page>
  //  

  /** initialer Text f?r die Ergebnisablage */
  public final static String PROP_COMMITTEXTTEMPLATE = "CommitTextTemplate";

  /** Position des Haupt-Sash: Integer von 0 bis 100 */
  public final static String PROP_MAINSASH = "mainSash";

  /** Position des rechten Sash: Integer von 0 bis 100 */
  public final static String PROP_RIGHTSASH = "rightSash";

  /**
   * Property-Names zum Layer in der Tabelle: alle Zeitreihen dieser Spalten
   * werden im diagram angezeigt
   */
  public final static String PROP_TIMEPROPNAME = "timeseriesPropertyNames";

  public CommitResultsWizardPage()
  {
    super( "<CommitResultsWizardPage>" );
  }

  public CommitResultsWizardPage( String title )
  {
    super( title );
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
      final SashForm rightSash = new SashForm( sashForm, SWT.VERTICAL );
      createTablePanel( rightSash );
      createCommitTextPanel( rightSash );
      createCommitButton( rightSash );

      final int mainWeight = Integer.parseInt( getArguments().getProperty( PROP_MAINSASH, "50" ) );
      final int rightWeight0 = Integer.parseInt( getArguments().getProperty( PROP_RIGHTSASH + "0",
          "50" ) );
      final int rightWeight1 = Integer.parseInt( getArguments().getProperty( PROP_RIGHTSASH + "1",
          "20" ) );

      // TODO: konfigure
      sashForm.setWeights( new int[]
      {
          mainWeight,
          100 - mainWeight } );

      rightSash.setWeights( new int[]
      {
          rightWeight0,
          rightWeight0 + rightWeight1,
          100 - rightWeight0 - rightWeight1 } );

      setControl( sashForm );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  private void createCommitButton( final Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.RIGHT );

    final Button button = new Button( composite, SWT.NONE | SWT.PUSH );
    button.setText( "ausgewählte Pegel in Ergebnisablage speichern" );
    button.addSelectionListener( new CommitResults() );
    button.setVisible( true );
    composite.setVisible( true );
    //button.setEnabled(true);
  }

  private void createCommitTextPanel( final Composite parent )
  {

    final Text text = new Text( parent, SWT.MULTI );
    text.setVisible( true );
    text.setText( getArguments().getProperty( PROP_COMMITTEXTTEMPLATE ) );

  }

  private void createTablePanel( final Composite parent )
  {
    initDiagramTable( parent );
  }

  private void createMapPanel( final Composite parent ) throws Exception, CoreException
  {
    initMap(parent, MapPanel.WIDGET_TOGGLE_SELECT );
  }

  /**
   * @see org.kalypso.ui.calcwizard.modelpages.IModelWizardPage#performFinish()
   */
  public boolean performFinish()
  {
    return true;
  }

  /**
   * @see org.deegree.model.feature.event.ModellEventListener#onModellChange(org.deegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
  //
  }

  private class CommitResults extends SelectionAdapter
  {
    /**
     * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
     */
    public void widgetSelected( SelectionEvent e )
    {
      System.out.println( "export Grafics" );
      final String propNames = getArguments().getProperty( PROP_TIMEPROPNAME, "" );
      final String[] timeNames = propNames.split( "#" );

      final IKalypsoTheme theme = getMapModell().getActiveTheme();
      if( !( theme instanceof IKalypsoFeatureTheme ) )
        return;

      final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme)theme;
      final GMLWorkspace workspace = featureTheme.getWorkspace();

      try
      {
        workspace.accept( new FeatureVisitor()
        {
          public boolean visit( final Feature f )
          {
            if( f.isSelected( SELECTION_ID ) )
            {
              for( int j = 0; j < timeNames.length; j++ )
              {
                Object observation = f.getProperty( timeNames[j] );
                if( observation == null )
                  System.out.println( "observation is null" );
                else
                  System.out.println( "observation is type:" + observation.getClass().toString() );
              }
            }
            
            return true;
          }
        }, featureTheme.getFeatureType(), FeatureVisitor.DEPTH_ZERO );
      }
      catch( final Throwable e1 )
      {
        e1.printStackTrace();
      }

    }
  }

  /**
   * @see org.kalypso.ui.calcwizard.modelpages.AbstractCalcWizardPage#getObservationsToShow()
   */
  protected TSLinkWithName[] getObservationsToShow()
  {
    return getObservationsFromMap();
  }
}