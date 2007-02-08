package org.kalypso.ui.shapeImportWizards.utils.importRoughness;

import java.util.List;
import java.util.HashMap;
import java.util.Iterator;
import java.util.ListIterator;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import test.org.kalypso.kalypsosimulationmodel.TestWorkspaces;

public class PageSecond extends WizardPage
{
  private HashMap<String, String> roughnessID = new HashMap<String, String>();

  List<String> roughnessList = null;

  protected PageSecond( )
  {
    super( "Select Files and Relate" );
    setTitle( "Select Files" );
    setDescription( "Select the source and Destination Files" );
    // TODO Auto-generated constructor stub
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    Composite container = new Composite( parent, SWT.NULL );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 2;
    gridLayout.numColumns = 2;
    gridLayout.marginWidth = 40;
    gridLayout.marginHeight = 50;
    gridLayout.verticalSpacing = 10;
    gridLayout.horizontalSpacing = 40;
    container.setLayout( gridLayout );
    setControl( container );

    // Test Data
    roughnessID.put( "SOme", "" );
    roughnessID.put( "Things", "" );
    roughnessID.put( "Which", "" );
    roughnessID.put( "For", "" );

    Label stg = null;
    GridData gridData = null;
    GridData gridDataID = null;
    int i = 0;
    final Combo[] comboRoughnessIDs = new Combo[roughnessID.size()];

    for( String key : roughnessID.keySet() )
    {

      // Key Contents Comes here. 
      stg = new Label( container, SWT.NONE );
      gridData = new GridData();
      stg.setLayoutData( gridData );
      stg.setText( key );

      // Combo Boxes Displaying the Roughness IDs.
      comboRoughnessIDs[i] = new Combo( container, SWT.READ_ONLY );
      gridDataID = new GridData();

      comboRoughnessIDs[i].setLayoutData( gridDataID );
      comboRoughnessIDs[i].setItems( getNameFromFeatureList( getGMLIterator() ) );
      comboRoughnessIDs[i].setText( "Selection" );

      comboRoughnessIDs[i].addSelectionListener( new SelectionAdapter()
      {
        @Override
        public void widgetSelected( SelectionEvent e )
        {
          System.out.println( ((Combo) e.widget).getText() );
          roughnessList.add(((Combo) e.widget).getText() );

        }
      } );
      i++;

    }
    
    List<String> lBack = null;
    
    for (String key: roughnessID.keySet()){
      lBack.add(key);
    }
    roughnessID.clear();
    for (int i1 = 0; i1<roughnessList.size(); i1++){
    roughnessID.put(lBack.get( i1 ),roughnessList.get( i1 ).toString());  
    }
 }

  private static final IRoughnessClsCollection getGMLIterator( )
  {
    GMLWorkspace workspace = null;
    try
    {
      workspace = GmlSerializer.createGMLWorkspace( TestWorkspaces.URL_ROUGHNESS_CLS_COLLECTION_VIEW_TEST, null );
      IRoughnessClsCollection col = new RoughnessClsCollection( workspace.getRootFeature() );
      return col;
    }
    catch( Throwable th )
    {
      return null;
    }
  }

  private static final String[] getNameFromFeatureList( IRoughnessClsCollection roughnessClsCollection )
  {
    final int SIZE = roughnessClsCollection.size();
    String[] names = new String[SIZE];
    for( int i = 0; i < SIZE; i++ )
    {
      names[i] = roughnessClsCollection.get( i ).getName();
    }
    return names;
  }
}