package org.kalypso.ui.wizards.imports.roughness;

import java.util.ArrayList;
import java.util.HashMap;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import test.org.kalypso.kalypsosimulationmodel.TestWorkspaces;

public class PageSecond extends WizardPage
{
  private DataContainer m_data;

  private Combo[] m_comboRoughnessIDs;

  private ArrayList<String> m_shpNamesList;

  protected PageSecond( DataContainer data ) throws Exception
  {
    super( "Select Files and Relate" );
    setTitle( "Select Files" );
    setDescription( "Select the source and Destination Files" );
    m_data = data;
    GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( TestWorkspaces.URL_ROUGHNESS_CLS_COLLECTION_VIEW_TEST, null );
    IRoughnessClsCollection collection = new RoughnessClsCollection( workspace.getRootFeature() );
    for( int i = 0; i < collection.size(); i++ )
      m_data.getRoughnessStaticCollectionMap().put( collection.get( i ).getName(), collection.get( i ).getGmlID() );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    Composite composite = new Composite( parent, SWT.NULL );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 2;
    gridLayout.marginWidth = 40;
    gridLayout.marginHeight = 50;
    gridLayout.verticalSpacing = 10;
    gridLayout.horizontalSpacing = 40;
    composite.setLayout( gridLayout );
    composite.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL ) );
    setControl( composite );
    setPageComplete( false );
  }

  public void delayedCreateControl( )
  {
    String[] names = new String[m_data.getRoughnessStaticCollectionMap().size()];
    int i = 0;
    for( String key : m_data.getRoughnessStaticCollectionMap().keySet() )
      names[i++] = key;
    Composite composite = (Composite) getControl();
    HashMap<String, String> map = m_data.getRoughnessShapeStaticRelationMap();
    m_shpNamesList = new ArrayList<String>();
    m_comboRoughnessIDs = new Combo[map.size()];
    GridData gridDataID = new GridData();
    for( String key : map.keySet() )
    {
      if( !m_shpNamesList.contains( map.get( key ) ) )
      {
        m_shpNamesList.add( map.get( key ) );
        new Label( composite, SWT.NONE ).setText( map.get( key ) ); //$NON-NLS-1$
        m_comboRoughnessIDs[m_shpNamesList.size() - 1] = new Combo( composite, SWT.READ_ONLY );
        m_comboRoughnessIDs[m_shpNamesList.size() - 1].setLayoutData( gridDataID );
        m_comboRoughnessIDs[m_shpNamesList.size() - 1].setItems(names);
        // comboRoughnessIDs[m_shpNamesList.size() - 1].setText( "Selection" );
      }
    }
    composite.layout();
    setPageComplete( true );
  }

  protected void saveDataToModel( )
  {
    if( isCurrentPage() )
    {
      HashMap<String, String> map = m_data.getRoughnessShapeStaticRelationMap();
      String shpName, dbName;
      for( String shpID : map.keySet() )
      {
        shpName = map.get( shpID );
        dbName = m_comboRoughnessIDs[ m_shpNamesList.indexOf( shpName ) ].getText();
        map.put( shpID, m_data.getRoughnessStaticCollectionMap().get( dbName ) );
      }
    }
  }
}