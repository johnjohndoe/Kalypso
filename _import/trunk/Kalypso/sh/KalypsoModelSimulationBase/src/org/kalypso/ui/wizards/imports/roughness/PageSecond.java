package org.kalypso.ui.wizards.imports.roughness;

import java.util.ArrayList;
import java.util.LinkedHashMap;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.ScrollBar;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;

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
    GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( m_data.getRoughnessDatabaseLocationURL(), null );
    IRoughnessClsCollection collection = new RoughnessClsCollection( workspace.getRootFeature() );
    for( int i = 0; i < collection.size(); i++ )
      m_data.getRoughnessStaticCollectionMap().put( collection.get( i ).getName(), collection.get( i ).getGmlID() );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    ScrolledComposite scrolledComposite = new ScrolledComposite( parent, SWT.BORDER | SWT.V_SCROLL );
    Composite composite = new Composite(scrolledComposite, SWT.NONE);
    scrolledComposite.setContent( composite );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 2;
    gridLayout.marginWidth = 20;
    gridLayout.marginHeight = 10;
    gridLayout.verticalSpacing = 10;
    gridLayout.horizontalSpacing = 40;
    composite.setLayout( gridLayout );
    setControl( composite );
    setPageComplete( false );
  }

  public void delayedCreateControl()
  {
    String[] names = new String[m_data.getRoughnessStaticCollectionMap().size()];
    int i = 0;
    for( String key : m_data.getRoughnessStaticCollectionMap().keySet() )
      names[i++] = key;
    Composite composite = (Composite) getControl();
    LinkedHashMap<String, String> map = m_data.getRoughnessShapeStaticRelationMap();
    m_shpNamesList = new ArrayList<String>();
    m_comboRoughnessIDs = new Combo[map.size()];
    for( String key : map.keySet() )
    {
      if( !m_shpNamesList.contains( map.get( key ) ) )
      {
        GridData gridDataID = new GridData();
        int nextEntryNr = m_shpNamesList.size(); 
        m_shpNamesList.add( map.get( key ) );
        new Label( composite, SWT.NONE ).setText( map.get( key ) ); //$NON-NLS-1$
        m_comboRoughnessIDs[nextEntryNr] = new Combo( composite, SWT.READ_ONLY );
        m_comboRoughnessIDs[nextEntryNr].setLayoutData( gridDataID );
        m_comboRoughnessIDs[nextEntryNr].setItems( names );
      }
    }
    Point pt = composite.computeSize(SWT.DEFAULT, SWT.DEFAULT);
    composite.setSize(pt);
    composite.layout();
//    composite.getParent().layout();
    setPageComplete( true );
  }
  
  /**
   * @see org.eclipse.jface.wizard.WizardPage#isPageComplete()
   */
  @Override
  public boolean isPageComplete( )
  {
//    boolean complete = true;
//    for(int i=0; i<m_comboRoughnessIDs.length; i++)
//      complete &= m_comboRoughnessIDs[i].getText().length() > 0;
//    return complete;
    return super.isPageComplete();
  }
  
  protected void saveDataToModel( )
  {
    if( isCurrentPage() )
    {
      LinkedHashMap<String, String> map = m_data.getRoughnessShapeStaticRelationMap();
      String shpName, dbName;
      for( String shpID : map.keySet() )
      {
        shpName = map.get( shpID );
        dbName = m_comboRoughnessIDs[m_shpNamesList.indexOf( shpName )].getText();
        map.put( shpID, m_data.getRoughnessStaticCollectionMap().get( dbName ) );
      }
    }
  }
}