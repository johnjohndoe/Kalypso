package org.kalypso.ui.wizards.imports.roughness;

import java.util.ArrayList;
import java.util.LinkedHashMap;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.kalypso.ui.wizards.i18n.Messages;


public class PageSecond extends WizardPage
{
  private final DataContainer m_data;

  private Combo[] m_comboRoughnessIDs;

  private ArrayList<String> m_shpNamesList;

  protected PageSecond( final DataContainer data ) throws Exception
  {
    super( Messages.getString("org.kalypso.ui.wizards.imports.roughness.PageSecond.0") ); //$NON-NLS-1$
    setTitle( Messages.getString( "org.kalypso.ui.wizards.imports.roughness.PageSecond.Title" ) );//$NON-NLS-1$
    setDescription( Messages.getString( "org.kalypso.ui.wizards.imports.roughness.PageSecond.Description" ) );//$NON-NLS-1$
    m_data = data;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    final ScrolledComposite scrolledComposite = new ScrolledComposite( parent, SWT.BORDER | SWT.V_SCROLL );
    final Composite composite = new Composite( scrolledComposite, SWT.NONE );
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

  public void delayedCreateControl( )
  {
    final Composite composite = (Composite) getControl();
    if( m_comboRoughnessIDs != null && m_comboRoughnessIDs.length > 0 )
    {
      final Control[] controls = composite.getChildren();
      for( int i = 0; i < controls.length; i++ )
        controls[i].dispose();
    }

    /* introduce some headers */
    new Label( composite, SWT.BOLD ).setText( "importierte Klasse" ); //$NON-NLS-1$
    new Label( composite, SWT.BOLD ).setText( "zugewiesene Klasse" ); //$NON-NLS-1$

    final String[] names = new String[m_data.getRoughnessStaticCollectionMap().size()];
    int i = 0;
    for( final String key : m_data.getRoughnessStaticCollectionMap().keySet() )
      names[i++] = key;
    final LinkedHashMap<String, String> map = m_data.getRoughnessShapeStaticRelationMap();
    m_shpNamesList = new ArrayList<String>();
    m_comboRoughnessIDs = new Combo[map.size()];
    for( final String key : map.keySet() )
    {
      if( !m_shpNamesList.contains( map.get( key ) ) )
      {
        final GridData gridDataID = new GridData();
        final int nextEntryNr = m_shpNamesList.size();
        m_shpNamesList.add( map.get( key ) );
        new Label( composite, SWT.NONE ).setText( map.get( key ) ); //$NON-NLS-1$
        m_comboRoughnessIDs[nextEntryNr] = new Combo( composite, SWT.READ_ONLY );
        m_comboRoughnessIDs[nextEntryNr].setLayoutData( gridDataID );
        m_comboRoughnessIDs[nextEntryNr].setItems( names );
        m_comboRoughnessIDs[nextEntryNr].select( getSelectionIndex( names, map.get( key ) ) );

        m_comboRoughnessIDs[nextEntryNr].setToolTipText( Messages.getString("org.kalypso.ui.wizards.imports.roughness.PageSecond.1") ); //$NON-NLS-1$
      }
    }
    composite.layout();
    composite.pack();
    final Point pt = composite.computeSize( SWT.DEFAULT, SWT.DEFAULT );

    int minimumSizeX = pt.x + 30;
    if( minimumSizeX < 400 )
      minimumSizeX = 400;
    composite.getShell().setSize( minimumSizeX, composite.getShell().getSize().y );
    composite.getShell().setMinimumSize( minimumSizeX, composite.getShell().getSize().y );

    // composite.getParent().layout();
    setPageComplete( true );
  }

  private int getSelectionIndex( final String[] array, final String key )
  {
    final String userSelected = m_data.getUserSelectionMap().get( key );
    for( int i = 0; i < array.length; i++ )
      if( array[i].equals( userSelected ) )
        return i;
    return -1;
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#isPageComplete()
   */
  @Override
  public boolean isPageComplete( )
  {
    // boolean complete = true;
    // for(int i=0; i<m_comboRoughnessIDs.length; i++)
    // complete &= m_comboRoughnessIDs[i].getText().length() > 0;
    // return complete;
    return super.isPageComplete();
  }

  protected void saveDataToModel( )
  {
    if( isCurrentPage() )
    {
      final LinkedHashMap<String, String> map = m_data.getRoughnessShapeStaticRelationMap();
      String shpName, dbName;
      for( final String shpID : map.keySet() )
      {
        shpName = map.get( shpID );
        dbName = m_comboRoughnessIDs[m_shpNamesList.indexOf( shpName )].getText();
        map.put( shpID, m_data.getRoughnessStaticCollectionMap().get( dbName ) );
      }
      final LinkedHashMap<String, String> userSelectionMap = m_data.getUserSelectionMap();
      for( int i = 0; i < m_shpNamesList.size(); i++ )
      {
        final String text = m_comboRoughnessIDs[i].getText();
        if( text != null && text.trim() != "" ) //$NON-NLS-1$
          userSelectionMap.put( m_shpNamesList.get( i ), text );
      }
    }
  }
}