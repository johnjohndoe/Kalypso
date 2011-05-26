/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.km.internal.ui.kmupdate;

import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.km.internal.binding.KMBindingUtils;
import org.kalypso.model.km.internal.core.AbstractProfileDataSet;
import org.kalypso.model.km.internal.core.ProfileData;
import org.kalypso.model.km.internal.core.ProfileFactory;
import org.kalypso.model.km.internal.i18n.Messages;

import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType;
import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType.Profile;

/**
 * @author Andreas Doemming (original)
 * @author Holger Albert (modified)
 */
public class KMViewer
{
  /**
   * The text field for the label.
   */
  private Text m_labelText;

  /**
   * The widget for selecting a file.
   */
  private DirectoryFieldWidget m_dirField;

  /**
   * The text for the start km.
   */
  private Text m_startText;

  /**
   * The text for the end km.
   */
  private Text m_endText;

  /**
   * The checkbox table viewer, which displays the profiles.
   */
  private CheckboxTableViewer m_profileListViewer;

  /**
   * The abstract profile data set.
   */
  private AbstractProfileDataSet m_profileSet;

  /**
   * The input.
   */
  private KalininMiljukovType m_input;

  /**
   * The constructor.
   */
  public KMViewer( )
  {
    m_labelText = null;
    m_dirField = null;
    m_startText = null;
    m_endText = null;
    m_profileListViewer = null;
    m_profileSet = null;
    m_input = null;
  }

  /**
   * This function creates the controls.
   * 
   * @param parent
   *          The parent composite.
   */
  public void createControls( Composite parent )
  {
    /* Set a layout to the parent. */
    parent.setLayout( new GridLayout( 3, false ) );

    /* Create a empty label. */
    Label emptyLabel = new Label( parent, SWT.NONE );
    emptyLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );

    /* Create a text. */
    m_labelText = new Text( parent, SWT.BORDER | SWT.READ_ONLY );
    m_labelText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* Create a empty label. */
    Label emptyLabel1 = new Label( parent, SWT.NONE );
    emptyLabel1.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );

    /* Create a widget for asking for a file. */
    m_dirField = new DirectoryFieldWidget( parent, Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMViewer.1" ), Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMViewer.0" ), 1, 1, 1 ); //$NON-NLS-1$ //$NON-NLS-2$
    m_dirField.addSelectionChangedListener( new ISelectionChangedListener()
    {
      /**
       * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
       */
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleDirectoryFieldSelected( (IStructuredSelection) event.getSelection() );
      }
    } );

    /* Create a label. */
    Label startLabel = new Label( parent, SWT.NONE );
    startLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );
    startLabel.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMViewer.2" ) ); //$NON-NLS-1$

    /* Create a text. */
    m_startText = new Text( parent, SWT.BORDER );
    m_startText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* Create a empty label. */
    Label emptyLabel2 = new Label( parent, SWT.NONE );
    emptyLabel2.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );

    /* Create a label. */
    Label endLabel = new Label( parent, SWT.NONE );
    endLabel.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMViewer.3" ) ); //$NON-NLS-1$
    endLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );

    /* Create a text. */
    m_endText = new Text( parent, SWT.BORDER );
    m_endText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* Create a empty label. */
    Label emptyLabel3 = new Label( parent, SWT.NONE );
    emptyLabel3.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );

    /* Create a label. */
    Label profilesLabel = new Label( parent, SWT.NONE );
    profilesLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.BEGINNING, false, false ) );
    profilesLabel.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMViewer.4" ) ); //$NON-NLS-1$

    /* Create a checkbox table viewer. */
    m_profileListViewer = CheckboxTableViewer.newCheckList( parent, SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER );
    m_profileListViewer.getTable().setHeaderVisible( true );
    m_profileListViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 ) );
    m_profileListViewer.setContentProvider( new KMViewerContentProvider() );
    m_profileListViewer.setCheckStateProvider( new KMViewerCheckStateProvider( this ) );

    /* Create a table viewer column. */
    // TableViewerColumn labelColumn = new TableViewerColumn( m_profileListViewer, SWT.LEFT );
    // labelColumn.setLabelProvider( new ProfileNameLabelProvider() );
    //    labelColumn.getColumn().setText( Messages.getString( "KMViewer_0" ) ); //$NON-NLS-1$
    // labelColumn.getColumn().setWidth( 100 );
    // ColumnViewerSorter.registerSorter( labelColumn, new ProfileNameSorter() );

    /* Create a table viewer column. */
    TableViewerColumn stationColumn = new TableViewerColumn( m_profileListViewer, SWT.LEFT );
    stationColumn.setLabelProvider( new ProfileStationLabelProvider() );
    stationColumn.getColumn().setText( Messages.getString( "KMViewer_1" ) ); //$NON-NLS-1$
    stationColumn.getColumn().setWidth( 100 );
    ColumnViewerSorter.registerSorter( stationColumn, new ProfileStationSorter() );

    /* Create a table viewer column. */
    TableViewerColumn validColumn = new TableViewerColumn( m_profileListViewer, SWT.LEFT );
    validColumn.setLabelProvider( new ProfileValidLabelProvider( this ) );
    validColumn.getColumn().setText( Messages.getString( "KMViewer_2" ) ); //$NON-NLS-1$
    validColumn.getColumn().setWidth( 200 );

    /* Make sure, the columns are properly resized. */
    m_profileListViewer.getTable().addControlListener( new ColumnsResizeControlListener() );

    /* Add a listener. */
    m_startText.addFocusListener( new FocusAdapter()
    {
      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {
        handleKMStartModified();
      }
    } );

    /* Add a listener. */
    m_endText.addFocusListener( new FocusAdapter()
    {
      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {
        handleKMEndModified();
      }
    } );
  }

  protected void handleKMStartModified( )
  {
    try
    {
      KalininMiljukovType input = getInput();
      if( input != null )
      {
        double km = NumberUtils.parseQuietDouble( m_startText.getText() );
        if( Double.isNaN( km ) )
          input.setKmStart( null );
        else
          input.setKmStart( km );
      }

      updateProfileList();
      updateControls();
    }
    catch( Exception ex )
    {
      ex.printStackTrace();
    }
  }

  protected void handleKMEndModified( )
  {
    try
    {
      KalininMiljukovType input = getInput();
      if( input != null )
      {
        double km = NumberUtils.parseQuietDouble( m_endText.getText() );
        if( Double.isNaN( km ) )
          input.setKmEnd( null );
        else
          input.setKmEnd( km );
      }

      updateProfileList();
      updateControls();
    }
    catch( Exception ex )
    {
      ex.printStackTrace();
    }
  }

  protected void handleDirectoryFieldSelected( IStructuredSelection selection )
  {
    String path = (String) selection.getFirstElement();

    KalininMiljukovType input = getInput();
    if( input != null )
      input.setPath( path );

    readProfileSet( path );

    if( m_profileSet != null && input != null )
    {
      Double oldKmStart = input.getKmStart();
      if( oldKmStart == null )
      {
        double startPosition = m_profileSet.getStartPosition() / 1000d;
        input.setKmStart( startPosition );
      }

      Double oldKmEnd = input.getKmEnd();
      if( oldKmEnd == null )
      {
        double endPosition = m_profileSet.getEndPosition() / 1000d;
        input.setKmEnd( endPosition );
      }
    }

    updateProfileList();
    updateControls();
  }

  private KalininMiljukovType getInput( )
  {
    return m_input;
  }

  private void updateProfileList( )
  {
    KalininMiljukovType input = getInput();
    if( input == null )
      return;

    List<Profile> profileList = input.getProfile();
    profileList.clear();

    if( m_profileSet == null )
      return;

    Double kmStart = input.getKmStart();
    Double kmEnd = input.getKmEnd();

    ProfileData[] allProfiles = m_profileSet.getAllProfiles();
    for( ProfileData pd : allProfiles )
    {
      String file = pd.getFile();
      double position = pd.getPosition();
      double station = position / 1000.0;

      if( (kmStart == null || kmStart <= station) && (kmEnd == null || station <= kmEnd) )
      {
        Profile profileData = KMBindingUtils.OF.createKalininMiljukovTypeProfile();
        profileData.setFile( file );
        profileData.setPositionKM( position );
        profileData.setEnabled( pd.isValidForKalypso() == null );
        profileList.add( profileData );
      }
    }
  }

  private void updateControls( )
  {
    KalininMiljukovType input = getInput();
    if( input == null )
    {
      // TODO: title + label
      m_labelText.setText( Messages.getString( "KMViewer_3" ) ); //$NON-NLS-1$
      m_dirField.setSelection( StructuredSelection.EMPTY );
      m_dirField.setEnabled( false );
      m_startText.setText( "" ); //$NON-NLS-1$
      m_startText.setEnabled( false );
      m_endText.setText( "" ); //$NON-NLS-1$
      m_endText.setEnabled( false );
      m_profileListViewer.getControl().setEnabled( false );
    }
    else
    {
      Double kmStart = input.getKmStart();
      Double kmEnd = input.getKmEnd();
      String path = input.getPath();

      m_dirField.setSelection( new StructuredSelection( path ) );

      if( kmStart != null )
        m_startText.setText( String.format( "%.4f", kmStart ) ); //$NON-NLS-1$
      else
        m_startText.setText( "" ); //$NON-NLS-1$

      if( kmEnd != null )
        m_endText.setText( String.format( "%.4f", kmEnd ) ); //$NON-NLS-1$
      else
        m_endText.setText( "" ); //$NON-NLS-1$

      m_dirField.setEnabled( true );
      m_startText.setEnabled( !StringUtils.isBlank( path ) );
      m_endText.setEnabled( !StringUtils.isBlank( path ) );
      m_profileListViewer.getControl().setEnabled( !StringUtils.isBlank( path ) );
    }

    m_profileListViewer.refresh();
    TableColumn[] columns = m_profileListViewer.getTable().getColumns();
    for( TableColumn column : columns )
      column.pack();
  }

  private void readProfileSet( String path )
  {
    if( !StringUtils.isBlank( path ) )
    {
      m_profileSet = ProfileFactory.createProfileObservationSet( new Path( path ), Double.NaN, Double.NaN );

      return;
    }

    m_profileSet = null;
  }

  private void inputChanged( final KalininMiljukovType oldInput, final KalininMiljukovType kmType )
  {
    // TODO: Check this stuff here...
    if( oldInput != null )
    {
      /* Keep checked elements. */
      List<Profile> profiles = oldInput.getProfile();
      for( Profile profile2 : profiles )
        profile2.setEnabled( false );

      Object[] checkedElements = m_profileListViewer.getCheckedElements();
      for( Object object : checkedElements )
      {
        for( Profile profile : profiles )
        {
          if( object == profile )
            profile.setEnabled( true );
        }
      }
    }

    m_input = kmType;

    if( m_profileListViewer != null )
      m_profileListViewer.setInput( kmType );

    String path = m_input == null ? null : m_input.getPath();
    readProfileSet( path );
    updateControls();
  }

  private ProfileData findData( final Profile profile )
  {
    if( m_profileSet == null )
      return null;

    String file = profile.getFile();

    ProfileData[] data = m_profileSet.getAllProfiles();
    for( ProfileData profileData : data )
    {
      if( file.equals( profileData.getFile() ) )
        return profileData;
    }

    return null;
  }

  public void setInput( String label, KalininMiljukovType km )
  {
    m_labelText.setText( label );

    KalininMiljukovType oldInput = getInput();

    inputChanged( oldInput, km );
  }

  public String getValidMessage( Profile profile )
  {
    ProfileData data = findData( profile );
    if( data == null )
      return StringUtils.EMPTY;

    String valid = data.isValidForKalypso();
    if( valid == null )
      return Messages.getString( "org.kalypso.model.km.ProfileData.10" ); //$NON-NLS-1$

    return valid;
  }

  public boolean isValid( Profile profile )
  {
    ProfileData data = findData( profile );
    if( data == null )
      return false;

    return data.isValidForKalypso() == null;
  }
}