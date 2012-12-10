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

import java.io.File;
import java.util.List;

import org.apache.commons.lang.StringUtils;
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
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.km.internal.binding.KMBindingUtils;
import org.kalypso.model.km.internal.core.ProfileData;
import org.kalypso.model.km.internal.core.ProfileDataSet;
import org.kalypso.model.km.internal.core.ProfileFactory;
import org.kalypso.model.km.internal.i18n.Messages;

import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType;
import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType.Profile;

/**
 * @author doemming
 */
public class KMViewer
{
  private final CheckboxTableViewer m_profileListViewer;

  private final Text m_textKmStart;

  private final Text m_textKmEnd;

  private final DirectoryFieldWidget m_dirField;

  private final Text m_textLabel;

  private ProfileDataSet m_profileSet;

  private KalininMiljukovType m_input;

  public KMViewer( final Composite parent )
  {
    parent.setLayout( new GridLayout( 3, false ) );

    // Label
    new Label( parent, SWT.NONE );
    m_textLabel = new Text( parent, SWT.BORDER | SWT.READ_ONLY );
    m_textLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    new Label( parent, SWT.NONE );

    final String toolTip = Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMViewer.0" ); //$NON-NLS-1$
    m_dirField = new DirectoryFieldWidget( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMViewer.1" ), toolTip, true, parent, 1, 1, 1 ); //$NON-NLS-1$
    m_dirField.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        handleDirectoryFieldSelected( selection );
      }
    } );

    // Start
    final Label labelKmStart = new Label( parent, SWT.NONE );
    labelKmStart.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMViewer.2" ) ); //$NON-NLS-1$
    m_textKmStart = new Text( parent, SWT.BORDER );
    m_textKmStart.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    new Label( parent, SWT.NONE );

    // END
    final Label labelKmEnd = new Label( parent, SWT.NONE );
    labelKmEnd.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMViewer.3" ) ); //$NON-NLS-1$
    m_textKmEnd = new Text( parent, SWT.BORDER );
    m_textKmEnd.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    new Label( parent, SWT.NONE );

    // Profiles
    final Label labelProfiles = new Label( parent, SWT.NONE );
    labelProfiles.setLayoutData( new GridData( SWT.BEGINNING, SWT.BEGINNING, false, false ) );
    labelProfiles.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMViewer.4" ) ); //$NON-NLS-1$

    m_profileListViewer = CheckboxTableViewer.newCheckList( parent, SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER );
    m_profileListViewer.getTable().setHeaderVisible( true );
    m_profileListViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 ) );

    m_profileListViewer.setContentProvider( new KMViewerContentProvider() );
    m_profileListViewer.setCheckStateProvider( new KMViewerCheckStateProvider( this ) );

    final TableViewerColumn labelColumn = new TableViewerColumn( m_profileListViewer, SWT.LEFT );
    labelColumn.setLabelProvider( new ProfileNameLabelProvider() );
    labelColumn.getColumn().setText( Messages.getString("KMViewer_0") ); //$NON-NLS-1$
    labelColumn.getColumn().setWidth( 100 );
    ColumnViewerSorter.registerSorter( labelColumn, new ProfileNameSorter() );

    final TableViewerColumn stationColumn = new TableViewerColumn( m_profileListViewer, SWT.LEFT );
    stationColumn.setLabelProvider( new ProfileStationLabelProvider() );
    stationColumn.getColumn().setText( Messages.getString("KMViewer_1") ); //$NON-NLS-1$
    stationColumn.getColumn().setWidth( 100 );
    ColumnViewerSorter.registerSorter( stationColumn, new ProfileStationSorter() );

    final TableViewerColumn validColumn = new TableViewerColumn( m_profileListViewer, SWT.LEFT );
    validColumn.setLabelProvider( new ProfileValidLabelProvider( this ) );
    validColumn.getColumn().setText( Messages.getString("KMViewer_2") ); //$NON-NLS-1$
    validColumn.getColumn().setWidth( 200 );

    m_textKmStart.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusLost( final FocusEvent e )
      {
        handleKMStartModified();
      }
    } );

    m_textKmEnd.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusLost( final FocusEvent e )
      {
        handleKMEndModified();
      }
    } );
  }

  protected void handleKMStartModified( )
  {
    final String text = m_textKmStart.getText();
    try
    {
      final double km = NumberUtils.parseQuietDouble( text );
      final KalininMiljukovType input = getInput();
      if( input != null )
      {
        if( Double.isNaN( km ) )
          input.setKmStart( null );
        else
          input.setKmStart( km );
      }

      updateProfileList();
      updateControls();
    }
    catch( final Exception ex )
    {
      ex.printStackTrace();
    }
  }

  protected void handleKMEndModified( )
  {
    final String text = m_textKmEnd.getText();
    try
    {
      final double km = NumberUtils.parseQuietDouble( text );
      final KalininMiljukovType input = getInput();
      if( input != null )
      {
        if( Double.isNaN( km ) )
          input.setKmEnd( null );
        else
          input.setKmEnd( km );
      }

      updateProfileList();
      updateControls();
    }
    catch( final Exception ex )
    {
      ex.printStackTrace();
    }
  }

  protected void handleDirectoryFieldSelected( final IStructuredSelection selection )
  {
    final String path = (String) selection.getFirstElement();

    final KalininMiljukovType input = getInput();

    if( input != null )
      input.setPath( path );

    readProfileSet( path );

    if( m_profileSet != null && input != null )
    {
      final Double oldKmStart = input.getKmStart();
      if( oldKmStart == null )
      {
        final double startPosition = m_profileSet.getStartPosition() / 1000d;
        input.setKmStart( startPosition );
      }
      final Double oldKmEnd = input.getKmEnd();
      if( oldKmEnd == null )
      {
        final double endPosition = m_profileSet.getEndPosition() / 1000d;
        input.setKmEnd( endPosition );
      }
    }

    updateProfileList();
    updateControls();
  }

  private void readProfileSet( final String path )
  {
    if( StringUtils.isBlank( path ) )
      m_profileSet = null;
    else
    {
      final File file = new File( path );
      m_profileSet = ProfileFactory.createProfileSet( file );
    }
  }

  private void inputChanged( final KalininMiljukovType oldInput, final KalininMiljukovType kmType )
  {
    // TODO: check this stuff here...
    if( oldInput != null )
    {
      /* Keep checked elements */
      final List<Profile> profiles = oldInput.getProfile();
      for( final Profile profile2 : profiles )
        profile2.setEnabled( false );

      final Object[] checkedElements = m_profileListViewer.getCheckedElements();
      for( final Object object : checkedElements )
      {
        for( final Profile profile : profiles )
        {
          if( object == profile )
            profile.setEnabled( true );
        }
      }
    }

    m_input = kmType;

    if( m_profileListViewer != null )
      m_profileListViewer.setInput( kmType );

    final String path = m_input == null ? null : m_input.getPath();
    readProfileSet( path );
    updateControls();
  }

  private void updateControls( )
  {
    final KalininMiljukovType input = getInput();

    if( input == null )
    {
      // TODO: title + label
      m_textLabel.setText( Messages.getString("KMViewer_3") ); //$NON-NLS-1$

      m_dirField.setSelection( StructuredSelection.EMPTY );

      m_dirField.setEnabled( false );
      m_textKmStart.setText( "" ); //$NON-NLS-1$
      m_textKmStart.setEnabled( false );
      m_textKmEnd.setText( "" ); //$NON-NLS-1$
      m_textKmEnd.setEnabled( false );
      m_profileListViewer.getControl().setEnabled( false );
    }
    else
    {
      final Double kmStart = input.getKmStart();
      final Double kmEnd = input.getKmEnd();
      final String path = input.getPath();

      m_dirField.setSelection( new StructuredSelection( path ) );

      if( kmStart != null )
        m_textKmStart.setText( String.format( "%.4f", kmStart ) ); //$NON-NLS-1$
      else
        m_textKmStart.setText( "" ); //$NON-NLS-1$

      if( kmEnd != null )
        m_textKmEnd.setText( String.format( "%.4f", kmEnd ) ); //$NON-NLS-1$
      else
        m_textKmEnd.setText( "" ); //$NON-NLS-1$

      m_dirField.setEnabled( true );
      m_textKmStart.setEnabled( !StringUtils.isBlank( path ) );
      m_textKmEnd.setEnabled( !StringUtils.isBlank( path ) );
      m_profileListViewer.getControl().setEnabled( !StringUtils.isBlank( path ) );
    }

    m_profileListViewer.refresh();
    final TableColumn[] columns = m_profileListViewer.getTable().getColumns();
    for( final TableColumn column : columns )
      column.pack();
  }

  protected void updateProfileList( )
  {
    final KalininMiljukovType km = getInput();
    if( km == null )
      return;

    final List<Profile> profileList = km.getProfile();
    profileList.clear();
    if( m_profileSet == null )
      return;

    final Double kmStart = km.getKmStart();
    final Double kmEnd = km.getKmEnd();

    final ProfileData[] allProfiles = m_profileSet.getAllProfiles();
    for( final ProfileData pd : allProfiles )
    {
      final File file = pd.getFile();
      final double position = pd.getPosition();
      final double station = position / 1000.0;

      if( (kmStart == null || kmStart <= station) && (kmEnd == null || station <= kmEnd) )
      {
        final Profile profileData = KMBindingUtils.OF.createKalininMiljukovTypeProfile();
        profileData.setFile( file.getAbsolutePath() );
        profileData.setPositionKM( position );
        profileData.setEnabled( pd.isValidForKalypso() == null );
        profileList.add( profileData );
      }
    }
  }

  public void setInput( final String label, final KalininMiljukovType km )
  {
    m_textLabel.setText( label );

    final KalininMiljukovType oldInput = getInput();
    inputChanged( oldInput, km );
  }

  private KalininMiljukovType getInput( )
  {
    return m_input;
  }

  public String getValidMessage( final Profile profile )
  {
    final ProfileData data = findData( profile );
    if( data == null )
      return StringUtils.EMPTY;

    final String valid = data.isValidForKalypso();
    if( valid == null )
      return Messages.getString( "org.kalypso.model.km.ProfileData.10" ); //$NON-NLS-1$

    return valid;
  }

  public boolean isValid( final Profile profile )
  {
    final ProfileData data = findData( profile );
    if( data == null )
      return false;

    return data.isValidForKalypso() == null;
  }

  private ProfileData findData( final Profile profile )
  {
    if( m_profileSet == null )
      return null;

    final String file = profile.getFile();

    final ProfileData[] data = m_profileSet.getAllProfiles();
    for( final ProfileData profileData : data )
    {
      if( file.equals( profileData.getFile().getAbsolutePath() ) )
        return profileData;
    }

    return null;
  }
}
