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
import java.util.Iterator;
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
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.contribs.java.lang.NumberUtils;
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

  private final StringBuffer m_selectionBuffer = new StringBuffer();

  private final DirectoryFieldWidget m_dirField;

  private final Text m_textLabel;

  public KMViewer( final Composite parent )
  {
    parent.setLayout( new GridLayout( 3, false ) );

    // Label
    /* final Label labelLabel = */new Label( parent, SWT.NONE );
// labelLabel.setText( "KM Strang" );
    m_textLabel = new Text( parent, SWT.BORDER | SWT.READ_ONLY );
    m_textLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    new Label( parent, SWT.NONE );

    final String toolTip = Messages.getString("org.kalypso.ui.rrm.kmupdate.KMViewer.0"); //$NON-NLS-1$
    m_dirField = new DirectoryFieldWidget( Messages.getString("org.kalypso.ui.rrm.kmupdate.KMViewer.1"), toolTip, true, parent, 1, 1, 1 ); //$NON-NLS-1$
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
    m_profileListViewer.setCheckStateProvider( new KMViewerCheckStateProvider() );

    final TableViewerColumn labelColumn = new TableViewerColumn( m_profileListViewer, SWT.LEFT );
    labelColumn.setLabelProvider( new ProfileNameLabelProvider() );
    labelColumn.getColumn().setText( "File" );
    labelColumn.getColumn().setWidth( 200 );
    ColumnViewerSorter.registerSorter( labelColumn, new ProfileNameSorter() );

    final TableViewerColumn stationColumn = new TableViewerColumn( m_profileListViewer, SWT.LEFT );
    stationColumn.setLabelProvider( new ProfileStationLabelProvider() );
    stationColumn.getColumn().setText( "Station" );
    stationColumn.getColumn().setWidth( 100 );
    ColumnViewerSorter.registerSorter( stationColumn, new ProfileStationSorter() );

    // FIXME: show the message that is internally created for enablement
// final TableViewerColumn validColumn = new TableViewerColumn( m_profileListViewer, SWT.LEFT );
// validColumn.setLabelProvider( new ProfileValidLabelProvider() );
// validColumn.getColumn().setText( "Message" );
// validColumn.getColumn().setWidth( 200 );

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
    final String value = (String) selection.getFirstElement();

    final KalininMiljukovType input = getInput();
    if( input == null )
      return;

    input.setPath( value );

    if( !StringUtils.isBlank( value ) )
    {
      final File path = new File( value );

      final ProfileDataSet set = ProfileFactory.createProfileSet( path );
      final Double oldKmStart = input.getKmStart();
      if( oldKmStart == null )
      {
        final double startPosition = set.getStartPosition() / 1000d;
        input.setKmStart( startPosition );
      }
      final Double oldKmEnd = input.getKmEnd();
      if( oldKmEnd == null )
      {
        final double endPosition = set.getEndPosition() / 1000d;
        input.setKmEnd( endPosition );
      }
    }

    updateProfileList();
    updateControls();
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

    if( m_profileListViewer != null )
      m_profileListViewer.setInput( kmType );

    updateControls();
  }

  private void updateControls( )
  {
    final KalininMiljukovType input = getInput();

    if( input == null )
    {
      // TODO: title + label
      m_textLabel.setText( "<Kein Strang selektiert>" );

      m_dirField.setSelection( StructuredSelection.EMPTY );

      m_dirField.setEnabled( false );
      m_textKmStart.setText( "" );
      m_textKmStart.setEnabled( false );
      m_textKmEnd.setText( "" );
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
        m_textKmStart.setText( String.format( "%.4f", kmStart ) );
      else
        m_textKmStart.setText( "" ); //$NON-NLS-1$

      if( kmEnd != null )
        m_textKmEnd.setText( String.format( "%.4f", kmEnd ) );
      else
        m_textKmEnd.setText( "" ); //$NON-NLS-1$

      m_dirField.setEnabled( true );
      m_textKmStart.setEnabled( !StringUtils.isBlank( path ) );
      m_textKmEnd.setEnabled( !StringUtils.isBlank( path ) );
      m_profileListViewer.getControl().setEnabled( !StringUtils.isBlank( path ) );
    }

    m_profileListViewer.refresh();
  }

  protected void updateProfileList( )
  {
    final KalininMiljukovType km = getInput();
    if( km == null )
      return;

    final double kmStart = km.getKmStart();
    final double kmEnd = km.getKmEnd();
    final String path = km.getPath();
    final List<Profile> profileList = km.getProfile();
    profileList.clear();
    if( path == null )
      return;

    final File profileDir = new File( path );
    final ProfileDataSet set = ProfileFactory.createProfileSet( profileDir, kmStart, kmEnd );
    final Iterator<ProfileData> allProfiles = set.getAllProfiles();
    while( allProfiles.hasNext() )
    {
      final ProfileData pd = allProfiles.next();
      final File file = pd.getFile();
      final double position = pd.getPosition();
      final Profile profileData = KMUpdateWizardPage.OF.createKalininMiljukovTypeProfile();
      profileData.setFile( file.toString() );
      profileData.setPositionKM( position );
      profileData.setEnabled( pd.isValidForKalypso( m_selectionBuffer ) );
      profileList.add( profileData );
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
    return (KalininMiljukovType) m_profileListViewer.getInput();
  }

  public StringBuffer getSelectionBuffer( )
  {
    return m_selectionBuffer;
  }

}
