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
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
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

  private final Text m_textKm1;

  private final Text m_textKm2;

  private final StringBuffer m_selectionBuffer = new StringBuffer();

  private final DirectoryFieldWidget m_dirField;

  private final Composite m_top;

  public KMViewer( final Composite parent )
  {
    m_top = parent;
    parent.setLayout( new GridLayout( 3, false ) );

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
    final Label labelKm1 = new Label( parent, SWT.NONE );
    labelKm1.setText( Messages.getString("org.kalypso.ui.rrm.kmupdate.KMViewer.2") ); //$NON-NLS-1$
    m_textKm1 = new Text( parent, SWT.BORDER );
    m_textKm1.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    new Label( parent, SWT.NONE );

    // END
    final Label labelKm2 = new Label( parent, SWT.NONE );
    labelKm2.setText( Messages.getString("org.kalypso.ui.rrm.kmupdate.KMViewer.3") ); //$NON-NLS-1$
    m_textKm2 = new Text( parent, SWT.BORDER );
    m_textKm2.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    new Label( parent, SWT.NONE );

    // Separator
    new Label( parent, SWT.HORIZONTAL | SWT.SEPARATOR ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 3, 1 ) );

    // Profiles
    final Group profileGroup = new Group( parent, SWT.NONE );
    profileGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 ) );
    profileGroup.setText( Messages.getString("org.kalypso.ui.rrm.kmupdate.KMViewer.4") ); //$NON-NLS-1$
    profileGroup.setLayout( new FillLayout() );

    m_profileListViewer = CheckboxTableViewer.newCheckList( profileGroup, SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER );
    m_profileListViewer.setContentProvider( new KMViewerContentProvider() );
    m_profileListViewer.setLabelProvider( new KMViewerLabelProvider() );

    // update
    final Button button = new Button( parent, SWT.PUSH );
    button.setText( Messages.getString("org.kalypso.ui.rrm.kmupdate.KMViewer.5") ); //$NON-NLS-1$
    button.setLayoutData( new GridData( SWT.CENTER, SWT.BEGINNING, false, false ) );
    button.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        updateProfileList();
      }
    } );

    m_textKm1.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusLost( final FocusEvent e )
      {
        final String text = m_textKm1.getText();
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
        }
        catch( final Exception ex )
        {
          ex.printStackTrace();
        }
      }
    } );

    m_textKm2.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusLost( final FocusEvent e )
      {
        final String text = m_textKm2.getText();
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
        }
        catch( final Exception ex )
        {
          ex.printStackTrace();
        }
      }
    } );
  }

  protected void handleDirectoryFieldSelected( final IStructuredSelection selection )
  {
    final String value = (String) selection.getFirstElement();
    if( StringUtils.isBlank( value ) )
      return;

    final Object input = m_profileListViewer.getInput();
    if( input instanceof KalininMiljukovType )
    {
      final KalininMiljukovType km = (KalininMiljukovType) input;
      km.setPath( value );
      final File path = new File( value );
      final ProfileDataSet set = ProfileFactory.createProfileSet( path );
      final Double oldKmStart = km.getKmStart();
      if( oldKmStart == null )
      {
        final double startPosition = set.getStartPosition() / 1000d;
        km.setKmStart( startPosition );
      }
      final Double oldKmEnd = km.getKmEnd();
      if( oldKmEnd == null )
      {
        final double endPosition = set.getEndPosition() / 1000d;
        km.setKmEnd( endPosition );
      }
      m_textKm1.setText( km.getKmStart().toString() );
      m_textKm2.setText( km.getKmEnd().toString() );
    }
  }

  private void inputChanged( final KalininMiljukovType oldInput, final KalininMiljukovType kmType )
  {
    if( oldInput != null )
    {
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

    if( kmType != null )
    {
      final Double kmStart = kmType.getKmStart();
      final Double kmEnd = kmType.getKmEnd();
      final String path = kmType.getPath();
      if( kmStart != null )
        m_textKm1.setText( kmStart.toString() );
      else
        m_textKm1.setText( "" ); //$NON-NLS-1$
      if( kmEnd != null )
        m_textKm2.setText( kmEnd.toString() );
      else
        m_textKm2.setText( "" ); //$NON-NLS-1$

      m_dirField.setSelection( new StructuredSelection( path ) );

      final List<Profile> profiles = kmType.getProfile();
      if( m_profileListViewer != null )
      {
        for( final Profile profile : profiles )
          m_profileListViewer.setChecked( profile, profile.isEnabled() );
      }
    }

    m_top.setVisible( kmType != null && m_profileListViewer != null );
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
      System.out.print( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMViewer.10" ) + pd.getPosition() ); //$NON-NLS-1$
      profileData.setEnabled( pd.isValidForKalypso( m_selectionBuffer ) );
      profileList.add( profileData );
    }
    inputChanged( null, km );
  }

  public void setInput( final KalininMiljukovType km )
  {
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
