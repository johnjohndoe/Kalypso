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
package org.kalypso.ui.rrm.kmupdate;

import java.io.File;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.model.km.ProfileData;
import org.kalypso.model.km.ProfileDataSet;
import org.kalypso.model.km.ProfileFactory;
import org.kalypso.ui.rrm.i18n.Messages;

import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType;
import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.ObjectFactory;
import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType.Profile;

/**
 * @author doemming
 */
public class KMViewer
{

  final CheckboxTableViewer m_profileListViewer;

  final Text m_textKm1;

  final Text m_textKm2;
  final StringBuffer m_selectionBuffer=new StringBuffer();

  private final DirectoryFieldWidget m_dirField;

  private final Composite m_top;

  public KMViewer( final Composite parent )
  {
//    m_selectionBuffer=new StringBuffer();
    m_top = parent;
    parent.setLayout( new GridLayout( 3, false ) );

    final String toolTip = Messages.get("org.kalypso.ui.rrm.kmupdate.KMViewer.0"); //$NON-NLS-1$
    m_dirField = new DirectoryFieldWidget( Messages.get("org.kalypso.ui.rrm.kmupdate.KMViewer.1"), toolTip, true, parent, 1, 1, 1 ); //$NON-NLS-1$
    m_dirField.addSelectionChangedListener( new ISelectionChangedListener()
    {

      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        final String value = (String) selection.getFirstElement();
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
    } );

    // 1/1
    final Label labelKm1 = new Label( parent, SWT.NONE );
    labelKm1.setLayoutData( new GridData() );
    labelKm1.setText( Messages.get("org.kalypso.ui.rrm.kmupdate.KMViewer.2") ); //$NON-NLS-1$

    // 1/2
    m_textKm1 = new Text( parent, SWT.NONE );
    final GridData data = new GridData( GridData.FILL_HORIZONTAL );
    data.horizontalSpan = 1;
    m_textKm1.setLayoutData( data );
    final Label composite = new Label( parent, SWT.NONE );
    composite.setLayoutData( new GridData() );
    // 2/1
    final Label labelKm2 = new Label( parent, SWT.NONE );
    labelKm2.setLayoutData( new GridData() );
    labelKm2.setText( Messages.get("org.kalypso.ui.rrm.kmupdate.KMViewer.3") ); //$NON-NLS-1$

    // 2/2
    m_textKm2 = new Text( parent, SWT.NONE );
    final GridData data3 = new GridData( GridData.FILL_HORIZONTAL );
    data3.horizontalSpan = 1;
    m_textKm2.setLayoutData( data3 );
    final Label composite2 = new Label( parent, SWT.NONE );
    composite2.setLayoutData( new GridData() );
    // row 3
    // row 4
    final Label label = new Label( parent, SWT.HORIZONTAL | SWT.SEPARATOR );
    final GridData data4 = new GridData( GridData.FILL_HORIZONTAL );
    data4.horizontalSpan = 3;
    data4.grabExcessHorizontalSpace = true;
    label.setLayoutData( data4 );

    // row 6 /1-2
    final Group profileGroup = new Group( parent, SWT.NONE );
    final GridData data2 = new GridData( GridData.FILL_BOTH );
    data2.horizontalSpan = 2;
    data2.grabExcessHorizontalSpace = true;
    data2.grabExcessVerticalSpace = true;
    profileGroup.setLayoutData( data2 );
    profileGroup.setText( Messages.get("org.kalypso.ui.rrm.kmupdate.KMViewer.4") ); //$NON-NLS-1$
    profileGroup.setLayout( new GridLayout() );

    // new ChecklistFieldEditor
    m_profileListViewer = CheckboxTableViewer.newCheckList( profileGroup, SWT.H_SCROLL | SWT.V_SCROLL );
    m_profileListViewer.setContentProvider( new KMViewerContentProvider() );
    m_profileListViewer.setLabelProvider( new KMViewerLabelProvider() );
    final GridData gData = new GridData( GridData.FILL_BOTH );
    gData.grabExcessHorizontalSpace = true;
    gData.grabExcessVerticalSpace = true;
    // gData.horizontalSpan = 3;
    m_profileListViewer.getControl().setLayoutData( gData );

    // row 6 /3
    final Button button = new Button( parent, SWT.PUSH );
    button.setText( Messages.get("org.kalypso.ui.rrm.kmupdate.KMViewer.5") ); //$NON-NLS-1$
    final GridData data5 = new GridData( GridData.VERTICAL_ALIGN_BEGINNING );
    button.setLayoutData( data5 );

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
          final double km = Double.parseDouble( text );
          final Object input = m_profileListViewer.getInput();
          if( input instanceof KalininMiljukovType )
            ((KalininMiljukovType) input).setKmStart( km );
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
          final double km = Double.parseDouble( text );
          final Object input = m_profileListViewer.getInput();
          if( input instanceof KalininMiljukovType )
            ((KalininMiljukovType) input).setKmEnd( km );
        }
        catch( final Exception ex )
        {
          ex.printStackTrace();
        }
      }
    } );
  }

  public void inputChanged( final KalininMiljukovType oldInput, final KalininMiljukovType kmType )
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
    if( m_profileListViewer != null && kmType != null )
    {
      m_profileListViewer.setInput( kmType );
      final Double kmStart = kmType.getKmStart();
      final Double kmEnd = kmType.getKmEnd();
      // final String id = kmType.getId();
      // final String riverName = kmType.getRiverName();
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
      for( final Profile profile : profiles )
      {
        m_profileListViewer.setChecked( profile, profile.isEnabled() );
      }
      setEnabled( true );
    }
    else
      setEnabled( false );
  }

  public void setEnabled( final boolean enabled )
  {
    // m_textKm1.setVisible( enabled );
    // m_textKm2.setVisible( enabled );
    // m_profileListViewer.getControl().setVisible( enabled );
    m_top.setVisible( enabled );
  }

  protected class KMViewerContentProvider implements IStructuredContentProvider
  {

    public Object[] getElements( final Object inputElement )
    {
      if( inputElement instanceof KalininMiljukovType )
      {
        final KalininMiljukovType kmType = (KalininMiljukovType) inputElement;
        final List<Profile> profiles = kmType.getProfile();
        return profiles.toArray();
      }
      return new Object[0];
    }

    public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
    {
    }

    public void dispose( )
    {
    }

  }

  protected class KMViewerLabelProvider implements ILabelProvider
  {
    /**
     * @see org.eclipse.jface.viewers.ILabelProvider#getImage(java.lang.Object)
     */
    public Image getImage( final Object element )
    {
      // no image
      return null;
    }

    /**
     * @see org.eclipse.jface.viewers.ILabelProvider#getText(java.lang.Object)
     */
    public String getText( final Object element )
    {
      final StringBuffer result = new StringBuffer();
      if( element instanceof Profile )
      {
        final Profile profile = (Profile) element;
        final String file = profile.getFile();
        if( file != null )
          result.append( file );

        final Double pos = profile.getPositionKM();
        if( pos != null )
        {
          final double positionKM = pos.doubleValue() / 1000d;
          result.append( "  " + Double.toString( positionKM ) + Messages.get("org.kalypso.ui.rrm.kmupdate.KMViewer.9") ); //$NON-NLS-1$ //$NON-NLS-2$
        }
      }
      return result.toString();
    }

    /**
     * @see org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
     */
    public void addListener( final ILabelProviderListener listener )
    {
      // TODO Auto-generated method stub

    }

    /**
     * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
     */
    public void dispose( )
    {
      // TODO Auto-generated method stub
    }

    /**
     * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object, java.lang.String)
     */
    public boolean isLabelProperty( final Object element, final String property )
    {
      // TODO Auto-generated method stub
      return false;
    }

    /**
     * @see org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse.jface.viewers.ILabelProviderListener)
     */
    public void removeListener( final ILabelProviderListener listener )
    {
      // TODO Auto-generated method stub

    }

  }

  void updateProfileList( )
  {
    final Object input = m_profileListViewer.getInput();
    if( input instanceof KalininMiljukovType )
    {
      final ObjectFactory fac = new ObjectFactory(); // TODO
      final KalininMiljukovType km = (KalininMiljukovType) input;
      final double kmStart = km.getKmStart();
      final double kmEnd = km.getKmEnd();
      final String path = km.getPath();
      final List<Profile> profileList = km.getProfile();
      profileList.clear();
      if( path != null )
      {

        final File profileDir = new File( path );
        final ProfileDataSet set = ProfileFactory.createProfileSet( profileDir, kmStart, kmEnd );
        final Iterator<ProfileData> allProfiles = set.getAllProfiles();
        while( allProfiles.hasNext() )
        {
          final ProfileData pd = allProfiles.next();
          final File file = pd.getFile();
          final double position = pd.getPosition();
          final Profile profileData = fac.createKalininMiljukovTypeProfile();
          profileData.setFile( file.toString() );
          profileData.setPositionKM( position );
          System.out.print(Messages.get("org.kalypso.ui.rrm.kmupdate.KMViewer.10") + pd.getPosition()); //$NON-NLS-1$
          profileData.setEnabled( pd.isValidForKalypso(m_selectionBuffer) );
          profileList.add( profileData );
          // m_profileListViewer.setChecked( profileData, true );
        }
        inputChanged( null, km );
      }
      // m_profileListViewer.refresh();
    }

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
