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
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import kalypsoUIRRM.KalypsoUIRRMPlugin;

import org.apache.commons.io.IOUtils;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.math.LinearEquation.SameXValuesException;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.model.km.AbstractKMValue;
import org.kalypso.model.km.ProfileDataSet;
import org.kalypso.model.km.ProfileFactory;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypsodeegree.model.feature.Feature;

import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovGroupType;
import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType;
import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.ObjectFactory;
import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType.Profile;

/**
 * @author doemming
 */
public class KMUpdateWizardPage extends WizardPage
{

  private static final String KM_CONFIG_PATH = "kalypsoRRM.kmUpdate.configPath";

  private Composite m_top = null;

  KMViewer m_kmViewer = null;

  private final Feature[] m_selection;

  private final CommandableWorkspace m_workspace;

  private CheckboxTableViewer m_channelListViewer;

  private KalininMiljukovGroupType m_kmGroup = null;

  final ObjectFactory m_factory = new ObjectFactory();

  final JAXBContext m_context = JaxbUtilities.createQuiet( ObjectFactory.class );

  final KalypsoUIRRMPlugin m_plugin = KalypsoUIRRMPlugin.getDefault();

  String m_configPath = null;

  public KMUpdateWizardPage( final CommandableWorkspace workspace, IFeatureSelection selection )
  {
    super( "Kalinin-Miljukov-Parameter aktualisieren ",// 
        "Berechnung und Export der Kalinin-Miljukov(KM) Parameter aus 1D-Simulationsergebnissen in die NA-Modelldaten", null );
    m_workspace = workspace;
    final EasyFeatureWrapper[] allFeatures = selection.getAllFeatures();
    m_selection = new Feature[allFeatures.length];
    for( int i = 0; i < allFeatures.length; i++ )
      m_selection[i] = allFeatures[i].getFeature();
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    m_top = new Composite( parent, SWT.NONE );

    final GridLayout gridLayout = new GridLayout( 2, true );
    m_top.setLayout( gridLayout );
    GridData data0 = new GridData( GridData.FILL_BOTH );
    data0.grabExcessHorizontalSpace = true;
    data0.grabExcessVerticalSpace = true;
    m_top.setLayoutData( data0 );

    final Composite configGroup = new Composite( m_top, SWT.FILL );
    final GridData data5 = new GridData( GridData.FILL_HORIZONTAL );
    data5.horizontalSpan = 2;
    data5.grabExcessHorizontalSpace = true;
    configGroup.setLayoutData( data5 );

    createGenerateArea( configGroup );

    // column 1 tree-group
    final Group treeGroup = new Group( m_top, SWT.FILL );
    treeGroup.setText( "Kalinin-Miljukov Straenge" );
    final GridData data2 = new GridData( GridData.FILL_BOTH );
    data2.grabExcessHorizontalSpace = true;
    data2.grabExcessVerticalSpace = true;
    treeGroup.setLayoutData( data2 );

    // column 2 KM detailed
    final Group kmGroup = new Group( m_top, SWT.NONE );
    kmGroup.setText( "Kalinin-Miljukov Details" );
    GridData data4 = new GridData( GridData.FILL_BOTH );
    data4.grabExcessHorizontalSpace = true;
    data4.grabExcessVerticalSpace = true;
    kmGroup.setLayoutData( data4 );
    m_kmViewer = new KMViewer( kmGroup );

    treeGroup.setLayout( new GridLayout() );
    createKMChannelListViewer( treeGroup );

    if( m_selection.length > 0 )
      m_channelListViewer.setSelection( new StructuredSelection( m_selection[0] ) );
    m_channelListViewer.setCheckedElements( m_selection );

    setControl( m_top );
  }

  private void createGenerateArea( final Composite parent )
  {
    parent.setLayout( new GridLayout( 3, false ) );

    final Label label = new Label( parent, SWT.NONE );
    label.setText( "config" );
    label.setLayoutData( new GridData() );

    final Text text = new Text( parent, SWT.READ_ONLY | SWT.BORDER );
    final GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
    gridData.grabExcessHorizontalSpace = true;
    text.setLayoutData( gridData );

    final Button loadButton = new Button( parent, SWT.PUSH );
    loadButton.setText( "load ..." );

    final Label space = new Label( parent, SWT.NONE );
    final GridData data = new GridData();
    data.horizontalSpan = 2;
    space.setLayoutData( data );

    final Button saveButton = new Button( parent, SWT.PUSH );
    saveButton.setText( "save..." );

    loadButton.addSelectionListener( new SelectionAdapter()
    {

      @Override
      public void widgetSelected( SelectionEvent e )
      {
        final FileDialog dialog = new FileDialog( parent.getShell() );
        final String path = dialog.open();
        if( path != null && path.length() > 0 )
        {
          m_configPath = path;
          text.setText( path );
          loadAs( path );
        }
        // else
        // ;// TODO message
      }

    } );

    saveButton.addSelectionListener( new SelectionAdapter()
    {

      @Override
      public void widgetSelected( SelectionEvent e )
      {
        final FileDialog dialog = new FileDialog( parent.getShell() );
        final String path = dialog.open();
        if( path != null && path.length() > 0 )
        {
          m_configPath = path;
          text.setText( path );
          saveAs( path );
        }
      }
    } );

    final String path = m_plugin.getDialogSettings().get( KM_CONFIG_PATH );

    if( path == null )
      createNewKMGroup();
    else
    {
      text.setText( path );
      loadAs( path );
    }
  }

  protected void createNewKMGroup( )
  {
    final IFeatureType kmFT = m_workspace.getFeatureType( KMUpdateConstants.QNAME_KMCHANNEL );
    final Feature[] features = m_workspace.getFeatures( kmFT );

    KalininMiljukovGroupType kmGroup = m_factory.createKalininMiljukovGroupType();
    final List<KalininMiljukovType> kalininMiljukovList = kmGroup.getKalininMiljukov();

    for( int i = 0; i < features.length; i++ )
    {
      final Feature feature = features[i];
      final KalininMiljukovType km = m_factory.createKalininMiljukovType();
      km.setId( feature.getId() );
      km.setFilePattern( "*km" );
      km.setPath( "" );
      kalininMiljukovList.add( km );
    }
    setKMGroup( kmGroup );
  }

  protected void setKMGroup( KalininMiljukovGroupType kmGroup )
  {
    m_kmGroup = kmGroup;
    if( m_channelListViewer != null )
    {
      m_channelListViewer.refresh();
      m_channelListViewer.setSelection( new ISelection()
      {
        public boolean isEmpty( )
        {
          return true;
        }
      } );
    }
  }

  private void createKMChannelListViewer( final Composite parent )
  {
    m_channelListViewer = CheckboxTableViewer.newCheckList( parent, SWT.H_SCROLL | SWT.V_SCROLL );

    m_channelListViewer.setContentProvider( new KMUpdateContentProvider() );
    m_channelListViewer.setLabelProvider( new KMUpdateLabelProvider() );

    // viewer.setSelection( m_selection ); TODO
    m_channelListViewer.setInput( m_workspace );
    final GridData data3 = new GridData( GridData.FILL_BOTH );
    data3.grabExcessHorizontalSpace = true;
    data3.grabExcessVerticalSpace = true;

    m_channelListViewer.getControl().setLayoutData( data3 );

    m_channelListViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {

      public void selectionChanged( SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();

        final Object firstElement = selection.getFirstElement();
        if( firstElement instanceof Feature )
        {
          final Feature feature = (Feature) firstElement;
          final String fid = feature.getId();
          final KalininMiljukovType km = getForID( fid );
          m_kmViewer.setInput( km );
        }
        else
          m_kmViewer.setInput( null );
      }

    } );
  }

  protected KalininMiljukovType getForID( String fid )
  {
    if( m_kmGroup == null )
      return null;
    final List<KalininMiljukovType> kalininMiljukov = m_kmGroup.getKalininMiljukov();
    for( Iterator iter = kalininMiljukov.iterator(); iter.hasNext(); )
    {
      final KalininMiljukovType km = (KalininMiljukovType) iter.next();
      if( fid.equals( km.getId() ) )
        return km;
    }
    return null;
  }

  void loadAs( String path )
  {
    final Unmarshaller unmarshaller;
    try
    {
      unmarshaller = m_context.createUnmarshaller();
      final File file = new File( path );
      Object object = unmarshaller.unmarshal( file );
      if( object instanceof JAXBElement )
      {
        final JAXBElement<KalininMiljukovGroupType> object2 = (JAXBElement<KalininMiljukovGroupType>) object;
        setKMGroup( object2.getValue() );
      }
    }
    catch( Exception ex )
    {
      MessageDialog.openError( getShell(), "Kalinin-Miljukov-Zuordnung", "Fehler beim Laden der Konfiguration (" + path + ")" );
      createNewKMGroup();
    }
  }

  void saveAs( String path )
  {
    FileWriter writer = null;
    try
    {
      final Marshaller marshaller = m_context.createMarshaller();
      final File file = new File( path );
      final JAXBElement<KalininMiljukovGroupType> element = m_factory.createKalininMiljukovGroup( m_kmGroup );
      writer = new FileWriter( file );
      marshaller.marshal( element, writer );
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }

  }

  public boolean finish( )
  {
    m_plugin.getDialogSettings().put( KM_CONFIG_PATH, m_configPath );
    saveAs( m_configPath );
    final List<FeatureChange> changes = new ArrayList<FeatureChange>();
    final Object[] checkedElements = m_channelListViewer.getCheckedElements();
    for( int i = 0; i < checkedElements.length; i++ )
    {
      Object object = checkedElements[i];
      if( object instanceof Feature )
        try
        {
          updateFeature( (Feature) object, changes );
        }
        catch( SameXValuesException e )
        {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }
    }
    final FeatureChange[] change = changes.toArray( new FeatureChange[changes.size()] );
    final ChangeFeaturesCommand command = new ChangeFeaturesCommand( m_workspace, change );
    try
    {
      m_workspace.postCommand( command );
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return true;
  }

  private KalininMiljukovType getKMForID( String id )
  {
    final List<KalininMiljukovType> kalininMiljukov = m_kmGroup.getKalininMiljukov();
    for( Iterator<KalininMiljukovType> iter = kalininMiljukov.iterator(); iter.hasNext(); )
    {
      KalininMiljukovType km = iter.next();
      if( id.equals( km.getId() ) )
        return km;
    }
    return null;
  }

  private List<FeatureChange> updateFeature( final Feature feature, final List<FeatureChange> changeList ) throws SameXValuesException
  {
    final List<FeatureChange> result;
    if( changeList == null )
      result = new ArrayList<FeatureChange>();
    else
      result = changeList;

    final KalininMiljukovType km = getKMForID( feature.getId() );
    final IFeatureType kmFT = m_workspace.getFeatureType( KMUpdateConstants.QNAME_KMCHANNEL );
    final IFeatureType kmPaFT = m_workspace.getFeatureType( KMUpdateConstants.QNAME_KMParameter );

    final IRelationType kmRT = (IRelationType) kmFT.getProperty( KMUpdateConstants.QNAME_KMParameterMember );

    final IPropertyType qrkPT = kmPaFT.getProperty( KMUpdateConstants.QNAME_qrk );
    final IPropertyType rkfPT = kmPaFT.getProperty( KMUpdateConstants.QNAME_rkf );
    final IPropertyType rkvT = kmPaFT.getProperty( KMUpdateConstants.QNAME_rkv );
    final IPropertyType rnfPT = kmPaFT.getProperty( KMUpdateConstants.QNAME_rnf );
    final IPropertyType rnvPT = kmPaFT.getProperty( KMUpdateConstants.QNAME_rnv );

    if( km == null )
      return result;
    double kmStart = km.getKmStart();
    double kmEnd = km.getKmEnd();
    final List<File> list = new ArrayList<File>();
    final List<Profile> profiles = km.getProfile();
    for( Iterator<Profile> iter = profiles.iterator(); iter.hasNext(); )
    {
      Profile profile = iter.next();
      if( profile.isEnabled() )
      {
        final String path = profile.getFile();
        final File file = new File( path );
        if( file.canRead() )
          list.add( file );
      }
    }
    final File[] files = list.toArray( new File[list.size()] );
    final ProfileDataSet profileSet = ProfileFactory.createProfileSet( files, kmStart, kmEnd );
    int max = 5;
    final AbstractKMValue[] values = profileSet.getKMValues( max );
    final Feature[] kmParameter = m_workspace.resolveLinks( feature, kmRT );
    if( kmParameter.length < max )
    {
      // TODO add new features
    }
    else
    {
      for( int i = 0; i < kmParameter.length; i++ )
      {
        final Feature kmParameterFE = kmParameter[i];
        final AbstractKMValue value = values[i];

        result.add( new FeatureChange( kmParameterFE, rkfPT, value.getK() ) );
        result.add( new FeatureChange( kmParameterFE, rkvT, value.getKForeland() ) );
        result.add( new FeatureChange( kmParameterFE, rnfPT, value.getN() ) );
        result.add( new FeatureChange( kmParameterFE, rnvPT, value.getNForeland() ) );
        result.add( new FeatureChange( kmParameterFE, qrkPT, value.getQForeland() ) ); // ??
      }
    }
    return result;
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#isPageComplete()
   */
  @Override
  public boolean isPageComplete( )
  {
    return m_channelListViewer.getCheckedElements().length > 0;
  }
}
