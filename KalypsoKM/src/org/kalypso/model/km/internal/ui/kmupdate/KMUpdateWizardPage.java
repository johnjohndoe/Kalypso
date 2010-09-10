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
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.bind.JaxbUtilities;
import org.kalypso.contribs.eclipse.jface.dialog.ScrolledTextInformationDialog;
import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.contribs.java.util.logging.ILogger;
import org.kalypso.contribs.java.util.logging.LoggerUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.model.KMChannel;
import org.kalypso.model.hydrology.binding.model.KMParameter;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.km.internal.core.IKMValue;
import org.kalypso.model.km.internal.core.ProfileDataSet;
import org.kalypso.model.km.internal.core.ProfileFactory;
import org.kalypso.model.km.internal.i18n.Messages;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovGroupType;
import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType;
import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType.Profile;
import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.ObjectFactory;

/**
 * @author doemming
 */
public class KMUpdateWizardPage extends WizardPage
{
  // FIXME: move into helper class
  public final static ObjectFactory OF = new ObjectFactory();

  public final static JAXBContext JC = JaxbUtilities.createQuiet( ObjectFactory.class );

  private KMViewer m_kmViewer = null;

  private final Feature[] m_selection;

  /**
   * @deprecated Use m_namodel instead
   */
  @Deprecated
  private final CommandableWorkspace m_workspace;

  private CheckboxTableViewer m_channelListViewer;

  private KalininMiljukovGroupType m_kmGroup = null;

  private String m_configPath = null;

  private final KMUpdateLabelProvider m_kmUpdateLabelProvider = new KMUpdateLabelProvider();

  private final NaModell m_naModel;

  public KMUpdateWizardPage( final CommandableWorkspace workspace, final IFeatureSelection selection )
  {
    super( "kmPage", Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.0" ), null ); //$NON-NLS-1$ //$NON-NLS-2$

    setDescription( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.1" ) ); //$NON-NLS-1$

    m_workspace = workspace;
    m_naModel = (NaModell) m_workspace.getRootFeature();

    m_selection = FeatureSelectionHelper.getFeatures( selection );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    final Composite top = new Composite( parent, SWT.NONE );
    top.setLayout( new GridLayout( 2, true ) );

    // column 1 tree-group
    final Group treeGroup = createKMChannelListViewer( top );
    treeGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    // column 2 KM detailed
    final Group kmGroup = new Group( top, SWT.NONE );
    kmGroup.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.3" ) ); //$NON-NLS-1$
    kmGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_kmViewer = new KMViewer( kmGroup );

    final Composite configPanel = createGenerateArea( top );
    configPanel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false, 2, 1 ) );

    setControl( top );
  }

  private Composite createGenerateArea( final Composite parent )
  {
    final Composite configPanel = new Composite( parent, SWT.NONE );
    configPanel.setLayout( new GridLayout( 4, false ) );

    final Label label = new Label( configPanel, SWT.NONE );
    label.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.4" ) ); //$NON-NLS-1$

    final Text text = new Text( configPanel, SWT.READ_ONLY | SWT.BORDER );
    text.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Button loadButton = new Button( configPanel, SWT.PUSH );
    loadButton.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.5" ) ); //$NON-NLS-1$
    loadButton.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );

    final Button saveButton = new Button( configPanel, SWT.PUSH );
    saveButton.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.6" ) ); //$NON-NLS-1$
    saveButton.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );

    loadButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleLoadConfig( text );
      }
    } );

    saveButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleSaveconfig( text );
      }
    } );

    final String path = getDialogSettings().get( getResourceKey() );
    m_configPath = path;
    if( path == null )
      createNewKMConfiguration();
    else
    {
      text.setText( m_configPath );
      loadAs( path );
    }

    return configPanel;
  }

  protected void handleSaveconfig( final Text text )
  {
    final FileDialog dialog = new FileDialog( getShell(), SWT.SAVE );
    dialog.setFilterExtensions( new String[] { "*.km_xml" } ); //$NON-NLS-1$
    final String path = dialog.open();
    if( path == null || path.length() <= 0 )
      return;

    text.setText( path );
    m_configPath = path;
    saveAs( path );
  }

  protected void handleLoadConfig( final Text text )
  {
    final FileDialog dialog = new FileDialog( getShell(), SWT.OPEN );
    dialog.setFilterExtensions( new String[] { "*.km_xml" } ); //$NON-NLS-1$
    final String path = dialog.open();
    if( path == null || path.length() <= 0 )
      return;

    text.setText( path );
    m_configPath = path;
    loadAs( path );
  }

  // FIXME: probably move into own wrapper class!
  protected void createNewKMConfiguration( )
  {
    final IFeatureType kmFT = m_workspace.getFeatureType( KMChannel.FEATURE_KM_CHANNEL );
    final Feature[] features = m_workspace.getFeatures( kmFT );

    final KalininMiljukovGroupType kmGroup = OF.createKalininMiljukovGroupType();
    final List<KalininMiljukovType> kalininMiljukovList = kmGroup.getKalininMiljukov();

    for( final Feature feature : features )
    {
      final KalininMiljukovType km = createKMForFeature( feature );
      kalininMiljukovList.add( km );
    }
    setKMGroup( kmGroup );
  }

  protected KalininMiljukovType createKMForFeature( final Feature feature )
  {
    final KalininMiljukovType km = OF.createKalininMiljukovType();
    km.setId( feature.getId() );
    km.setFilePattern( "*km" ); //$NON-NLS-1$
    km.setPath( "" ); //$NON-NLS-1$

    final Double propStart = (Double) feature.getProperty( NaModelConstants.KM_CHANNEL_KMSTART );
    km.setKmStart( propStart );

    final Double propEnd = (Double) feature.getProperty( NaModelConstants.KM_CHANNEL_KMEND );
    km.setKmEnd( propEnd );
    return km;
  }

  protected void setKMGroup( final KalininMiljukovGroupType kmGroup )
  {
    m_kmGroup = kmGroup;
    if( m_channelListViewer == null )
      return;

    m_channelListViewer.refresh();
    m_channelListViewer.setSelection( StructuredSelection.EMPTY );
  }

  private Group createKMChannelListViewer( final Composite parent )
  {
    final Group treeGroup = new Group( parent, SWT.FILL );
    treeGroup.setLayout( new FillLayout() );
    treeGroup.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.2" ) ); //$NON-NLS-1$

    m_channelListViewer = CheckboxTableViewer.newCheckList( treeGroup, SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER );

    final KMChannel[] kmChannels = m_naModel.getKMChannels();

    m_channelListViewer.setContentProvider( new ArrayContentProvider() );

    m_channelListViewer.setLabelProvider( m_kmUpdateLabelProvider );
    m_channelListViewer.setInput( kmChannels );

    m_channelListViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        handleChannelListChanged( selection );
      }
    } );

    if( m_selection.length > 0 )
      m_channelListViewer.setSelection( new StructuredSelection( m_selection[0] ) );
    m_channelListViewer.setCheckedElements( m_selection );

    return treeGroup;
  }

  protected void handleChannelListChanged( final IStructuredSelection selection )
  {
    if( m_kmViewer == null )
      return;

    final Object firstElement = selection.getFirstElement();
    if( firstElement instanceof Feature )
    {
      final Feature feature = (Feature) firstElement;
      final String fid = feature.getId();
      KalininMiljukovType km = getForID( fid );
      // FIXME: if km created here, it never goes into the km-group (and will never be saved)
      if( km == null )
      {
        km = createKMForFeature( feature );
        // FIXME: put km into km-group
      }

      m_kmViewer.setInput( km );
    }
    else
      m_kmViewer.setInput( null );
  }

  void loadAs( final String path )
  {
    try
    {
      final Unmarshaller unmarshaller = JC.createUnmarshaller();
      final File file = new File( path );
      final Object object = unmarshaller.unmarshal( file );
      if( object instanceof JAXBElement )
      {
        final JAXBElement< ? > object2 = (JAXBElement< ? >) object;
        setKMGroup( (KalininMiljukovGroupType) object2.getValue() );
      }
    }
    catch( final Exception ex )
    {
      final String message = Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.12", path ); //$NON-NLS-1$
      MessageDialog.openError( getShell(), Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.11" ), message ); //$NON-NLS-1$
      createNewKMConfiguration();
    }
  }

  boolean saveAs( final String path )
  {
    if( path == null )
      return false;

    try
    {
      final File file = new File( path );
      final Marshaller marshaller = JaxbUtilities.createMarshaller( JC );
      final JAXBElement<KalininMiljukovGroupType> element = OF.createKalininMiljukovGroup( m_kmGroup );
      marshaller.marshal( element, file );
      return true;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final String msg = String.format( "Failed to write configuration file", e.getLocalizedMessage() );
      MessageDialog.openError( getShell(), getWizard().getWindowTitle(), msg );
      return false;
    }
  }

  // FIXME: mopve into own class!
  public boolean finish( )
  {
    // next line forces the dialog setting into the xml-binding objects
    final StringBuffer detailBuffer = new StringBuffer();
    final StringBuffer errorBuffer = new StringBuffer();
    final StringBuffer monitorBuffer = new StringBuffer();
    final StringBuffer selectionBuffer = m_kmViewer.getSelectionBuffer();
    final ILogger detailedLogger = new ILogger()
    {
      /**
       * @see org.kalypso.contribs.java.util.logging.ILogger#log(java.util.logging.Level, boolean, java.lang.String)
       */
      @Override
      public void log( final Level level, final int code, final String message )
      {
        detailBuffer.append( message );
      }
    };

    final ILogger errorLogger = new ILogger()
    {
      /**
       * @see org.kalypso.contribs.java.util.logging.ILogger#log(java.util.logging.Level, boolean, java.lang.String)
       */
      @Override
      public void log( final Level level, final int code, final String message )
      {
        errorBuffer.append( message );
      }
    };
    final ILogger monitorLogger = new ILogger()
    {
      /**
       * @see org.kalypso.contribs.java.util.logging.ILogger#log(java.util.logging.Level, boolean, java.lang.String)
       */
      @Override
      public void log( final Level level, final int code, final String message )
      {
        monitorBuffer.append( message );
      }

    };

    m_kmViewer.setInput( null );
    getDialogSettings().put( getResourceKey(), m_configPath );

    if( saveAs( m_configPath ) )
      monitorLogger.log( Level.INFO, LoggerUtilities.CODE_SHOW_DETAILS, Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.14" ) + m_configPath + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    else
    {
      errorLogger.log( Level.SEVERE, LoggerUtilities.CODE_SHOW_DETAILS, Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.16" ) ); //$NON-NLS-1$
      if( m_configPath == null )
        errorLogger.log( Level.SEVERE, LoggerUtilities.CODE_SHOW_DETAILS, Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.17" ) ); //$NON-NLS-1$
      else
        errorLogger.log( Level.SEVERE, LoggerUtilities.CODE_SHOW_DETAILS, Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.18" ) + m_configPath + ".\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    }

    final List<FeatureChange> changes = new ArrayList<FeatureChange>();
    final Object[] checkedElements = m_channelListViewer.getCheckedElements();
    boolean susccess = true;
    for( final Object object : checkedElements )
    {
      if( object instanceof KMChannel )
      {
        final KMChannel feature = (KMChannel) object;
        try
        {
          updateFeature( errorLogger, detailedLogger, feature, changes );
          monitorLogger.log( Level.SEVERE, LoggerUtilities.CODE_SHOW_DETAILS, Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.20" ) + m_kmUpdateLabelProvider.getText( feature ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
        }
        catch( final Exception e )
        {
          errorLogger.log( Level.SEVERE, LoggerUtilities.CODE_SHOW_DETAILS, Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.22" ) + m_kmUpdateLabelProvider.getText( feature ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
          e.printStackTrace();
          susccess = false;
        }
      }
    }
    if( !susccess )
    {
      MessageDialog.openError( getShell(), Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.24" ), errorBuffer.toString() ); //$NON-NLS-1$
      return false;
    }

    final FeatureChange[] change = changes.toArray( new FeatureChange[changes.size()] );
    final ChangeFeaturesCommand command = new ChangeFeaturesCommand( m_workspace, change );
    try
    {
      m_workspace.postCommand( command );
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    final String separator = "\n-------------------------------------\n"; //$NON-NLS-1$
    final String message = //
      Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.26" ) + monitorBuffer.toString()//  //$NON-NLS-1$
      + separator//
      + Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.27" ) + errorBuffer.toString() //  //$NON-NLS-1$
      + separator //
      + Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.28" ) + detailBuffer.toString() // //$NON-NLS-1$
      + separator //
      + Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.29" ) + selectionBuffer.toString(); //$NON-NLS-1$
    // MessageDialog.openInformation( getShell(), "Erfolg der Berechnungen", message );
    final Dialog dialog = new ScrolledTextInformationDialog( getShell(), Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.30" ), Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.31" ), message ); //$NON-NLS-1$ //$NON-NLS-2$
    dialog.open();
    // StatusUtilities..
    return true;
  }

  protected KalininMiljukovType getForID( final String fid )
  {
    if( m_kmGroup == null )
      return null;

    final List<KalininMiljukovType> kalininMiljukov = m_kmGroup.getKalininMiljukov();
    for( final Object element : kalininMiljukov )
    {
      final KalininMiljukovType km = (KalininMiljukovType) element;
      if( fid.equals( km.getId() ) )
        return km;
    }
    return null;
  }

  private List<FeatureChange> updateFeature( final ILogger errorLogger, final ILogger detailedLogger, final KMChannel kmChannel, final List<FeatureChange> changeList ) throws Exception
  {
    final String log = Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.32" ) + m_kmUpdateLabelProvider.getText( kmChannel ) + ":"; //$NON-NLS-1$ //$NON-NLS-2$
    final List<FeatureChange> result;
    if( changeList == null )
      result = new ArrayList<FeatureChange>();
    else
      result = changeList;

    final KalininMiljukovType km = getForID( kmChannel.getId() );
    final IFeatureType kmFT = m_workspace.getFeatureType( KMChannel.FEATURE_KM_CHANNEL );
    final IFeatureType kmPaFT = m_workspace.getFeatureType( KMParameter.FEATURE_KM_PARAMETER );

    final IPropertyType kmKMStartPT = kmFT.getProperty( NaModelConstants.KM_CHANNEL_KMSTART );
    final IPropertyType kmKMEndPT = kmFT.getProperty( NaModelConstants.KM_CHANNEL_KMEND );

    final IPropertyType qrkPT = kmPaFT.getProperty( NaModelConstants.KM_CHANNEL_QRK_PROP );
    final IPropertyType rkvT = kmPaFT.getProperty( NaModelConstants.KM_CHANNEL_RKV_PROP );
    final IPropertyType rnvPT = kmPaFT.getProperty( NaModelConstants.KM_CHANNEL_RNV_PROP );
    final IPropertyType cPT = kmPaFT.getProperty( NaModelConstants.KM_CHANNEL_C_PROP );

    if( km == null )
    {
      errorLogger.log( Level.SEVERE, LoggerUtilities.CODE_SHOW_DETAILS, log + Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.34" ) ); //$NON-NLS-1$
      return result;
    }
    final Double kmStart = km.getKmStart();
    final Double kmEnd = km.getKmEnd();
    if( kmStart == null || kmEnd == null )
    {
      errorLogger.log( Level.SEVERE, LoggerUtilities.CODE_SHOW_DETAILS, log + Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.35" ) ); //$NON-NLS-1$
      return result;
    }
    if( kmStart.compareTo( kmEnd ) > 0 )
    {
      errorLogger.log( Level.SEVERE, LoggerUtilities.CODE_SHOW_DETAILS, log + Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.36" ) ); //$NON-NLS-1$
      return result;
    }
    final List<File> list = new ArrayList<File>();
    final List<Profile> profiles = km.getProfile();
    for( final Profile profile : profiles )
    {
      if( profile.isEnabled() )
      {
        final String path = profile.getFile();
        final File file = new File( path );
        if( file.canRead() )
          list.add( file );
      }
    }
    if( list.isEmpty() )
    {
      errorLogger.log( Level.SEVERE, LoggerUtilities.CODE_SHOW_DETAILS, log + Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.37" ) ); //$NON-NLS-1$
      return result;
    }
    final File[] files = list.toArray( new File[list.size()] );

    final ProfileDataSet profileSet = ProfileFactory.createProfileSet( files, kmStart, kmEnd );
    // TODO:make it more general - not reduced to 5
    final int max = 5;
    final IKMValue[] values = profileSet.getKMValues();

    result.add( new FeatureChange( kmChannel, kmKMStartPT, km.getKmStart() ) );
    result.add( new FeatureChange( kmChannel, kmKMEndPT, km.getKmEnd() ) );

    final IFeatureBindingCollection<KMParameter> kmParameter = kmChannel.getParameters();
    if( kmParameter.size() < max )
    {
      errorLogger.log( Level.SEVERE, LoggerUtilities.CODE_SHOW_DETAILS, log + Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.38" ) ); //$NON-NLS-1$
      // TODO add new features
    }
    else
    {
      detailedLogger.log( Level.SEVERE, LoggerUtilities.CODE_SHOW_DETAILS, log + Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.39" ) ); //$NON-NLS-1$
      for( int i = 0; i < kmParameter.size(); i++ )
      {
        final Feature kmParameterFE = kmParameter.get( i );
        final IKMValue value = values[i];
        detailedLogger.log( Level.SEVERE, LoggerUtilities.CODE_SHOW_DETAILS, (i + 1) + ". " + value.toString() + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
        result.add( new FeatureChange( kmParameterFE, KMParameter.PROP_RKF, Double.parseDouble( FortranFormatHelper.printf( value.getK(), "f8.4" ) ) ) ); //$NON-NLS-1$
        result.add( new FeatureChange( kmParameterFE, rkvT, Double.parseDouble( FortranFormatHelper.printf( value.getKForeland(), "f8.4" ) ) ) ); //$NON-NLS-1$
        result.add( new FeatureChange( kmParameterFE, KMParameter.PROP_RNF, Double.parseDouble( FortranFormatHelper.printf( value.getN(), "f7.2" ) ) ) ); //$NON-NLS-1$
        result.add( new FeatureChange( kmParameterFE, rnvPT, Double.parseDouble( FortranFormatHelper.printf( value.getNForeland(), "f7.2" ) ) ) ); //$NON-NLS-1$
        result.add( new FeatureChange( kmParameterFE, qrkPT, Double.parseDouble( FortranFormatHelper.printf( value.getQSum(), "f8.3" ) ) ) ); //$NON-NLS-1$
        result.add( new FeatureChange( kmParameterFE, cPT, Double.parseDouble( FortranFormatHelper.printf( value.getAlpha(), "f8.3" ) ) ) ); //$NON-NLS-1$
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

  private String getResourceKey( )
  {
    final String base = "kalypsoRRM.kmUpdate.configPath"; //$NON-NLS-1$
    final URL context = m_workspace.getContext();
    if( context != null )
      return base + context.toString();
    else
      return base;
  }
}
