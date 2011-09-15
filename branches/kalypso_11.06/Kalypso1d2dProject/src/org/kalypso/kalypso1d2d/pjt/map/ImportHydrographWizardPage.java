package org.kalypso.kalypso1d2d.pjt.map;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.io.FilenameUtils;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateOpen;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup.FileChangedListener;
import org.kalypso.contribs.eclipse.ui.forms.MessageProvider;
import org.kalypso.kalypso1d2d.pjt.i18n.Messages;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.shape.FileMode;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.dbf.IDBFField;
import org.kalypso.transformation.ui.CRSSelectionPanel;
import org.kalypso.ui.ImageProvider;

public class ImportHydrographWizardPage extends WizardPage
{
//  protected static final String DATA_PROPERTY = "property"; //$NON-NLS-1$

//  private final Map<String, ComboViewer> m_propertyViewers = new HashMap<String, ComboViewer>();

//  private final String[] m_properties;

  private FileChooserGroup m_shapeChooser;

  private CRSSelectionPanel m_crsPanel;

  private String m_selectedCRS;

  public ImportHydrographWizardPage( final String pageName )
  {
    super( pageName );

    setImageDescriptor( ImageProvider.IMAGE_WIZBAN_IMPORT );
    setTitle( Messages.getString( "org.kalypso.gml.ui.commands.importshape.ImportShapeWizardPage.0" ) ); //$NON-NLS-1$

//    m_properties = properties;
  }

  /**
   * Creates the top level control for this dialog page under the given parent composite, then calls
   * <code>setControl</code> so that the created control can be accessed via <code>getControl</code>
   * 
   * @param parent
   *          the parent composite
   */
  @Override
  public void createControl( final Composite parent )
  {
    final Composite container = new Composite( parent, SWT.NULL );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 3;
    container.setLayout( gridLayout );
    setControl( container );

    final FileChooserDelegateOpen shapeChooser = new FileChooserDelegateOpen();
    shapeChooser.addFilter( Messages.getString( "org.kalypso.gml.ui.commands.importshape.ImportShapeWizardPage.1" ), "*.shp" ); //$NON-NLS-1$ //$NON-NLS-2$
    shapeChooser.addFilter( Messages.getString( "org.kalypso.gml.ui.commands.importshape.ImportShapeWizardPage.1" ), "*.txt" ); //$NON-NLS-1$ //$NON-NLS-2$
    shapeChooser.addFilter( Messages.getString( "org.kalypso.gml.ui.commands.importshape.ImportShapeWizardPage.1" ), "*.csv" ); //$NON-NLS-1$ //$NON-NLS-2$
    m_shapeChooser = new FileChooserGroup( shapeChooser );
    m_shapeChooser.setDialogSettings( getDialogSettings() );
    m_shapeChooser.setLabel( Messages.getString( "org.kalypso.gml.ui.commands.importshape.ImportShapeWizardPage.2" ) ); //$NON-NLS-1$
    m_shapeChooser.createControlsInGrid( container );
    m_shapeChooser.addFileChangedListener( new FileChangedListener()
    {
      @Override
      public void fileChanged( final File file )
      {
        handleShapeFileChanged( file );
      }
    } );

    new Label( container, SWT.NONE ).setText( Messages.getString( "org.kalypso.gml.ui.commands.importshape.ImportShapeWizardPage.3" ) ); //$NON-NLS-1$

    final Composite crsContainer = new Composite( container, SWT.NONE );
    crsContainer.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    crsContainer.setLayout( new FillLayout() );
    m_crsPanel = new CRSSelectionPanel( crsContainer, CRSSelectionPanel.NO_GROUP );
    m_crsPanel.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        validatePage();
      }
    } );

    setPageComplete( m_shapeChooser.getFile() != null );

    setControl( container );
  }

  protected void handleShapeFileChanged( final File file )
  {
    validatePage();

    // FIXME: introduce '<none selected>' entry; page is only complete, if all entries have been selected
/*
    final IDBFField[] fields;
    if( file != null && file.exists() )
    {
      fields = readFields( file );
    }
    else
    {
      fields = new IDBFField[0];
    }

    for( final ComboViewer viewer : m_propertyViewers.values() )
    {
      viewer.getControl().setData( DATA_PROPERTY, null );
      viewer.setInput( fields );
      viewer.getControl().setEnabled( fields != null );
      if( fields.length > 0 )
        viewer.setSelection( new StructuredSelection( fields[0] ) );
    }
    */
  }
/*
  private IDBFField[] readFields( final File file )
  {
    ShapeFile shape = null;
    try
    {
      if( file == null )
        return null;

      final String shapeBase = FilenameUtils.removeExtension( file.getAbsolutePath() );
      shape = new ShapeFile( shapeBase, ShapeSerializer.getShapeDefaultCharset(), FileMode.READ );
      final IDBFField[] fields = shape.getFields();
      shape.close();
      return fields;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final String msg = String.format( Messages.getString( "org.kalypso.gml.ui.commands.importshape.ImportShapeWizardPage.4" ), e.getLocalizedMessage() ); //$NON-NLS-1$
      setErrorMessage( msg );
      return null;
    }
    finally
    {
      try
      {
        if( shape != null )
          shape.close();
      }
      catch( final IOException ignored )
      {
        // ignored
      }
    }
  }
*/
  /**
   * Update the current page complete state based on the field content.
   */
  protected boolean validatePage( )
  {
    final IMessageProvider message = doValidate();
    boolean isValid = message == null;
    if( isValid )
      setMessage( null );
    else
      setMessage( message.getMessage(), message.getMessageType() );

    setPageComplete( isValid );
    return isValid;
  }

  private IMessageProvider doValidate( )
  {
    final IMessageProvider shapeMessage = m_shapeChooser.validate();
    if( shapeMessage != null )
      return shapeMessage;

    m_selectedCRS = m_crsPanel.getSelectedCRS();
    if( m_selectedCRS == null )
      return new MessageProvider( Messages.getString( "org.kalypso.gml.ui.commands.importshape.ImportShapeWizardPage.5" ), ERROR ); //$NON-NLS-1$

    return shapeMessage;
  }

  public File getShapeFile( )
  {
    return m_shapeChooser.getFile();
  }
/*
  public String getProperty( final String propertyName )
  {
    final ComboViewer comboViewer = m_propertyViewers.get( propertyName );
    if( comboViewer == null )
      return null;

    return (String) comboViewer.getControl().getData( DATA_PROPERTY );
  }
*/
  public String getSelectedCRS( )
  {
    return m_selectedCRS;
  }
}
